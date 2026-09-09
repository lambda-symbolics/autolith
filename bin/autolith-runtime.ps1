# Autolith runtime provisioning for Windows.
#
# Usage: autolith-runtime.ps1 [--install] --script PATH [ARGUMENT...]
#
# Selects an SBCL that satisfies sbcl.version in the same order as
# bin/autolith-runtime: AUTOLITH_SBCL, the recorded runtime command, the
# managed installation, then PATH. With --install and no compatible SBCL, it
# downloads the pinned official Windows binary, verifies it against
# sbcl-windows-releases.sha256, and unpacks it below the data root without
# registering it with Windows. Everything after that, including the matching
# source tree and the environment the entry points expect, is
# script/runtime.lisp's job.

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
$runtimeRelease = '2.6.6'
$archiveName = "sbcl-$runtimeRelease-x86-64-windows-binary.msi"
$archiveUrl = "https://downloads.sourceforge.net/project/sbcl/sbcl/$runtimeRelease/$archiveName"

function Fail([string]$message) {
  [Console]::Error.WriteLine("Autolith runtime setup failed: $message")
  exit 1
}

function Get-DataRoot {
  # Mirrors AUTOLITH-APPLICATION-ROOT for :data in script/roots.lisp.
  $xdg = $env:XDG_DATA_HOME
  if ($xdg -and ($xdg -match '^([A-Za-z]:[\\/]|\\\\)')) {
    return (Join-Path $xdg 'autolith')
  }
  if (-not $env:LOCALAPPDATA) { Fail 'LOCALAPPDATA is not set.' }
  return (Join-Path $env:LOCALAPPDATA 'autolith\data')
}

function Get-RuntimeVersion([string]$candidate) {
  try {
    $output = & $candidate --noinform --no-userinit --no-sysinit --non-interactive `
      --eval '(write-string (lisp-implementation-version))' 2>$null
    if ($LASTEXITCODE -ne 0) { return $null }
    $text = ($output | Out-String).Trim()
    if ($text -match '^\d+\.\d+\.\d+$') { return $text }
  } catch {}
  return $null
}

function Test-RuntimeCompatible([string]$candidate) {
  if (-not $candidate -or -not (Test-Path -LiteralPath $candidate -PathType Leaf)) { return $false }
  $version = Get-RuntimeVersion $candidate
  if (-not $version) { return $false }
  return ([version]$version -ge [version]$script:minimumVersion)
}

function Resolve-Command([string]$name) {
  $command = Get-Command $name -CommandType Application -ErrorAction SilentlyContinue
  if ($command) { return $command.Source }
  return $null
}

function Get-PinnedChecksum {
  $path = Join-Path $sourceRoot 'sbcl-windows-releases.sha256'
  if (-not (Test-Path -LiteralPath $path)) { Fail 'sbcl-windows-releases.sha256 is unavailable.' }
  foreach ($line in Get-Content -LiteralPath $path) {
    $fields = ($line.Trim() -split '\s+')
    if ($fields.Length -eq 2 -and $fields[1] -eq $archiveName -and $fields[0] -match '^[0-9a-fA-F]{64}$') {
      return $fields[0].ToLowerInvariant()
    }
  }
  Fail "sbcl-windows-releases.sha256 does not pin $archiveName."
}

function Install-Runtime {
  $expected = Get-PinnedChecksum
  $installRoot = Join-Path $runtimesRoot $runtimeRelease
  New-Item -ItemType Directory -Force -Path $installRoot | Out-Null
  $temporary = Join-Path $installRoot ".install.$PID"
  if (Test-Path -LiteralPath $temporary) { Remove-Item -Recurse -Force -LiteralPath $temporary }
  New-Item -ItemType Directory -Force -Path $temporary | Out-Null
  try {
    $archive = Join-Path $temporary $archiveName
    [Console]::Error.WriteLine("Installing pinned SBCL $runtimeRelease for Autolith.")
    & curl.exe --fail --location --show-error --retry 3 --progress-bar `
      --proto '=https' --tlsv1.2 --output $archive $archiveUrl
    if ($LASTEXITCODE -ne 0) { Fail 'the SBCL download failed.' }
    $actual = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($actual -ne $expected) { Fail 'the downloaded SBCL archive has the wrong SHA-256 identity.' }
    $extract = Join-Path $temporary 'extract'
    $process = Start-Process -FilePath 'msiexec.exe' -Wait -PassThru -NoNewWindow `
      -ArgumentList @('/a', "`"$archive`"", '/qn', "TARGETDIR=`"$extract`"")
    if ($process.ExitCode -ne 0) { Fail "msiexec could not unpack the SBCL archive (status $($process.ExitCode))." }
    $unpacked = Join-Path $extract 'PFiles\Steel Bank Common Lisp'
    if (-not (Test-Path -LiteralPath (Join-Path $unpacked 'sbcl.exe')) -or
        -not (Test-Path -LiteralPath (Join-Path $unpacked 'sbcl.core'))) {
      Fail 'the SBCL archive has an unexpected layout.'
    }
    if (-not (Test-RuntimeCompatible (Join-Path $unpacked 'sbcl.exe'))) {
      Fail 'the unpacked SBCL runtime failed its version probe.'
    }
    $stale = $null
    if (Test-Path -LiteralPath $managedPrefix) {
      $stale = Join-Path $installRoot "installation.stale.$PID"
      Move-Item -LiteralPath $managedPrefix -Destination $stale
    }
    try {
      Move-Item -LiteralPath $unpacked -Destination $managedPrefix
    } catch {
      if ($stale -and (Test-Path -LiteralPath $stale)) { Move-Item -LiteralPath $stale -Destination $managedPrefix }
      Fail 'the verified SBCL runtime could not be published.'
    }
    if ($stale -and (Test-Path -LiteralPath $stale)) { Remove-Item -Recurse -Force -LiteralPath $stale }
  } finally {
    if (Test-Path -LiteralPath $temporary) { Remove-Item -Recurse -Force -LiteralPath $temporary }
  }
}

function Add-OpenSslDirectory {
  # cl+ssl loads OpenSSL 3 by name. A source checkout takes the DLLs from
  # AUTOLITH_OPENSSL_DIRECTORY, a native\openssl directory beside the
  # sources, or Git for Windows, in that order.
  $candidates = @()
  if ($env:AUTOLITH_OPENSSL_DIRECTORY) { $candidates += $env:AUTOLITH_OPENSSL_DIRECTORY }
  $candidates += (Join-Path $sourceRoot 'native\openssl')
  $git = Resolve-Command 'git'
  if ($git) { $candidates += (Join-Path (Split-Path -Parent (Split-Path -Parent $git)) 'mingw64\bin') }
  foreach ($candidate in $candidates) {
    if ($candidate -and (Test-Path -LiteralPath (Join-Path $candidate 'libcrypto-3-x64.dll'))) {
      $env:PATH = "$candidate;" + $env:PATH
      return
    }
  }
}

$installRequested = $false
$scriptPath = $null
$scriptArguments = @()
for ($index = 0; $index -lt $args.Count; $index++) {
  switch ($args[$index]) {
    '--install' { $installRequested = $true }
    '--script' {
      if ($index + 1 -ge $args.Count) { Fail '--script needs a pathname.' }
      $scriptPath = $args[$index + 1]
      if ($index + 2 -lt $args.Count) { $scriptArguments = @($args[($index + 2)..($args.Count - 1)]) }
      $index = $args.Count
    }
    default { Fail "unknown argument $($args[$index])." }
  }
}
if (-not $scriptPath) { Fail 'no Lisp script was provided.' }
if (-not (Test-Path -LiteralPath $scriptPath -PathType Leaf)) { Fail "Lisp script $scriptPath does not exist." }

$minimumVersion = (Get-Content -LiteralPath (Join-Path $sourceRoot 'sbcl.version') -Raw).Trim()
if ($minimumVersion -notmatch '^\d+\.\d+\.\d+$') { Fail 'sbcl.version is malformed.' }
$runtimesRoot = Join-Path (Get-DataRoot) 'runtimes'
$runtimeCommandPath = Join-Path $runtimesRoot 'command'
$managedPrefix = Join-Path (Join-Path $runtimesRoot $runtimeRelease) 'installation'
$managedSbcl = Join-Path $managedPrefix 'sbcl.exe'

$sbclCommand = $null
if ($env:AUTOLITH_SBCL) {
  $resolved = $env:AUTOLITH_SBCL
  if (-not (Test-Path -LiteralPath $resolved -PathType Leaf)) { $resolved = Resolve-Command $env:AUTOLITH_SBCL }
  if (-not $resolved) { Fail 'AUTOLITH_SBCL does not name an executable.' }
  if (-not (Test-RuntimeCompatible $resolved)) { Fail "AUTOLITH_SBCL does not satisfy SBCL $minimumVersion or newer." }
  $sbclCommand = $resolved
} elseif (Test-Path -LiteralPath $runtimeCommandPath -PathType Leaf) {
  $recorded = (Get-Content -LiteralPath $runtimeCommandPath -TotalCount 1)
  if ($recorded) { $recorded = $recorded.Trim() }
  if ($recorded -and ($recorded -match '^([A-Za-z]:[\\/]|\\\\)') -and (Test-RuntimeCompatible $recorded)) {
    $sbclCommand = $recorded
  }
}
if (-not $sbclCommand -and (Test-RuntimeCompatible $managedSbcl)) { $sbclCommand = $managedSbcl }
if (-not $sbclCommand) {
  $pathSbcl = Resolve-Command 'sbcl'
  if ($pathSbcl -and (Test-RuntimeCompatible $pathSbcl)) { $sbclCommand = $pathSbcl }
}
if (-not $sbclCommand -and $installRequested) {
  Install-Runtime
  $sbclCommand = $managedSbcl
}
if (-not $sbclCommand) { Fail "SBCL $minimumVersion or newer is unavailable; run script\bootstrap.ps1." }

Add-OpenSslDirectory
$runtimeArguments = @()
if ($installRequested) { $runtimeArguments += '--install' }
$runtimeArguments += @('--script', $scriptPath) + $scriptArguments
& $sbclCommand --script (Join-Path $sourceRoot 'script\runtime.lisp') @runtimeArguments
exit $LASTEXITCODE
