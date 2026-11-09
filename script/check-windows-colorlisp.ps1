# Native packaging fixtures; run after script/bootstrap.ps1.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$root = Split-Path -Parent $PSScriptRoot
$runtime = Join-Path $root 'bin/autolith-runtime.ps1'
$probe = Join-Path $PSScriptRoot 'windows-colorlisp.lisp'
$shell = (Get-Process -Id $PID).Path
$temporaryRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\')
$temporary = Join-Path $temporaryRoot ('autolith-colorlisp-' + [guid]::NewGuid().ToString('N'))
$oldLibrary = $env:COLORLISP_NATIVE_LIBRARY
$oldCache = $env:XDG_CACHE_HOME
$checks = 0

function Assert-ColorLisp([bool]$Condition, [string]$Message) {
  if (-not $Condition) { throw $Message }
  $script:checks++
}

function Invoke-ColorLispProbe([string]$Action, [string]$Library) {
  # Expected failures write Lisp conditions to stderr.
  $previous = $ErrorActionPreference
  try {
    $ErrorActionPreference = 'Continue'
    $output = & $shell -NoProfile -ExecutionPolicy Bypass -File $runtime --script $probe $Action $Library 2>&1
    $status = $LASTEXITCODE
  } finally { $ErrorActionPreference = $previous }
  return [pscustomobject]@{ Status = $status; Output = ($output | Out-String) }
}

try {
  New-Item -ItemType Directory -Path $temporary | Out-Null
  $selected = Join-Path $temporary 'selected library.so'
  $result = Invoke-ColorLispProbe copy $selected
  Assert-ColorLisp ($result.Status -eq 0) "Initial library selection failed: $($result.Output)"

  $env:COLORLISP_NATIVE_LIBRARY = $selected
  $env:XDG_CACHE_HOME = Join-Path $temporary 'cache'
  $decoyDirectory = Join-Path $env:XDG_CACHE_HOME 'colorlisp/native/stale'
  New-Item -ItemType Directory -Force -Path $decoyDirectory | Out-Null
  [IO.File]::WriteAllBytes((Join-Path $decoyDirectory 'colorlisp.exp'), [byte[]]@(0x64,0x86,2,0))
  $packaged = Join-Path $temporary 'package/native/colorlisp.dll'
  $result = Invoke-ColorLispProbe copy $packaged
  Assert-ColorLisp ($result.Status -eq 0) "Selected library packaging failed: $($result.Output)"
  Assert-ColorLisp ((Get-FileHash $selected).Hash -eq (Get-FileHash $packaged).Hash) 'The selected library was not copied exactly.'
  $result = Invoke-ColorLispProbe validate $packaged
  Assert-ColorLisp ($result.Status -eq 0) "Packaged highlighting failed: $($result.Output)"

  $valid = [IO.File]::ReadAllBytes($packaged)
  $pe = [BitConverter]::ToInt32($valid, 0x3c)
  $cases = @(
    @{ Name = 'coff'; Bytes = [byte[]]@(0x64,0x86,2,0) },
    @{ Name = 'truncated'; Bytes = [byte[]]$valid[0..63] },
    @{ Name = 'wrong-machine'; Offset = $pe + 4; Value = 0 },
    @{ Name = 'bad-signature'; Offset = $pe; Value = 0 },
    @{ Name = 'not-dll'; Offset = $pe + 23; Value = ($valid[$pe + 23] -band 0xdf) },
    @{ Name = 'pe32'; Offset = $pe + 25; Value = 1 }
  )
  foreach ($case in $cases) {
    $invalid = Join-Path $temporary ($case.Name + '.dll')
    $bytes = if ($case.ContainsKey('Bytes')) { $case.Bytes } else { $valid.Clone() }
    if ($case.ContainsKey('Offset')) { $bytes[$case.Offset] = $case.Value }
    [IO.File]::WriteAllBytes($invalid, [byte[]]$bytes)
    $result = Invoke-ColorLispProbe validate $invalid
    Assert-ColorLisp ($result.Status -ne 0) "Accepted invalid $($case.Name) library."
    Assert-ColorLisp ($result.Output -match 'not a PE image|not an x64 PE32\+ DLL') "The loader was reached for $($case.Name): $($result.Output)"
  }
  $env:COLORLISP_NATIVE_LIBRARY = Join-Path $temporary 'coff.dll'
  $unpublished = Join-Path $temporary 'unpublished.dll'
  $result = Invoke-ColorLispProbe copy $unpublished
  Assert-ColorLisp ($result.Status -ne 0) 'An invalid selected library was accepted.'
  Assert-ColorLisp (-not (Test-Path -LiteralPath $unpublished)) 'An invalid selected library was published.'
  $unrelated = Join-Path $env:LOCALAPPDATA 'autolith/data/native/fff/fff_c.dll'
  $result = Invoke-ColorLispProbe validate $unrelated
  Assert-ColorLisp ($result.Status -ne 0) 'A valid PE DLL without the ColorLisp API was accepted.'
  Assert-ColorLisp ($result.Output -match 'colorlisp_session_') "The ColorLisp API was not exercised: $($result.Output)"
  Write-Output "Windows ColorLisp packaging: $checks checks passed."
} finally {
  $env:COLORLISP_NATIVE_LIBRARY = $oldLibrary
  $env:XDG_CACHE_HOME = $oldCache
  if ((Split-Path -Parent $temporary) -ne $temporaryRoot) { throw 'Unexpected fixture cleanup path.' }
  if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary -Recurse -Force }
}

# Expected native failures must not become the GitHub Actions step's exit code.
exit 0
