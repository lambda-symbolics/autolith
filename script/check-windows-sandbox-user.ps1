param([Parameter(Mandatory=$true)][string]$Release,
      [Parameter(Mandatory=$true)][string]$Root)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$log = Join-Path $Root 'sandbox.log'
try {
  $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
  $principal = [Security.Principal.WindowsPrincipal]::new($identity)
  if ($principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) { throw 'Sandbox test account is elevated.' }
  $env:TEMP = Join-Path $Root 'temp'
  $env:TMP = $env:TEMP
  $env:LOCALAPPDATA = Join-Path $Root 'AppData\Local'
  $env:APPDATA = Join-Path $Root 'AppData\Roaming'
  $env:HOME = $Root
  $env:USERPROFILE = $Root
  New-Item -ItemType Directory -Force -Path $env:TEMP,$env:LOCALAPPDATA,$env:APPDATA | Out-Null
  $env:AUTOLITH_SBCL = Join-Path $Release 'runtime\sbcl.exe'
  $env:SBCL_HOME = Join-Path $Release 'runtime'
  $env:AUTOLITH_SBCL_SOURCE_ROOT = Join-Path $Release 'libexec\sbcl-source'
  $env:AUTOLITH_FFF_LIBRARY = Join-Path $Release 'native\fff_c.dll'
  $env:COLORLISP_NATIVE_LIBRARY = Join-Path $Release 'native\colorlisp.dll'
  . (Join-Path $Release 'libexec\autolith\bin\autolith-sandbox.ps1')
  Set-AutolithSandboxDefault (Join-Path $Release 'native\sandbox')
  # Exercise Autolith's root authority and environment selection from the package.
  $powershell = if ($env:POWERSHELL) { $env:POWERSHELL } else { 'powershell.exe' }
  $bundleGate = Join-Path $Release 'libexec\autolith\script\check-windows-sandbox-bundle.ps1'
  $bundleGateRoot = Join-Path $Root 'packaged-bundle-gate'
  & $powershell -NoProfile -ExecutionPolicy Bypass -File $bundleGate -Root $bundleGateRoot >> $log 2>&1
  if ($LASTEXITCODE -ne 0) { throw 'Packaged sandbox root and environment gate failed.' }
  # Exercise a fresh packaged launch with Git but without native compilers in PATH.
  $env:AUTOLITH_TEST_GIT = (Get-Command git.exe -CommandType Application -ErrorAction Stop | Select-Object -First 1).Source
  $gitDirectory = Split-Path -Parent $env:AUTOLITH_TEST_GIT
  $systemRoot = [Environment]::GetEnvironmentVariable('SystemRoot')
  $windowsPowerShell = Join-Path $systemRoot 'System32\WindowsPowerShell\v1.0'
  $env:PATH = "$gitDirectory;$windowsPowerShell;$(Join-Path $Release 'native');$(Join-Path $systemRoot 'System32');$systemRoot"
  Get-Command powershell.exe -CommandType Application -ErrorAction Stop | Out-Null
  & git.exe --version >> $log 2>&1
  if ($LASTEXITCODE -ne 0) { throw 'Git is unavailable in the compiler-free test environment.' }
  & (Join-Path $Release 'bin\autolith.ps1') --version >> $log 2>&1
  if ($LASTEXITCODE -ne 0) { throw 'Standard-user packaged launcher failed without a toolchain.' }
  $source = Join-Path $Release 'libexec\autolith'
  Set-Location $source
  & (Join-Path $source 'script\check.ps1') --suite windows-sandbox --jobs 1 --timeout 120 >> $log 2>&1
  if ($LASTEXITCODE -ne 0) { throw 'Packaged shell.run containment failed.' }
  'Packaged Autolith shell.run sandbox passed under a standard Windows account.' >> $log
} catch {
  $_ | Out-String | Add-Content -LiteralPath $log
  exit 1
}
exit 0
