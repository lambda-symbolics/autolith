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
  & (Join-Path $Release 'bin\autolith.ps1') --version >> $log 2>&1
  if ($LASTEXITCODE -ne 0) { throw 'Standard-user packaged launcher failed.' }
  $env:AUTOLITH_SBCL = Join-Path $Release 'runtime\sbcl.exe'
  $env:SBCL_HOME = Join-Path $Release 'runtime'
  $env:AUTOLITH_SBCL_SOURCE_ROOT = Join-Path $Release 'libexec\sbcl-source'
  $env:AUTOLITH_FFF_LIBRARY = Join-Path $Release 'native\fff_c.dll'
  $env:COLORLISP_NATIVE_LIBRARY = Join-Path $Release 'native\colorlisp.dll'
  $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = Join-Path $Release 'native\cl-exec-sandbox-windows.exe'
  $env:PATH = "$(Join-Path $Release 'native');$env:PATH"
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
