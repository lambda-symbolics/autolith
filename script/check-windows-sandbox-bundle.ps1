param([Parameter(Mandatory=$true)][string]$Root)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. (Join-Path (Split-Path -Parent $PSScriptRoot) 'bin\autolith-sandbox.ps1')
function Assert([bool]$Value, [string]$Message) { if (-not $Value) { throw $Message } }
function Assert-Failure([scriptblock]$Action) {
  $failed = $false
  try { & $Action | Out-Null } catch { $failed = $true }
  Assert $failed 'Expected bundle validation failure.'
}
$source = Join-Path $Root 'workspace\build'
$private = Join-Path $Root 'private\sandbox'
New-Item -ItemType Directory -Force -Path $source,$private | Out-Null
foreach ($name in $ClExecSandboxFiles) { [IO.File]::WriteAllText((Join-Path $source $name), "fixture-$name") }
$identity = Get-ClExecSandboxIdentity $source
$generation = Join-Path $private $identity
New-Item -ItemType Directory -Path $generation | Out-Null
foreach ($name in $ClExecSandboxFiles) { Copy-Item -LiteralPath (Join-Path $source $name) -Destination $generation }
[IO.File]::WriteAllText((Join-Path $private 'selected'), $identity)
$helper = Join-Path $generation 'cl-exec-sandbox-windows.exe'
$saved = $env:CL_EXEC_SANDBOX_WINDOWS_HELPER
try {
  $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = 'explicit-user-helper.exe'
  Set-AutolithSandboxDefault $private
  Assert ($env:CL_EXEC_SANDBOX_WINDOWS_HELPER -eq 'explicit-user-helper.exe') 'Explicit override was replaced.'
  $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = ''
  Set-AutolithSandboxDefault $private
  Assert ($env:CL_EXEC_SANDBOX_WINDOWS_HELPER -eq $helper) 'Empty override must resolve the selected bundle.'
} finally { $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = $saved }
if ([Environment]::OSVersion.Platform -eq [PlatformID]::Win32NT) {
  $workspace = Join-Path $Root 'workspace'
  Assert-Failure { Install-AutolithSandboxBundle $generation (Join-Path $workspace 'native') $workspace }
  Assert-Failure { Install-AutolithSandboxBundle $generation 'C:relative' $workspace }
  Assert-Failure { Assert-AutolithSandboxInstallRoot $workspace $workspace }
  Assert-AutolithSandboxInstallRoot (Join-Path $Root 'workspace-sibling') $workspace
  $savedData = $env:XDG_DATA_HOME
  $savedLocal = $env:LOCALAPPDATA
  try {
    $env:XDG_DATA_HOME = Join-Path $Root 'data'
    $env:LOCALAPPDATA = Join-Path $Root 'local'
    Assert ((Get-AutolithSandboxRoot) -eq (Join-Path $env:XDG_DATA_HOME 'autolith\native\sandbox')) 'Absolute XDG data root was ignored.'
    $env:XDG_DATA_HOME = 'relative-data'
    Assert ((Get-AutolithSandboxRoot) -eq (Join-Path $env:LOCALAPPDATA 'autolith\data\native\sandbox')) 'Relative XDG data root did not fall back to LOCALAPPDATA.'
    $env:LOCALAPPDATA = 'relative-local'
    Assert-Failure { Get-AutolithSandboxRoot }
  } finally {
    $env:XDG_DATA_HOME = $savedData
    $env:LOCALAPPDATA = $savedLocal
  }
}
'Autolith sandbox root and environment checks passed.'
exit 0
