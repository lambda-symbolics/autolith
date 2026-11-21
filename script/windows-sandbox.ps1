param([Parameter(Mandatory=$true)][ValidateSet('BuildInstall','Install','Resolve','Validate')][string]$Action,
      [Parameter(Mandatory=$true)][string]$Root,
      [string]$Source,
      [string]$Workspace,
      [string]$Builder)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
[Console]::OutputEncoding = [Text.UTF8Encoding]::new($false)
. (Join-Path (Split-Path -Parent $PSScriptRoot) 'bin\autolith-sandbox.ps1')
if ($Action -eq 'BuildInstall' -or $Action -eq 'Install') {
  Assert-AutolithSandboxInstallRoot $Root $Workspace
}
if ($Action -eq 'BuildInstall') { BuildInstall-ClExecSandboxBundle $Builder $Root }
elseif ($Action -eq 'Install') { Install-ClExecSandboxBundle $Source $Root }
elseif ($Action -eq 'Validate') { Get-ClExecSandboxExplicitSelection $Source }
else { Get-ClExecSandboxSelection $Root }
