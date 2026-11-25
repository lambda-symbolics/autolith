$AutolithSandboxBundleScript = Join-Path $PSScriptRoot 'cl-exec-sandbox-bundle.ps1'
if (-not [IO.File]::Exists($AutolithSandboxBundleScript)) {
  throw 'The cl-exec-sandbox bundle helper is missing; run script/bootstrap.ps1.'
}
. $AutolithSandboxBundleScript

function Assert-AutolithSandboxInstallRoot([string]$Root, [string]$Workspace) {
  if ($Root -notmatch '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)') {
    throw 'Sandbox install root must be absolute.'
  }
  $rootPath = [IO.Path]::GetFullPath($Root).TrimEnd('\')
  $workspacePath = [IO.Path]::GetFullPath($Workspace).TrimEnd('\')
  if ($rootPath.Equals($workspacePath, [StringComparison]::OrdinalIgnoreCase) -or
      $rootPath.StartsWith($workspacePath + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'Sandbox bundle must be installed outside the source workspace.'
  }
}

function Install-AutolithSandboxBundle([string]$Source, [string]$Root, [string]$Workspace) {
  Assert-AutolithSandboxInstallRoot $Root $Workspace
  Install-ClExecSandboxBundle $Source $Root
}

function Get-AutolithDirectoryName {
  # Mirrors AUTOLITH-SCRIPT-APPLICATION-DIRECTORY in script/roots.lisp.
  $name = $env:AUTOLITH_DIRECTORY_NAME
  if (-not $name) { return 'autolith' }
  if ($name -cnotmatch '^[A-Za-z0-9][A-Za-z0-9._-]*\z') {
    throw "AUTOLITH_DIRECTORY_NAME `"$name`" is not a portable directory name."
  }
  return $name
}

function Get-AutolithSandboxRoot {
  $absolute = '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)'
  $directory = Get-AutolithDirectoryName
  if ($env:XDG_DATA_HOME -and $env:XDG_DATA_HOME -match $absolute) {
    return (Join-Path $env:XDG_DATA_HOME "$directory\native\sandbox")
  }
  if (-not $env:LOCALAPPDATA -or $env:LOCALAPPDATA -notmatch $absolute) {
    throw 'LOCALAPPDATA must name an absolute directory.'
  }
  return (Join-Path $env:LOCALAPPDATA "$directory\data\native\sandbox")
}

function Set-AutolithSandboxDefault([string]$Root) {
  if (-not $env:CL_EXEC_SANDBOX_WINDOWS_HELPER) {
    $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = Get-ClExecSandboxSelection $Root
  }
}
