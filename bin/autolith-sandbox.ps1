$AutolithSandboxHostModules = Join-Path $PSHOME 'Modules'
$env:PSModulePath = (@($AutolithSandboxHostModules) + @(
  $env:PSModulePath -split [IO.Path]::PathSeparator |
    Where-Object { $_ -and $_ -ne $AutolithSandboxHostModules }
)) -join [IO.Path]::PathSeparator
$AutolithSandboxFiles = @('cl-exec-sandbox-windows.exe', 'cl-exec-sandbox-compat64.dll', 'cl-exec-sandbox-compat32.dll', 'LICENSE-Detours.txt')

function Assert-AutolithSandboxPath([string]$Path) {
  # Reparse points in a private install would allow a later copy to escape it.
  $item = Get-Item -LiteralPath $Path -Force -ErrorAction Stop
  while ($null -ne $item) {
    if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw "Sandbox bundle path is a reparse point: $($item.FullName)" }
    $parent = [IO.Directory]::GetParent($item.FullName)
    $item = if ($null -ne $parent) { Get-Item -LiteralPath $parent.FullName -Force -ErrorAction Stop } else { $null }
  }
}

function Get-AutolithSandboxIdentity([string]$Directory) {
  Assert-AutolithSandboxPath $Directory
  $records = foreach ($name in $AutolithSandboxFiles) {
    $path = Join-Path $Directory $name
    $item = Get-Item -LiteralPath $path -Force -ErrorAction Stop
    if ($item.PSIsContainer -or $item.Length -eq 0 -or ($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
      throw "Invalid sandbox bundle member: $path"
    }
    # Direct .NET calls preserve native sharing errors on PowerShell 5.1 and 7.
    $sha = [Security.Cryptography.SHA256]::Create()
    try {
      $stream = [IO.File]::Open($path, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read)
      try { $hash = ([BitConverter]::ToString($sha.ComputeHash($stream))).Replace('-', '').ToLowerInvariant() }
      finally { $stream.Dispose() }
    } finally { $sha.Dispose() }
    "$name=$hash`n"
  }
  $sha = [Security.Cryptography.SHA256]::Create()
  try { return ([BitConverter]::ToString($sha.ComputeHash([Text.Encoding]::UTF8.GetBytes(($records -join ''))))).Replace('-', '').ToLowerInvariant() }
  finally { $sha.Dispose() }
}

function Get-AutolithSandboxSelection([string]$Root) {
  Assert-AutolithSandboxPath $Root
  $selector = Join-Path $Root 'selected'
  Assert-AutolithSandboxPath $selector
  $stream = [IO.File]::Open($selector, [IO.FileMode]::Open, [IO.FileAccess]::Read,
                           [IO.FileShare]::Read -bor [IO.FileShare]::Delete)
  try {
    $reader = [IO.StreamReader]::new($stream)
    try { $identity = $reader.ReadToEnd().Trim() }
    finally { $reader.Dispose() }
  } finally { $stream.Dispose() }
  if ($identity -cnotmatch '^[0-9a-f]{64}$') { throw 'Invalid sandbox bundle selector.' }
  $directory = Join-Path $Root $identity
  if ((Get-AutolithSandboxIdentity $directory) -cne $identity) { throw 'Selected sandbox bundle is incomplete or corrupt.' }
  return (Join-Path $directory 'cl-exec-sandbox-windows.exe')

}
function Get-AutolithSandboxExplicitSelection([string]$Path) {
  $item = Get-Item -LiteralPath $Path -Force -ErrorAction Stop
  Assert-AutolithSandboxPath $item.FullName
  if ($item.PSIsContainer -or $item.Name -cne 'cl-exec-sandbox-windows.exe' -or
      $item.Length -eq 0 -or ($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    throw "Invalid explicit sandbox helper: $Path"
  }
  $identity = Get-AutolithSandboxIdentity $item.DirectoryName
  if (-not $identity) { throw "Explicit sandbox helper bundle is incomplete: $Path" }
  return $item.FullName
}

function Set-AutolithSandboxPrivateAcl([string]$Directory) {
  # Only the installing account, SYSTEM, and administrators can modify this tree.
  Assert-AutolithSandboxPath $Directory
  if (-not ('AutolithSandboxAcl' -as [type])) {
    Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class AutolithSandboxAcl {
  [DllImport("advapi32.dll", EntryPoint="SetNamedSecurityInfoW", CharSet=CharSet.Unicode, ExactSpelling=true)]
  public static extern uint SetNamedSecurityInfo(string path, uint objectType, uint information,
                                                IntPtr owner, IntPtr group, IntPtr dacl, IntPtr sacl);
}
'@
  }
  $acl = [Security.AccessControl.DirectorySecurity]::new()
  $acl.SetAccessRuleProtection($true, $false)
  foreach ($sid in @([Security.Principal.WindowsIdentity]::GetCurrent().User,
                     [Security.Principal.SecurityIdentifier]::new('S-1-5-18'),
                     [Security.Principal.SecurityIdentifier]::new('S-1-5-32-544'))) {
    $acl.AddAccessRule([Security.AccessControl.FileSystemAccessRule]::new($sid, 'FullControl', 'ContainerInherit,ObjectInherit', 'None', 'Allow'))
  }
  $descriptor = [Security.AccessControl.RawSecurityDescriptor]::new($acl.GetSecurityDescriptorBinaryForm(), 0)
  $bytes = [byte[]]::new($descriptor.DiscretionaryAcl.BinaryLength)
  $descriptor.DiscretionaryAcl.GetBinaryForm($bytes, 0)
  $dacl = [Runtime.InteropServices.Marshal]::AllocHGlobal($bytes.Length)
  try {
    [Runtime.InteropServices.Marshal]::Copy($bytes, 0, $dacl, $bytes.Length)
    # SE_FILE_OBJECT, DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION.
    $errorCode = [AutolithSandboxAcl]::SetNamedSecurityInfo(
      $Directory, 1, [uint32]2147483652, [IntPtr]::Zero, [IntPtr]::Zero, $dacl, [IntPtr]::Zero)
    if ($errorCode -ne 0) { throw [ComponentModel.Win32Exception]::new([int]$errorCode) }
  } finally { [Runtime.InteropServices.Marshal]::FreeHGlobal($dacl) }
}

function Get-AutolithSandboxErrorCode([Exception]$Exception) {
  # PowerShell wraps static .NET calls in MethodInvocationException.
  while ($null -ne $Exception) {
    if ($Exception -is [ComponentModel.Win32Exception]) { return $Exception.NativeErrorCode }
    if (($Exception.HResult -band 0xffff0000L) -eq 0x80070000L) { return ($Exception.HResult -band 0xffff) }
    $Exception = $Exception.InnerException
  }
  return -1
}

function Invoke-AutolithSandboxSharingRetry([scriptblock]$Action, [int]$TimeoutMilliseconds = 5000) {
  # A monotonic budget also bounds repeated failures while validating a winner.
  $clock = [Diagnostics.Stopwatch]::StartNew()
  while ($true) {
    try { return (& $Action) }
    catch {
      $errorCode = Get-AutolithSandboxErrorCode $_.Exception
      if ($errorCode -notin @(5, 32, 33) -or $clock.ElapsedMilliseconds -ge $TimeoutMilliseconds) { throw }
      Start-Sleep -Milliseconds 25
    }
  }
}

function Publish-AutolithSandboxGeneration([string]$Stage, [string]$Generation, [string]$Identity) {
  Invoke-AutolithSandboxSharingRetry {
    if (-not [IO.Directory]::Exists($Generation)) {
      try { [IO.Directory]::Move($Stage, $Generation) }
      catch {
        $errorCode = Get-AutolithSandboxErrorCode $_.Exception
        # Reuse only an actual publication winner, never an unrelated move error.
        if ($errorCode -notin @(5, 32, 33, 80, 183) -or
            -not [IO.Directory]::Exists($Generation)) { throw }
      }
    }
    # A complete generation is renamed atomically. Never repair or overwrite it.
    if ((Get-AutolithSandboxIdentity $Generation) -cne $Identity) { throw 'Existing sandbox generation is corrupt.' }
  }
}

function Install-AutolithSandboxBundle([string]$Source, [string]$Root, [string]$Workspace) {
  if ($Root -notmatch '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)') {
    throw 'Sandbox install root must be absolute.'
  }
  $rootPath = [IO.Path]::GetFullPath($Root).TrimEnd('\')
  $workspacePath = [IO.Path]::GetFullPath($Workspace).TrimEnd('\')
  if ($rootPath.Equals($workspacePath, [StringComparison]::OrdinalIgnoreCase) -or
      $rootPath.StartsWith($workspacePath + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'Sandbox bundle must be installed outside the source workspace.'
  }
  # Validate existing ancestors before creating any directories.
  $ancestor = $rootPath
  while (-not (Test-Path -LiteralPath $ancestor)) { $ancestor = Split-Path -Parent $ancestor }
  Assert-AutolithSandboxPath $ancestor
  $identity = Get-AutolithSandboxIdentity $Source
  New-Item -ItemType Directory -Force -Path $rootPath | Out-Null
  Assert-AutolithSandboxPath $rootPath
  Set-AutolithSandboxPrivateAcl $rootPath
  $stage = Join-Path $rootPath ('.stage-' + [Guid]::NewGuid().ToString('N'))
  $selector = Join-Path $rootPath ('.select-' + [Guid]::NewGuid().ToString('N'))
  $generation = Join-Path $rootPath $identity
  $installationFailed = $false
  try {
    New-Item -ItemType Directory -Path $stage | Out-Null
    foreach ($name in $AutolithSandboxFiles) { Copy-Item -LiteralPath (Join-Path $Source $name) -Destination (Join-Path $stage $name) }
    $stagedIdentity = Invoke-AutolithSandboxSharingRetry { Get-AutolithSandboxIdentity $stage }
    if ($stagedIdentity -cne $identity) { throw 'Sandbox bundle changed while copying.' }
    Publish-AutolithSandboxGeneration $stage $generation $identity
    [IO.File]::WriteAllText($selector, $identity + "`n", [Text.Encoding]::ASCII)
    if (-not ('AutolithSandboxMove' -as [type])) {
      Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class AutolithSandboxMove {
  [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
  public static extern bool MoveFileEx(string source, string target, uint flags);
}
'@
    }
    # Same-directory MoveFileEx(REPLACE_EXISTING), as in platform-win32.lisp.
    $deadline = [DateTime]::UtcNow.AddSeconds(5)
    while (-not [AutolithSandboxMove]::MoveFileEx($selector, (Join-Path $rootPath 'selected'), 1)) {
      $errorCode = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
      # A replaced selector can still be delete-pending in another installer.
      if ($errorCode -notin @(5, 32, 33) -or [DateTime]::UtcNow -ge $deadline) {
        throw [ComponentModel.Win32Exception]::new($errorCode)
      }
      Start-Sleep -Milliseconds 25
    }
    return (Join-Path $generation 'cl-exec-sandbox-windows.exe')
  } catch {
    $installationFailed = $true
    throw
  } finally {
    # Delete only this invocation's private staging paths, never a generation.
    $cleanupError = $null
    foreach ($temporary in @($stage, $selector)) {
      try {
        Invoke-AutolithSandboxSharingRetry {
          if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary -Recurse -Force -ErrorAction Stop }
        }
      } catch {
        if ($null -eq $cleanupError) { $cleanupError = $_ }
        if ($installationFailed) { Write-Warning "Sandbox staging cleanup failed for ${temporary}: $($_.Exception.Message)" }
      }
    }
    if (-not $installationFailed -and $null -ne $cleanupError) { throw $cleanupError }
  }
}

function Get-AutolithSandboxRoot {
  $absolute = '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+)'
  if ($env:XDG_DATA_HOME -and $env:XDG_DATA_HOME -match $absolute) {
    return (Join-Path $env:XDG_DATA_HOME 'autolith\native\sandbox')
  }
  if (-not $env:LOCALAPPDATA -or $env:LOCALAPPDATA -notmatch $absolute) {
    throw 'LOCALAPPDATA must name an absolute directory.'
  }
  return (Join-Path $env:LOCALAPPDATA 'autolith\data\native\sandbox')
}

function Set-AutolithSandboxDefault([string]$Root) {
  if (-not $env:CL_EXEC_SANDBOX_WINDOWS_HELPER) {
    $env:CL_EXEC_SANDBOX_WINDOWS_HELPER = Get-AutolithSandboxSelection $Root
  }
}
