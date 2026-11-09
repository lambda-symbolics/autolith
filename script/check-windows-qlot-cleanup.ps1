# Run in Windows PowerShell 5.1 or PowerShell 7 without bootstrapping Lisp.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$cleanup = Join-Path $PSScriptRoot 'qlot-delete-directory.ps1'
$temporary = Join-Path ([IO.Path]::GetTempPath()) ('autolith-qlot-' + [guid]::NewGuid().ToString('N'))
$checks = 0

function Assert-QlotCleanup([bool]$Condition, [string]$Message) {
  if (-not $Condition) { throw $Message }
  $script:checks++
}

try {
  $tree = Join-Path $temporary ('checkout ' + [char]0x17e)
  $outside = Join-Path $temporary 'outside'
  $dotTree = Join-Path $tree '.git/objects'
  [IO.Directory]::CreateDirectory($dotTree) | Out-Null
  [IO.Directory]::CreateDirectory($outside) | Out-Null
  $literal = Join-Path $tree '[literal]'
  [IO.Directory]::CreateDirectory($literal) | Out-Null
  [IO.File]::WriteAllText((Join-Path $literal 'data'), 'literal entry')
  $locked = Join-Path $dotTree 'read-only'
  [IO.File]::WriteAllText($locked, 'git object')
  [IO.File]::SetAttributes($locked, [IO.FileAttributes]::ReadOnly -bor [IO.FileAttributes]::Hidden)
  $sentinel = Join-Path $outside 'sentinel'
  [IO.File]::WriteAllText($sentinel, 'outside target')
  New-Item -ItemType SymbolicLink -Path (Join-Path $tree 'circular-link') -Target $tree | Out-Null
  New-Item -ItemType Junction -Path (Join-Path $tree 'outside-directory') -Target $outside | Out-Null
  New-Item -ItemType SymbolicLink -Path (Join-Path $tree 'outside-file') -Target $sentinel | Out-Null
  $missing = Join-Path $temporary 'missing-target'
  [IO.File]::WriteAllText($missing, 'temporary target')
  New-Item -ItemType SymbolicLink -Path (Join-Path $tree 'dangling-link') -Target $missing | Out-Null
  [IO.File]::Delete($missing)

  & $cleanup -Path $tree
  Assert-QlotCleanup (-not [IO.Directory]::Exists($tree)) 'Cleanup did not remove the checkout.'
  Assert-QlotCleanup ([IO.File]::ReadAllText($sentinel) -eq 'outside target') 'Cleanup followed an external link.'
  & $cleanup -Path $tree
  Assert-QlotCleanup (-not [IO.Directory]::Exists($tree)) 'Missing checkout cleanup failed.'

  [IO.Directory]::CreateDirectory($tree) | Out-Null
  $busy = Join-Path $tree 'busy'
  $handle = [IO.File]::Open($busy, [IO.FileMode]::Create, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
  $failed = $false
  try {
    try { & $cleanup -Path $tree } catch { $failed = $true }
    Assert-QlotCleanup $failed 'A sharing violation must propagate to the caller.'
  } finally {
    $handle.Dispose()
  }
  & $cleanup -Path $tree
  Assert-QlotCleanup (-not [IO.Directory]::Exists($tree)) 'Cleanup did not recover after the handle closed.'

  $failed = $false
  try { & $cleanup -Path ([IO.Path]::GetPathRoot($temporary)) } catch { $failed = $true }
  Assert-QlotCleanup $failed 'Filesystem root cleanup must be refused.'
  Write-Host "Qlot cleanup: $checks checks passed."
} finally {
  & $cleanup -Path $temporary
}
