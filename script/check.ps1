# Run script/check.lisp the way script/check does, under the checked runtime.
#
# Usage: check.ps1 [--list] [--suite NAME]... [--test NAME]... [--jobs N] [--timeout SECONDS]

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
$constantPattern = '^\s*\(\s*([^\s()]+:)?(define-constant|defconstant)(\s|\(|\)|$)'
$roots = @('src', 'recovery', 'server', 'script', 'tests') |
  ForEach-Object { Join-Path $sourceRoot $_ } |
  Where-Object { Test-Path -LiteralPath $_ }
$files = @(Get-Item -LiteralPath (Join-Path $sourceRoot 'autolith.asd')) +
  @(Get-ChildItem -LiteralPath $roots -Recurse -File -Filter '*.lisp')
$matches = $files | Select-String -Pattern $constantPattern
if ($matches) {
  [Console]::Error.WriteLine('Autolith source must not declare constants:')
  foreach ($match in $matches) {
    [Console]::Error.WriteLine("$($match.Path):$($match.LineNumber):$($match.Line)")
  }
  exit 1
}

& (Join-Path $sourceRoot 'bin\autolith-runtime.ps1') --script (Join-Path $sourceRoot 'script\check.lisp') @args
exit $LASTEXITCODE
