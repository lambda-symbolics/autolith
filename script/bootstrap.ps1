# Bootstrap a Windows source checkout.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$sourceRoot = Split-Path -Parent $PSScriptRoot
$runtime = Join-Path $sourceRoot 'bin\autolith-runtime.ps1'
& $runtime --install --script (Join-Path $sourceRoot 'script\ensure-quicklisp.lisp')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
