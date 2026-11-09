$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
& (Join-Path $PSScriptRoot 'autolith-runtime.ps1') --script (Join-Path $sourceRoot 'script\launcher.lisp') @args
exit $LASTEXITCODE
