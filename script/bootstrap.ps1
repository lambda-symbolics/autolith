# Bootstrap a Windows source checkout the way script/bootstrap does.
#
# Installs the pinned SBCL when none is available, then runs
# script/bootstrap.lisp: locked dependencies, the private native libraries,
# the pristine recovery image, and the fast startup image.

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
& (Join-Path $sourceRoot 'bin\autolith-runtime.ps1') --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
