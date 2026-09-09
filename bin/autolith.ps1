# The stable Windows launcher, the counterpart of bin/autolith.
#
# bin/autolith-runtime.ps1 selects the runtime and script/launcher.lisp does
# what the Bash launcher does: it runs the fast startup image or the source
# loader, offers a bootstrap when the image is missing, restores the console,
# and enters pristine recovery after a crash.

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
& (Join-Path $PSScriptRoot 'autolith-runtime.ps1') --script (Join-Path $sourceRoot 'script\launcher.lisp') @args
exit $LASTEXITCODE
