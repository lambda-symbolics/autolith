# Bootstrap a Windows source checkout.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$sourceRoot = Split-Path -Parent $PSScriptRoot
$runtime = Join-Path $sourceRoot 'bin\autolith-runtime.ps1'
$quicklispSetup = Join-Path $env:USERPROFILE 'quicklisp\setup.lisp'
if (-not (Test-Path -LiteralPath $quicklispSetup)) {
  $installer = Join-Path $env:TEMP "autolith-quicklisp-$PID.lisp"
  try {
    Invoke-WebRequest -UseBasicParsing -Uri 'https://beta.quicklisp.org/quicklisp.lisp' -OutFile $installer
    & $runtime --install --script (Join-Path $sourceRoot 'script\ensure-quicklisp.lisp') $installer
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  } finally {
    if (Test-Path -LiteralPath $installer) { Remove-Item -Force -LiteralPath $installer }
  }
}
& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
