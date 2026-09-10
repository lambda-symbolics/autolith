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

$qlotRoot = Join-Path $env:USERPROFILE 'quicklisp\local-projects\qlot'
if (-not (Test-Path -LiteralPath (Join-Path $qlotRoot '.git'))) {
  if (Test-Path -LiteralPath $qlotRoot) { Remove-Item -Recurse -Force -LiteralPath $qlotRoot }
  & git clone --quiet https://github.com/fukamachi/qlot.git $qlotRoot
  if ($LASTEXITCODE -ne 0) { throw 'Qlot checkout failed.' }
}
& git -C $qlotRoot fetch --quiet origin 0929984f5037891b4b0eba6ef2aa1f200d57a9fb
& git -C $qlotRoot checkout --quiet --detach 0929984f5037891b4b0eba6ef2aa1f200d57a9fb
if ($LASTEXITCODE -ne 0) { throw 'Pinned Qlot checkout failed.' }
foreach ($relative in 'src\cache.lisp','src\main.lisp','src\install.lisp') {
  $pathname = Join-Path $qlotRoot $relative
  $source = Get-Content -Raw -LiteralPath $pathname
  $patched = $source.Replace('#+sbcl', '#+(and sbcl (not win32))').Replace('#-sbcl', '#-(or sbcl win32)')
  if ($patched -eq $source -and -not $source.Contains('#+(and sbcl (not win32))')) {
    throw "The pinned Qlot source $relative has no SBCL host guards."
  }
  Set-Content -NoNewline -LiteralPath $pathname -Value $patched
}

& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
