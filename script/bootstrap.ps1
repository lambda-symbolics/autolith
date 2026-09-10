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
$qlotCache = Join-Path $qlotRoot 'src\cache.lisp'
$qlotSource = Get-Content -Raw -LiteralPath $qlotCache
$old = "        #+sbcl`n        (sb-posix:symlink target-str link-str)`n        #-sbcl"
$new = "        #+(and sbcl (not win32))`n        (sb-posix:symlink target-str link-str)`n        #+win32`n        (copy-directory-tree target link)`n        #-sbcl"
if ($qlotSource.Contains($old)) {
  Set-Content -NoNewline -LiteralPath $qlotCache -Value ($qlotSource.Replace($old, $new))
} elseif (-not $qlotSource.Contains('#+(and sbcl (not win32))')) {
  throw 'The pinned Qlot source has an unexpected symlink implementation.'
}

& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
