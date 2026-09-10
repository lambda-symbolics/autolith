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

# Qlot 1.8.4 names SB-POSIX:SYMLINK at read time, but Windows SBCL does not
# export it. Remove this compatibility patch when a newer locked Qlot guards
# that reference itself.
$qlotCache = Get-ChildItem -Path (Join-Path $env:USERPROFILE 'quicklisp\dists\quicklisp\software') `
  -Recurse -File -Filter 'cache.lisp' | Where-Object { $_.FullName -match '[\\/]qlot-[^\\/]+[\\/]src[\\/]cache\.lisp$' } | Select-Object -First 1
if (-not $qlotCache) { throw 'The installed Qlot cache source is unavailable.' }
$qlotSource = Get-Content -Raw -LiteralPath $qlotCache.FullName
$old = @'
        #+sbcl
        (sb-posix:symlink target-str link-str)
        #-sbcl
'@
$new = @'
        #+(and sbcl (not win32))
        (sb-posix:symlink target-str link-str)
        #+win32
        (copy-directory-tree target link)
        #-sbcl
'@
if ($qlotSource.Contains($old)) {
  Set-Content -NoNewline -LiteralPath $qlotCache.FullName -Value ($qlotSource.Replace($old, $new))
} elseif (-not $qlotSource.Contains('#+(and sbcl (not win32))')) {
  throw 'The installed Qlot source has an unexpected symlink implementation.'
}

& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
