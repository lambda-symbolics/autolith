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

# Qlot 1.8.4 names SB-POSIX:SYMLINK and the FCNTL record-lock symbols inside
# #+sbcl forms. Windows SBCL exports neither, so the reader rejects its cache
# source there. The pinned checkout below gains Windows guards until an
# upstream release carries them: links become copies, the cache lock is a
# no-op, and cached files keep their write permission, since the read-only
# attribute would block later replacements. The checkout is forced back to
# the pin first, so rerunning the bootstrap patches pristine sources.
$qlotCommit = '0929984f5037891b4b0eba6ef2aa1f200d57a9fb'
$qlotRoot = Join-Path $env:USERPROFILE 'quicklisp\local-projects\qlot'
if (-not (Test-Path -LiteralPath (Join-Path $qlotRoot '.git'))) {
  if (Test-Path -LiteralPath $qlotRoot) { Remove-Item -Recurse -Force -LiteralPath $qlotRoot }
  & git -c core.autocrlf=false clone --quiet https://github.com/fukamachi/qlot.git $qlotRoot
  if ($LASTEXITCODE -ne 0) { throw 'Qlot checkout failed.' }
}
& git -C $qlotRoot cat-file -e "$qlotCommit^{commit}"
if ($LASTEXITCODE -ne 0) {
  & git -C $qlotRoot fetch --quiet origin $qlotCommit
  if ($LASTEXITCODE -ne 0) { throw 'Pinned Qlot fetch failed.' }
}
& git -C $qlotRoot checkout --quiet --force --detach $qlotCommit
if ($LASTEXITCODE -ne 0) { throw 'Pinned Qlot checkout failed.' }

function Edit-QlotSource([string]$relative, [object[]]$replacements) {
  # Apply exact-text REPLACEMENTS to the pinned Qlot source RELATIVE, failing
  # when a form is missing, so a changed upstream file is noticed at once.
  $pathname = Join-Path $qlotRoot $relative
  $source = [IO.File]::ReadAllText($pathname).Replace("`r`n", "`n")
  foreach ($replacement in $replacements) {
    if (-not $source.Contains($replacement.Old)) {
      throw "The pinned Qlot source $relative lacks the form for the $($replacement.Name) guard."
    }
    $source = $source.Replace($replacement.Old, $replacement.New)
  }
  [IO.File]::WriteAllText($pathname, $source, [Text.UTF8Encoding]::new($false))
}

Edit-QlotSource 'src\cache.lisp' @(
  @{ Name = 'symlink'
     Old = "        #+sbcl`n        (sb-posix:symlink target-str link-str)`n        #-sbcl`n"
     New = "        #+(and sbcl (not win32))`n        (sb-posix:symlink target-str link-str)`n        #+(and sbcl win32)`n        (copy-directory-tree target link)`n        #-sbcl`n" },
  @{ Name = 'read-only'
     Old = "(defun make-directory-read-only (path)`n  #+sbcl`n"
     New = "(defun make-directory-read-only (path)`n  #+(and sbcl (not win32))`n" },
  @{ Name = 'acquire-lock'
     Old = "(defun acquire-lock (stream mode)`n  #+sbcl`n"
     New = "(defun acquire-lock (stream mode)`n  #+(and sbcl (not win32))`n" },
  @{ Name = 'acquire-lock fallback'
     Old = "  #-(or sbcl ccl ecl)`n  (declare (ignore stream mode)))"
     New = "  #-(or (and sbcl (not win32)) ccl ecl)`n  (declare (ignore stream mode)))" },
  @{ Name = 'release-lock'
     Old = "(defun release-lock (stream)`n  #+sbcl`n"
     New = "(defun release-lock (stream)`n  #+(and sbcl (not win32))`n" },
  @{ Name = 'release-lock fallback'
     Old = "  #-(or sbcl ccl ecl)`n  (declare (ignore stream)))"
     New = "  #-(or (and sbcl (not win32)) ccl ecl)`n  (declare (ignore stream)))" }
)

& $runtime --install --script (Join-Path $sourceRoot 'script\bootstrap.lisp') @args
exit $LASTEXITCODE
