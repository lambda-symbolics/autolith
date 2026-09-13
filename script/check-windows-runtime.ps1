# Exercise the staged runtime, ASDF discovery, and core save/reload on Unicode paths.
param([Parameter(Mandatory = $true)][string]$RuntimeDirectory)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$runtime = Join-Path $RuntimeDirectory 'sbcl.exe'
$core = Join-Path $RuntimeDirectory 'unicode-test.core'
$previousHome = $env:SBCL_HOME
$previousCore = $env:AUTOLITH_RUNTIME_TEST_CORE
try {
  $env:SBCL_HOME = $RuntimeDirectory
  $env:AUTOLITH_RUNTIME_TEST_CORE = $core
  $save = @'
(progn
  (assert (= 65001 (sb-alien:alien-funcall
                   (sb-alien:extern-alien "GetACP"
                                          (function sb-alien:unsigned-int)))))
  (assert (probe-file sb-ext:*runtime-pathname*))
  (assert (probe-file sb-impl::*sbcl-homedir-pathname*))
  (sb-ext:save-lisp-and-die
   (uiop:getenv "AUTOLITH_RUNTIME_TEST_CORE")
   :executable nil
   :toplevel (lambda ()
               (assert (probe-file sb-ext:*core-pathname*))
               (assert (equal (uiop:parse-native-namestring
                               (uiop:getenv "AUTOLITH_RUNTIME_TEST_CORE"))
                              sb-ext:*core-pathname*))
               (format t "Unicode runtime and core round-trip passed.~%")
               (sb-ext:exit :code 0))))
'@
  & $runtime --noinform --non-interactive --eval '(require :asdf)' --eval $save
  if ($LASTEXITCODE -ne 0) { throw 'Unicode runtime initialization or core save failed.' }
  & $runtime --noinform --core $core --end-runtime-options
  if ($LASTEXITCODE -ne 0) { throw 'Unicode core reload failed.' }
} finally {
  $env:SBCL_HOME = $previousHome
  $env:AUTOLITH_RUNTIME_TEST_CORE = $previousCore
  if (Test-Path -LiteralPath $core) { Remove-Item -Force -LiteralPath $core }
}
