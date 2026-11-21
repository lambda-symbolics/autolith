# Invoke the locked upstream native fixtures after windows-bootstrap.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$root = Split-Path -Parent $PSScriptRoot
$setup = Join-Path $root '.qlot/setup.lisp'
if (-not (Test-Path -LiteralPath $setup -PathType Leaf)) { throw 'Missing project Qlot setup.' }
$lock = Get-Content -Raw -LiteralPath (Join-Path $root 'qlfile.lock')
$pattern = '(?s)\("colorlisp"\s+\.\s+\(:class\s+qlot/source/git:source-git\s+:initargs\s+\(:remote-url\s+"https://github.com/lambda-symbolics/colorlisp\.git"\s+:ref\s+"([0-9a-f]{40})"\)\s+:version\s+"git-\1"\)\)'
$entries = [regex]::Matches($lock, $pattern)
if ($entries.Count -ne 1) { throw 'Expected one immutable ColorLisp Git lock entry.' }
$ref = $entries[0].Groups[1].Value
$temporary = Join-Path ([IO.Path]::GetTempPath()) ('autolith-colorlisp-upstream-' + [guid]::NewGuid().ToString('N'))
$names = @('AUTOLITH_COLORLISP_CHECK_ROOT', 'AUTOLITH_COLORLISP_CHECK_REF', 'AUTOLITH_COLORLISP_CHECK_METADATA')
$previous = @{}
foreach ($name in $names) { $previous[$name] = [Environment]::GetEnvironmentVariable($name, 'Process') }
try {
  New-Item -ItemType Directory -Path $temporary | Out-Null
  $resolver = Join-Path $temporary 'resolve.lisp'
  $metadata = Join-Path $temporary 'metadata.txt'
  $env:AUTOLITH_COLORLISP_CHECK_ROOT = $root
  $env:AUTOLITH_COLORLISP_CHECK_REF = $ref
  $env:AUTOLITH_COLORLISP_CHECK_METADATA = $metadata
  @'
(require :asdf)
(setf asdf:*central-registry* nil)
(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))
(load (merge-pathnames ".qlot/setup.lisp"
                       (uiop:ensure-directory-pathname
                        (uiop:getenv "AUTOLITH_COLORLISP_CHECK_ROOT"))))
(let* ((ref (uiop:getenv "AUTOLITH_COLORLISP_CHECK_REF"))
       (version (concatenate 'string "git-" ref))
       (dist (ql-dist:find-dist "colorlisp"))
       (system (ql-dist:find-system "colorlisp"))
       (owner (and system (ql-dist:dist system))))
  ;; Quicklisp may return distinct objects for the same distribution.
  (unless (and dist owner
               (string= (ql-dist:name owner) "colorlisp")
               (string= (ql-dist:version dist) version)
               (string= (ql-dist:version owner) version))
    (error "Installed ColorLisp distribution does not match qlfile.lock."))
  (let ((asd (ql-dist:find-asdf-system-file "colorlisp")))
    (unless (and asd (probe-file asd))
      (error "The locked ColorLisp source is not installed."))
    (asdf:load-asd asd)
    (unless (equal (probe-file (asdf:system-source-file (asdf:find-system "colorlisp")))
                   (probe-file asd))
      (error "ASDF selected another ColorLisp source."))
    (unless (and (member :win32 *features*) (member :x86-64 *features*)
                 (equal (lisp-implementation-type) "SBCL"))
      (error "The upstream Windows fixture requires native x64 SBCL."))
    (with-open-file (stream (uiop:getenv "AUTOLITH_COLORLISP_CHECK_METADATA")
                            :direction :output :if-exists :error
                            :external-format :utf-8)
      (format stream "~A~%~A~%~A~%"
              (uiop:native-namestring sb-ext:*runtime-pathname*)
              (uiop:native-namestring (uiop:pathname-directory-pathname asd))
              (lisp-implementation-version)))))
'@ | Set-Content -LiteralPath $resolver -Encoding UTF8
  $shell = (Get-Process -Id $PID).Path
  & $shell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $root 'bin/autolith-runtime.ps1') --script $resolver
  if ($LASTEXITCODE -ne 0) { throw 'Could not resolve the locked native fixture environment.' }
  $resolved = @(Get-Content -LiteralPath $metadata -Encoding UTF8)
  if ($resolved.Count -ne 3) { throw 'Invalid native fixture metadata.' }
  $sbcl = $resolved[0]
  $source = $resolved[1]
  if (-not (Test-Path -LiteralPath $sbcl -PathType Leaf)) { throw 'Resolved SBCL executable is missing.' }
  if ($resolved[2] -ne (Get-Content -Raw -LiteralPath (Join-Path $root 'sbcl.version')).Trim()) {
    throw 'Resolved SBCL differs from the candidate runtime version.'
  }
  $fixture = Join-Path $source 'script/check-windows-native.ps1'
  if (-not (Test-Path -LiteralPath $fixture -PathType Leaf)) { throw 'The locked source has no upstream Windows fixture.' }
  # Refuse an older fixture rather than silently running its global Quicklisp fallback.
  $command = Get-Command -Name $fixture
  if (-not $command.Parameters.ContainsKey('SetupPath')) { throw 'The locked fixture lacks explicit SetupPath support.' }
  Get-Command clang -CommandType Application -ErrorAction Stop | Out-Null
  Write-Output "ColorLisp upstream: lock=$ref source=$source SBCL=$($resolved[2]) executable=$sbcl"
  # Use another PowerShell process because the owning fixture terminates with exit.
  & $shell -NoProfile -ExecutionPolicy Bypass -File $fixture -Sbcl $sbcl -SetupPath $setup
  if ($LASTEXITCODE -ne 0) { throw 'The upstream Windows ColorLisp native fixture failed.' }
} finally {
  foreach ($name in $names) { [Environment]::SetEnvironmentVariable($name, $previous[$name], 'Process') }
  if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary -Recurse -Force }
}
exit 0
