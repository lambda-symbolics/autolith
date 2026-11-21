param([string]$OutputDirectory='dist',[Parameter(Mandatory=$true)][string]$Tag)
$ErrorActionPreference='Stop'; Set-StrictMode -Version Latest
if ($Tag -notmatch '^v\d+\.\d+\.\d+(?:-dev\.\d+)?$') { throw 'Tag is malformed.' }
$root=Split-Path -Parent $PSScriptRoot; $platform='x86_64-windows'; $name="autolith-$Tag-$platform"
$out=[IO.Path]::GetFullPath((Join-Path $root $OutputDirectory)); $stage=Join-Path $env:RUNNER_TEMP "package-$PID"; $release=Join-Path $stage $name
try {
  New-Item -ItemType Directory -Force -Path $out,(Join-Path $release 'libexec\autolith'),(Join-Path $release 'bin'),(Join-Path $release 'native')|Out-Null
  $tar=Join-Path $stage 'source.tar'; & git -C $root archive --format=tar --output=$tar HEAD; if($LASTEXITCODE){throw 'git archive failed.'}; & tar.exe -xf $tar -C (Join-Path $release 'libexec\autolith'); if($LASTEXITCODE){throw 'tar extraction failed.'}
  $packagedSource = Join-Path $release 'libexec\autolith'
  & git -C $packagedSource init --quiet
  if ($LASTEXITCODE) { throw 'packaged source Git initialization failed.' }
  & git -C $packagedSource config core.autocrlf false
  & git -C $packagedSource config user.name 'Autolith package builder'
  & git -C $packagedSource config user.email 'package@localhost'
  & git -C $packagedSource add --all
  if ($LASTEXITCODE) { throw 'packaged source Git staging failed.' }
  & git -C $packagedSource commit --quiet -m 'Packaged Autolith source'
  if ($LASTEXITCODE) { throw 'packaged source Git commit failed.' }
  Add-Content -LiteralPath (Join-Path $packagedSource '.git\info\exclude') -Value '.qlot/'
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $root '.qlot') -Destination (Join-Path $release 'libexec\autolith\.qlot')
  $version=(Get-Content -Raw (Join-Path $root 'sbcl.version')).Trim(); $data=Join-Path $env:LOCALAPPDATA 'autolith\data'; $runtimeRoot=Join-Path $data "runtimes\$version"
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $runtimeRoot 'installation') -Destination (Join-Path $release 'runtime')
  & (Join-Path $PSScriptRoot 'prepare-windows-runtime.ps1') -Runtime (Join-Path $release 'runtime\sbcl.exe')
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $runtimeRoot 'source') -Destination (Join-Path $release 'libexec\sbcl-source')
  Copy-Item -Force (Join-Path $root 'bin\autolith.cmd') (Join-Path $release 'bin\autolith.cmd'); Copy-Item -Force (Join-Path $root 'bin\autolith-release.ps1') (Join-Path $release 'bin\autolith.ps1')
  $fff = Get-ChildItem -Recurse -File -LiteralPath (Join-Path $data 'native\fff') -Filter '*.dll' | Select-Object -First 1
  if (-not $fff) { throw 'Built FFF DLL is missing.' }
  Copy-Item -Force -LiteralPath $fff.FullName -Destination (Join-Path $release 'native\fff_c.dll')
  & (Join-Path $root 'bin\autolith-runtime.ps1') --script (Join-Path $PSScriptRoot 'windows-colorlisp.lisp') copy (Join-Path $release 'native\colorlisp.dll')
  if ($LASTEXITCODE -ne 0) { throw 'ColorLisp packaging or highlighting failed.' }
  . (Join-Path $root 'bin\autolith-sandbox.ps1')
  Copy-Item -LiteralPath $AutolithSandboxBundleScript -Destination (Join-Path $packagedSource 'bin\cl-exec-sandbox-bundle.ps1')
  $helper = Get-ClExecSandboxSelection (Get-AutolithSandboxRoot)
  $generation = Split-Path -Parent $helper
  $identity = Split-Path -Leaf $generation
  $bundleRoot = Join-Path $release 'native\sandbox'
  $packagedGeneration = Join-Path $bundleRoot $identity
  New-Item -ItemType Directory -Path $packagedGeneration -Force | Out-Null
  foreach ($file in $ClExecSandboxFiles) {
    Copy-Item -LiteralPath (Join-Path $generation $file) -Destination (Join-Path $packagedGeneration $file)
  }
  if ((Get-ClExecSandboxIdentity $packagedGeneration) -cne $identity) { throw 'Packaged sandbox bundle is corrupt.' }
  [IO.File]::WriteAllText((Join-Path $bundleRoot 'selected'), $identity + "`n", [Text.Encoding]::ASCII)
  . (Join-Path $root 'bin\autolith-openssl.ps1')
  $openssl = Find-AutolithOpenSsl -SourceRoot $root
  if (-not $openssl) { throw 'No complete OpenSSL 4 or 3 x64 DLL pair was found.' }
  foreach ($dll in $openssl.Crypto, $openssl.Ssl) {
    Copy-Item -Force -LiteralPath $dll -Destination (Join-Path $release 'native')
  }
  $commit=(& git -C $root rev-parse HEAD).Trim(); $versionName=($Tag -replace '^v','' -replace '-dev\..*$',''); @("version=$versionName","tag=$Tag","commit=$commit","platform=$platform")|Set-Content -Encoding ascii (Join-Path $release 'RELEASE')
  $archive=Join-Path $out "$name.zip"; if(Test-Path $archive){Remove-Item -Force $archive}; & tar.exe -a -cf $archive -C $stage $name; if($LASTEXITCODE){throw 'zip creation failed.'}
  $hash=(Get-FileHash $archive -Algorithm SHA256).Hash.ToLowerInvariant(); "$hash *$name.zip"|Set-Content -Encoding ascii "$archive.sha256"; $archive
} finally { if(Test-Path $stage){Remove-Item -Recurse -Force $stage} }
