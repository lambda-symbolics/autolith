param([string]$OutputDirectory='dist',[Parameter(Mandatory=$true)][string]$Tag)
$ErrorActionPreference='Stop'; Set-StrictMode -Version Latest
if ($Tag -notmatch '^v\d+\.\d+\.\d+(?:-dev\.\d+)?$') { throw 'Tag is malformed.' }
$root=Split-Path -Parent $PSScriptRoot; $platform='x86_64-windows'; $name="autolith-$Tag-$platform"
$out=[IO.Path]::GetFullPath((Join-Path $root $OutputDirectory)); $stage=Join-Path $env:RUNNER_TEMP "package-$PID"; $release=Join-Path $stage $name
try {
  New-Item -ItemType Directory -Force -Path $out,(Join-Path $release 'libexec\autolith'),(Join-Path $release 'bin'),(Join-Path $release 'native')|Out-Null
  $tar=Join-Path $stage 'source.tar'; & git -C $root archive --format=tar --output=$tar HEAD; if($LASTEXITCODE){throw 'git archive failed.'}; & tar.exe -xf $tar -C (Join-Path $release 'libexec\autolith'); if($LASTEXITCODE){throw 'tar extraction failed.'}
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $root '.qlot') -Destination (Join-Path $release 'libexec\autolith\.qlot')
  $version=(Get-Content -Raw (Join-Path $root 'sbcl.version')).Trim(); $data=Join-Path $env:LOCALAPPDATA 'autolith\data'; $runtimeRoot=Join-Path $data "runtimes\$version"
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $runtimeRoot 'installation') -Destination (Join-Path $release 'runtime')
  Copy-Item -Recurse -Force -LiteralPath (Join-Path $runtimeRoot 'source') -Destination (Join-Path $release 'libexec\sbcl-source')
  Copy-Item -Force (Join-Path $root 'bin\autolith.cmd') (Join-Path $release 'bin\autolith.cmd'); Copy-Item -Force (Join-Path $root 'bin\autolith-release.ps1') (Join-Path $release 'bin\autolith.ps1')
  $fff = Get-ChildItem -Recurse -File -LiteralPath (Join-Path $data 'native\fff') -Filter '*.dll' | Select-Object -First 1
  $colorlisp = Get-ChildItem -Recurse -File -LiteralPath (Join-Path $env:USERPROFILE '.cache\colorlisp') | Where-Object { $_.Name -match 'colorlisp' } | Select-Object -First 1
  if (-not $fff -or -not $colorlisp) { throw 'Built FFF or ColorLisp DLL is missing.' }
  Copy-Item -Force -LiteralPath $fff.FullName -Destination (Join-Path $release 'native\fff_c.dll')
  Copy-Item -Force -LiteralPath $colorlisp.FullName -Destination (Join-Path $release 'native\colorlisp.dll')
  $git=(Get-Command git.exe).Source; $gitRoot=Split-Path -Parent (Split-Path -Parent $git); foreach($dll in 'libcrypto-3-x64.dll','libssl-3-x64.dll'){ $p=Join-Path $gitRoot "mingw64\bin\$dll"; if(-not(Test-Path $p)){throw "$dll is missing."}; Copy-Item -Force $p (Join-Path $release 'native') }
  $commit=(& git -C $root rev-parse HEAD).Trim(); $versionName=($Tag -replace '^v','' -replace '-dev\..*$',''); @("version=$versionName","tag=$Tag","commit=$commit","platform=$platform")|Set-Content -Encoding ascii (Join-Path $release 'RELEASE')
  $archive=Join-Path $out "$name.zip"; if(Test-Path $archive){Remove-Item -Force $archive}; Compress-Archive -LiteralPath $release -DestinationPath $archive -CompressionLevel Optimal
  $hash=(Get-FileHash $archive -Algorithm SHA256).Hash.ToLowerInvariant(); "$hash *$name.zip"|Set-Content -Encoding ascii "$archive.sha256"; $archive
} finally { if(Test-Path $stage){Remove-Item -Recurse -Force $stage} }
