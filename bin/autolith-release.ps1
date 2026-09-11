$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

Add-Type -TypeDefinition @'
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Text;

public static class AutolithWindowsPath
{
    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern uint GetShortPathName(
        string longPath,
        StringBuilder shortPath,
        uint shortPathLength);

    public static string GetShortPath(string path)
    {
        var buffer = new StringBuilder(260);
        var length = GetShortPathName(path, buffer, (uint)buffer.Capacity);
        if (length == 0)
            throw new Win32Exception(Marshal.GetLastWin32Error());
        if (length >= buffer.Capacity)
        {
            buffer.Capacity = checked((int)length + 1);
            length = GetShortPathName(path, buffer, (uint)buffer.Capacity);
            if (length == 0)
                throw new Win32Exception(Marshal.GetLastWin32Error());
        }
        return buffer.ToString();
    }
}
'@

function ConvertTo-AutolithSbclCorePath {
  param([Parameter(Mandatory = $true)][string]$Path)
  $parent = Split-Path -Parent $Path
  $leaf = Split-Path -Leaf $Path
  $shortParent = [AutolithWindowsPath]::GetShortPath($parent)
  $runtimePath = Join-Path $shortParent $leaf
  if ($runtimePath -match '[^\x00-\x7F]') {
    throw "Autolith cannot obtain an ASCII Windows path for SBCL core images below $parent."
  }
  return $runtimePath
}
$releaseRoot = Split-Path -Parent $PSScriptRoot
$sourceRoot = Join-Path $releaseRoot 'libexec\autolith'
$runtime = Join-Path $releaseRoot 'runtime\sbcl.exe'
$runtimeSource = Join-Path $releaseRoot 'libexec\sbcl-source'
$record = Join-Path $releaseRoot 'RELEASE'
if (-not (Test-Path -LiteralPath $record)) { throw 'Autolith release failed: RELEASE is missing.' }
$fields = @{}; Get-Content -LiteralPath $record | ForEach-Object { if ($_ -match '^([^=]+)=(.*)$') { $fields[$Matches[1]] = $Matches[2] } }
if ($fields.platform -ne 'x86_64-windows') { throw "Autolith release failed: RELEASE platform $($fields.platform) is not x86_64-windows." }
if (-not (Test-Path -LiteralPath $runtime) -or -not (Test-Path -LiteralPath (Join-Path $runtimeSource 'version.lisp-expr'))) { throw 'Autolith release failed: bundled SBCL runtime or source is missing.' }
$env:AUTOLITH_SBCL = $runtime
$env:AUTOLITH_SBCL_SOURCE_ROOT = $runtimeSource
$native = Join-Path $releaseRoot 'native'
$fff = Get-ChildItem -LiteralPath $native -Filter '*fff*.dll' -File | Select-Object -First 1
$color = Get-ChildItem -LiteralPath $native -Filter '*colorlisp*.dll' -File | Select-Object -First 1
if (-not $fff -or -not $color) { throw 'Autolith release failed: bundled native libraries are missing.' }
$env:AUTOLITH_FFF_LIBRARY = $fff.FullName
$env:COLORLISP_NATIVE_LIBRARY = $color.FullName
$env:PATH = "$native;$env:PATH"
$env:GIT_OPTIONAL_LOCKS = '0'
if ($args.Count -gt 0 -and $args[0] -eq '--autolith-release-probe') {
  "version=$($fields.version)"; "tag=$($fields.tag)"; "commit=$($fields.commit)"; "platform=$($fields.platform)"; "source=$sourceRoot"; "runtime=$runtime"; exit 0
}
$dataRoot = Join-Path $env:LOCALAPPDATA 'autolith\data'
$activeCore = Join-Path $dataRoot 'active\autolith-active.core'
$recoveryCore = Join-Path $dataRoot 'recovery\autolith-recovery.core'
$marker = Join-Path $dataRoot 'release-images'
$identity = "$($fields.tag):$($fields.platform)"
$usable = (Test-Path -LiteralPath $activeCore) -and (Test-Path -LiteralPath $recoveryCore) -and (Test-Path -LiteralPath $marker) -and ((Get-Content -Raw -LiteralPath $marker).Trim() -eq $identity)
if (-not $usable) {
  [Console]::Error.WriteLine('Building Autolith images for this machine. This happens once per release.')
  New-Item -ItemType Directory -Force -Path (Split-Path -Parent $activeCore),(Split-Path -Parent $recoveryCore) | Out-Null
  $activeRuntimeCore = ConvertTo-AutolithSbclCorePath $activeCore
  $recoveryRuntimeCore = ConvertTo-AutolithSbclCorePath $recoveryCore
  & $runtime --script (Join-Path $sourceRoot 'script\build-recovery.lisp') $recoveryRuntimeCore
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $runtime --script (Join-Path $sourceRoot 'script\build-active.lisp') $activeRuntimeCore
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  Set-Content -Encoding ascii -NoNewline -LiteralPath $marker -Value $identity
}
$env:AUTOLITH_ACTIVE_CORE = ConvertTo-AutolithSbclCorePath $activeCore
$env:AUTOLITH_RECOVERY_CORE = ConvertTo-AutolithSbclCorePath $recoveryCore
& $runtime --script (Join-Path $sourceRoot 'script\runtime.lisp') --script (Join-Path $sourceRoot 'script\launcher.lisp') @args
exit $LASTEXITCODE
