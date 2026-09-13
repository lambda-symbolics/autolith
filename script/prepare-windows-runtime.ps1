# Select UTF-8 for SBCL's narrow Win32 startup and core I/O APIs.
# Modify only the staged release executable, preserving its existing manifest.
param([Parameter(Mandatory = $true)][string]$Runtime)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$tool = Get-Command mt.exe -ErrorAction SilentlyContinue
if ($tool) {
  $manifestTool = $tool.Source
} else {
  $sdk = Join-Path ${env:ProgramFiles(x86)} 'Windows Kits\10\bin'
  $candidate = Get-ChildItem -Path (Join-Path $sdk '*\x64\mt.exe') -File |
    Sort-Object FullName -Descending | Select-Object -First 1
  if (-not $candidate) { throw 'Packaging SBCL requires mt.exe from the Windows SDK.' }
  $manifestTool = $candidate.FullName
}
$manifest = [IO.Path]::GetTempFileName()
try {
  & $manifestTool -nologo "-inputresource:$Runtime;#1" "-out:$manifest"
  if ($LASTEXITCODE -ne 0) { throw 'Cannot read the SBCL executable manifest.' }
  $document = [xml][IO.File]::ReadAllText($manifest)
  $namespace = 'urn:schemas-microsoft-com:asm.v3'
  $settingsNamespace = 'http://schemas.microsoft.com/SMI/2019/WindowsSettings'
  $application = $document.DocumentElement.SelectSingleNode(
    "*[local-name()='application' and namespace-uri()='$namespace']")
  if (-not $application) {
    $application = $document.CreateElement('application', $namespace)
    [void]$document.DocumentElement.AppendChild($application)
  }
  $settings = $application.SelectSingleNode("*[local-name()='windowsSettings']")
  if (-not $settings) {
    $settings = $document.CreateElement('windowsSettings', $namespace)
    [void]$application.AppendChild($settings)
  }
  $codePage = $settings.SelectSingleNode("*[local-name()='activeCodePage']")
  if (-not $codePage) {
    $codePage = $document.CreateElement('activeCodePage', $settingsNamespace)
    [void]$settings.AppendChild($codePage)
  }
  $codePage.InnerText = 'UTF-8'
  $document.Save($manifest)
  & $manifestTool -nologo -manifest $manifest "-outputresource:$Runtime;#1"
  if ($LASTEXITCODE -ne 0) { throw 'Cannot embed the UTF-8 SBCL manifest.' }
} finally {
  Remove-Item -Force -LiteralPath $manifest
}
