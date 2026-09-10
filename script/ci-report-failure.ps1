# Publish a failed CI step's output as check annotations.
#
# Usage: ci-report-failure.ps1 -Title TEXT -Log PATH [-Status N]
#
# GitHub Actions job logs need authentication, while check annotations are
# readable through the public API. A failing Windows step therefore reports
# the log lines that mention errors and the bounded tail of its log as
# ::error annotations, the way the Nix smoke step reports its first run.
param(
  [Parameter(Mandatory=$true)][string]$Title,
  [Parameter(Mandatory=$true)][string]$Log,
  [int]$Status = 1
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$chunkCharacters = 3000
$tailCharacters = 18000
$highlightPattern = '(?i)error|fail|unhandled|debugger|warning|not found|denied|missing|unbound|undefined'

function Write-Annotation([string]$annotationTitle, [string]$message) {
  $escapedTitle = $annotationTitle.Replace('%', '%25').Replace(',', '%2C').Replace(':', '%3A')
  $escaped = $message.Replace('%', '%25').Replace("`r", '%0D').Replace("`n", '%0A')
  Write-Output "::error title=$escapedTitle::$escaped"
}

$text = if (Test-Path -LiteralPath $Log) {
  [IO.File]::ReadAllText($Log)
} else {
  "The log $Log was not written."
}
$lines = $text -split "`r?`n"
$highlights = @($lines | Where-Object { $_ -match $highlightPattern } | Select-Object -Last 40)
if ($highlights.Count -gt 0) {
  $joined = $highlights -join "`n"
  if ($joined.Length -gt $chunkCharacters) {
    $joined = $joined.Substring($joined.Length - $chunkCharacters)
  }
  Write-Annotation "$Title (status $Status), error lines" $joined
}
$tail = if ($text.Length -gt $tailCharacters) {
  $text.Substring($text.Length - $tailCharacters)
} else {
  $text
}
$chunks = [Math]::Max(1, [Math]::Ceiling($tail.Length / $chunkCharacters))
for ($index = 0; $index -lt $chunks; $index++) {
  $start = $index * $chunkCharacters
  $length = [Math]::Min($chunkCharacters, $tail.Length - $start)
  if ($length -le 0) { break }
  Write-Annotation "$Title (status $Status), log tail $($index + 1) of $chunks" $tail.Substring($start, $length)
}
