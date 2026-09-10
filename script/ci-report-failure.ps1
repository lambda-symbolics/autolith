# Publish a failed CI step's output as check annotations.
#
# Usage: ci-report-failure.ps1 -Title TEXT -Log PATH [-Status N]
#
# GitHub Actions job logs need authentication, while check annotations are
# readable through the public API. A failing Windows step therefore reports
# the first error report, the log lines that mention errors, and the bounded
# tail of its log as ::error annotations, the way the Nix smoke step reports
# its first run. SBCL backtrace frames print whole forms on one line each and
# would crowd out the messages around them, so they are dropped first.
param(
  [Parameter(Mandatory=$true)][string]$Title,
  [Parameter(Mandatory=$true)][string]$Log,
  [int]$Status = 1
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$chunkCharacters = 3000
$tailCharacters = 15000
$framePattern = '^\s*\d+: \('
$errorStartPattern = '(?i)unhandled|debugger invoked|BUILD FAILED|^error|failed with status|installation failed|setup failed'
$highlightPattern = '(?i)error|fail|unhandled|debugger|warning|not found|denied|missing|unbound|undefined'

function Write-Annotation([string]$annotationTitle, [string]$message) {
  $escapedTitle = $annotationTitle.Replace('%', '%25').Replace(',', '%2C').Replace(':', '%3A')
  $escaped = $message.Replace('%', '%25').Replace("`r", '%0D').Replace("`n", '%0A')
  Write-Output "::error title=$escapedTitle::$escaped"
}

function Write-Chunked([string]$label, [string]$text, [int]$maximumChunks) {
  if ($text.Length -eq 0) { return }
  $chunks = [Math]::Min($maximumChunks, [Math]::Ceiling($text.Length / $chunkCharacters))
  for ($index = 0; $index -lt $chunks; $index++) {
    $start = $index * $chunkCharacters
    $length = [Math]::Min($chunkCharacters, $text.Length - $start)
    if ($length -le 0) { break }
    Write-Annotation "$Title (status $Status), $label $($index + 1) of $chunks" $text.Substring($start, $length)
  }
}

$text = if (Test-Path -LiteralPath $Log) {
  [IO.File]::ReadAllText($Log)
} else {
  "The log $Log was not written."
}
$lines = @($text -split "`r?`n" | Where-Object { $_ -notmatch $framePattern })
$compact = $lines -join "`n"

$firstError = ($lines | Select-String -Pattern $errorStartPattern | Select-Object -First 1)
if ($firstError) {
  $context = ($lines[($firstError.LineNumber - 1)..([Math]::Min($lines.Count - 1, $firstError.LineNumber + 60))]) -join "`n"
  Write-Chunked 'first error' $context 2
}

$highlights = @($lines | Where-Object { $_ -match $highlightPattern } | Select-Object -Last 40)
if ($highlights.Count -gt 0) {
  $joined = $highlights -join "`n"
  if ($joined.Length -gt $chunkCharacters) {
    $joined = $joined.Substring($joined.Length - $chunkCharacters)
  }
  Write-Annotation "$Title (status $Status), error lines" $joined
}

$tail = if ($compact.Length -gt $tailCharacters) {
  $compact.Substring($compact.Length - $tailCharacters)
} else {
  $compact
}
Write-Chunked 'log tail' $tail 5
