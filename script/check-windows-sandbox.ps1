param([Parameter(Mandatory=$true)][string]$Archive)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$name = "AutolithSandbox$PID"
$root = Join-Path $env:PUBLIC $name
$password = ConvertTo-SecureString (([Guid]::NewGuid().ToString('N')) + '!aA1') -AsPlainText -Force
$user = $null
try {
  $user = New-LocalUser -Name $name -Password $password -AccountNeverExpires
  New-Item -ItemType Directory -Path $root | Out-Null
  & icacls.exe $root /inheritance:r /grant:r "*$($user.SID.Value):(OI)(CI)F" '*S-1-5-18:(OI)(CI)F' '*S-1-5-32-544:(OI)(CI)F'
  if ($LASTEXITCODE -ne 0) { throw 'Cannot prepare standard-user sandbox test root.' }
  Expand-Archive -LiteralPath $Archive -DestinationPath $root
  $release = Join-Path $root ([IO.Path]::GetFileNameWithoutExtension($Archive))
  $runner = Join-Path $release 'libexec\autolith\script\check-windows-sandbox-user.ps1'
  $credential = [Management.Automation.PSCredential]::new("$env:COMPUTERNAME\$name", $password)
  $process = Start-Process -FilePath (Get-Command pwsh.exe).Source -Credential $credential -LoadUserProfile -PassThru -WorkingDirectory $root -ArgumentList @('-NoProfile','-File',"`"$runner`"",'-Release',"`"$release`"",'-Root',"`"$root`"")
  if (-not $process.WaitForExit(1200000)) {
    & taskkill.exe /PID $process.Id /T /F
    throw 'Packaged Windows sandbox tests timed out.'
  }
  $process.Refresh()
  $log = Join-Path $root 'sandbox.log'
  if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log }
  if ($process.ExitCode -ne 0) { throw "Packaged standard-user sandbox tests failed: $($process.ExitCode)" }
} finally {
  if ($user) {
    Get-CimInstance Win32_UserProfile | Where-Object SID -eq $user.SID.Value | Remove-CimInstance
    Remove-LocalUser -Name $name
  }
  if (Test-Path -LiteralPath $root) {
    # Saved cores have owner-only ACLs. Reclaim the disposable account's files.
    & takeown.exe /F $root /R /D Y | Out-Null
    & icacls.exe $root /grant '*S-1-5-32-544:(OI)(CI)F' /T /C | Out-Null
    Remove-Item -Recurse -Force -LiteralPath $root
  }
}
