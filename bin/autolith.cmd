@echo off
rem Run the stable Windows launcher from cmd.exe, preferring PowerShell 7.
setlocal
where pwsh >nul 2>nul
if %ERRORLEVEL% equ 0 (
  pwsh -NoProfile -ExecutionPolicy Bypass -File "%~dp0autolith.ps1" %*
) else (
  powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0autolith.ps1" %*
)
exit /b %ERRORLEVEL%
