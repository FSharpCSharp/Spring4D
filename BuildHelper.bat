@echo off
echo %1
echo %2
echo %3
echo %4
echo %5
echo %6
echo "%7"
CALL %1
setlocal
for /f "tokens=2,4" %%c in ('echo %~4 %~3') do set buildlog=%~dp2%%c.%%d.MSBuildLog.txt
echo %buildlog%
%FrameworkDir%\msbuild.exe /nologo %2 /target:build /p:DCC_BuildAllUnits=true /p:%3 /p:%4 /p:%5 /p:%6 /l:FileLogger,Microsoft.Build.Engine;logfile=%buildlog%
endlocal
if "%7"=="" goto :eof
pause
