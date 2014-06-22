@echo off
echo %1
echo %2
echo %3
echo %4
echo %5
echo %6
echo %7
CALL %1
%FrameworkDir%\msbuild.exe /nologo %2 /target:build /p:DCC_BuildAllUnits=true /p:%3 /p:%4 /p:%5 /p:%6
if "%7"=="" goto :eof
pause
