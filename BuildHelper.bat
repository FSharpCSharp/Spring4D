@echo off
CALL %1
%FrameworkDir%\msbuild.exe /nologo %2 /target:build /p:%3 /p:%4 /p:%5
if "%6"=="" goto :eof
pause