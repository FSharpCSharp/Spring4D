@echo off

if "%1" == "" goto :Usage

ECHO Clean...
REM msbuild.exe /nologo /t:Clean /p:Config=%1 "Delphi Spring Framework.groupproj"
msbuild.exe /nologo /t:Clean /p:Config=%1 "Packages\Spring.Base.dproj"
msbuild.exe /nologo /t:Clean /p:Config=%1 "Packages\Spring.Core.dproj"
msbuild.exe /nologo /t:Clean /p:Config=%1 "Packages\Spring.Extensions.dproj"

ECHO Building...
REM msbuild.exe /nologo /t:Build /p:Config=%1 "Delphi Spring Framework.groupproj"
msbuild.exe /nologo /t:Build /p:Config=%1 "Packages\Spring.Base.dproj"
msbuild.exe /nologo /t:Build /p:Config=%1 "Packages\Spring.Core.dproj"
msbuild.exe /nologo /t:Build /p:Config=%1 "Packages\Spring.Extensions.dproj"

GOTO :End

:Usage
ECHO Usage: Build.bat [DEBUG / RELEASE]
GOTO :End

:End
@Pause
