@echo off

goto :Delphi101

:DelphiXE
echo.
echo Building for Delphi XE
echo.
if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\bin\rsvars.bat"
msbuild Packages\DelphiXE\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
)

:DelphiXE2
echo.
echo Building for Delphi XE2
echo.
if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
msbuild Packages\DelphiXE2\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE2\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE3
echo.
echo Building for Delphi XE3
echo.
if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\rsvars.bat"
msbuild Packages\DelphiXE3\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE3\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE4
echo.
echo Building for Delphi XE4
echo.
if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\rsvars.bat"
msbuild Packages\DelphiXE4\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE4\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE5
echo.
echo Building for Delphi XE5
echo.
if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\rsvars.bat"
msbuild Packages\DelphiXE5\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE5\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE6
echo.
echo Building for Delphi XE6
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\rsvars.bat"
msbuild Packages\DelphiXE6\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE6\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE7
echo.
echo Building for Delphi XE7
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"
msbuild Packages\DelphiXE7\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE7\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:DelphiXE8
echo.
echo Building for Delphi XE8
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\rsvars.bat"
msbuild Packages\DelphiXE8\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\DelphiXE8\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi10
echo.
echo Building for Delphi 10
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\17.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\17.0\bin\rsvars.bat"
msbuild Packages\Delphi10Seattle\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi10Seattle\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi101
echo.
echo Building for Delphi 10.1
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\rsvars.bat"
msbuild Packages\Delphi10Berlin\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi10Berlin\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi102
echo.
echo Building for Delphi 10.2
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"
msbuild Packages\Delphi10Tokyo\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi10Tokyo\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi103
echo.
echo Building for Delphi 10.3
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat"
msbuild Packages\Delphi10Rio\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi10Rio\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi104
echo.
echo Building for Delphi 10.4
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"
msbuild Packages\Delphi10Sydney\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi10Sydney\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

:Delphi11
echo.
echo Building for Delphi 11
echo.
if exist "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat" (
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
msbuild Packages\Delphi11\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win32 /verbosity:quiet
msbuild Packages\Delphi11\Spring4D.groupproj /target:build /p:DCC_BuildAllUnits=true /p:Config=Debug /p:Platform=Win64 /verbosity:quiet
)

rem pause