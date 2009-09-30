program SpringFrameworkTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Classes,
  Forms,
  Windows,
  SysUtils,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.System,
  Spring.System.Tests in 'Tests\Spring.System.Tests.pas',
  Spring.Helpers.Tests in 'Tests\Spring.Helpers.Tests.pas',
  Spring.Security.Tests in 'Tests\Spring.Security.Tests.pas';

{$R *.RES}

procedure RegisterTestCases;
begin
  RegisterTests('Spring System Tests', [
    TTestSplitString.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestVersion.Suite,
    TTestEnum.Suite,
    TTestBuffer.Suite,
    TTestEmptyBuffer.Suite,
    TTestFiveBytesBuffer.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3)
  ]);

  RegisterTests('Spring Cryptography Tests', [
    TTestMD5.Suite,
    TTestDES.Suite,
    TTestTripleDES.Suite
  ]);

  RegisterTests('Spring Patterns Tests', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Spring Helpers Tests', [
    TTestGuidHelper.Suite,
    TTestPersistentSnapshot.Suite
  ]);
end;

begin
  Application.Initialize;
  RegisterTestCases;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

