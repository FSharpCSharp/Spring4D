program Spring.Core.Tests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Spring.Tests.IoC.Components in 'Tests\Spring.Tests.IoC.Components.pas',
  Spring.Tests.IoC.LifetimeManager in 'Tests\Spring.Tests.IoC.LifetimeManager.pas',
  Spring.Tests.IoC in 'Tests\Spring.Tests.IoC.pas';

{$R *.RES}

procedure RegisterTestCases;
begin
  RegisterTests('Spring IoC Tests', [
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestEmptyContainer.Suite,
    TTestSimpleContainer.Suite,
    TTestDifferentServiceImplementations.Suite,
    TTestActivatorDelegate.Suite,
    TTestTypedInjectionByCoding.Suite,
    TTestTypedInjectionsByAttribute.Suite,
    TTestDirectCircularDependency.Suite,
    TTestCrossedCircularDependency.Suite
  ]);
end;

begin
  Application.Initialize;
  RegisterTestCases;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

