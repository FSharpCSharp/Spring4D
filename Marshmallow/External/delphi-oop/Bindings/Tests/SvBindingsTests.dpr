// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program SvBindingsTests;
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
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSvBindings in 'TestSvBindings.pas',
  SvBindings.Converters in '..\SvBindings.Converters.pas',
  SvBindings in '..\SvBindings.pas',
  SvBindings.Validation in '..\SvBindings.Validation.pas',
  SvDesignPatterns in '..\..\Core\SvDesignPatterns.pas',
  SvRttiUtils in '..\..\Core\SvRttiUtils.pas',
  ViewTestBindings in 'ViewTestBindings.pas' {frmTest},
  DataObject in 'DataObject.pas',
  SvBindings.Converters.DWScript in '..\SvBindings.Converters.DWScript.pas';

{$R *.RES}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutDown := True;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

