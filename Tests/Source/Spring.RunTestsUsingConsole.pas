unit Spring.RunTestsUsingConsole;

interface

uses
  TestFramework;

procedure RunRegisteredTestCases();

implementation

uses
  SysUtils,
  TextTestRunner,
  Spring.TestUtils;

var
  ExitBehavior: TRunnerExitBehavior = rxbContinue;

procedure RunRegisteredTestCases();
begin
  WriteLn('To run with rxbPause, use -p switch');
  WriteLn('To run with rxbHaltOnFailures, use -h switch');
  WriteLn('No switch runs as rxbContinue');

  if FindCmdLineSwitch('p', ['-', '/'], true) then
    ExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    ExitBehavior := rxbHaltOnFailures;
  ProcessTestResult(TextTestRunner.RunRegisteredTests(ExitBehavior));
{$ifdef MACOS}
  {$ifdef DEBUG}
  Write('Press <Enter>');
  Readln;
  {$endif DEBUG}
{$endif MACOS}
end;

end.
