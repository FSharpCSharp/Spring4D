unit Spring.RunTestsUsingXmlOutput;

interface

procedure RunRegisteredTestCases();

implementation

uses
  SysUtils,
  TestFramework,
  FinalBuilder.XMLTestRunner,
  Spring.TestUtils;

var
  OutputFile: string = 'Spring.Tests.Reports.xml';
  ConfigFile: string;

procedure RunRegisteredTestCases();
begin
  if ConfigFile <> '' then
  begin
    RegisteredTests.LoadConfiguration(ConfigFile, False, True);
    WriteLn('Loaded config file ' + ConfigFile);
  end;
  if ParamCount > 0 then
    OutputFile := ParamStr(1);
  WriteLn('Writing output to ' + OutputFile);
  WriteLn(Format('Running %d of %d test cases', [RegisteredTests.CountEnabledTestCases, RegisteredTests.CountTestCases]));
  ProcessTestResult(FinalBuilder.XMLTestRunner.RunRegisteredTests(OutputFile));
end;

end.
