unit Spring.RunTestsUsingVclGui;

interface

procedure RunRegisteredTestCases();

implementation

uses
  Forms,
  GUITestRunner;

procedure RunRegisteredTestCases();
begin
  Application.Initialize();
  TGUITestRunner.RunRegisteredTests();
end;

end.
