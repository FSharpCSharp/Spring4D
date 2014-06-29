program ORMRegisterTool;

uses
  Forms,
  ViewMain in 'View\ViewMain.pas' {frmMain},
  Registration.EnvironmentVariable in 'Registration.EnvironmentVariable.pas',
  Constants in 'Constants.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
