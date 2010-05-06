program FileSearcher;

uses
  FastMM4,
  Forms,
  MainForm in 'Source\MainForm.pas' {frmSearchDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSearchDemo, frmSearchDemo);
  Application.Run;
end.
