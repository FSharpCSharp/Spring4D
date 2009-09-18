program Environments;

uses
  Classes,
  Forms,
  DemoForm in 'Source\DemoForm.pas' {frmDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
