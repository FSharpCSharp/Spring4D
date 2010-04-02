program BindingDemo;



uses
  Forms,
  Model in 'Source\Model.pas',
  VclControlsBindingForm in 'Source\VclControlsBindingForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
