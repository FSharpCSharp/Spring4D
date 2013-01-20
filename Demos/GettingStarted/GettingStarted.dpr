program GettingStarted;

uses
  UsedUnits in 'Source\UsedUnits.pas',
  Forms,
  ViewMain in 'Views\ViewMain.pas' {frmMain},
  ProductModel in 'Source\ProductModel.pas',
  ViewEditProduct in 'Views\ViewEditProduct.pas' {frmEditProduct};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$WARNINGS ON}
  Application.Run;
end.
