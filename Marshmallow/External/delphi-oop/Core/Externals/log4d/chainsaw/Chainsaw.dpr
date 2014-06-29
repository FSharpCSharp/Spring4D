program Chainsaw;

uses
  Forms,
  ChainsawMain in 'ChainsawMain.pas' {frmChainsaw},
  ChainsawConfig in 'ChainsawConfig.pas' {frmConfig},
  Log4D in '..\src\Log4D.pas',
  Log4DXML in '..\src\Log4DXML.pas',
  ChainsawData in 'ChainsawData.pas' {dtmLogging: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Chainsaw';
  Application.CreateForm(TdtmLogging, dtmLogging);
  Application.CreateForm(TfrmChainsaw, frmChainsaw);
  Application.Run;
end.
