program Log4DDemo;

uses
  Forms,
  Log4DDemo1 in 'Log4DDemo1.pas' {frmLog4DDemo},
  Log4D in '..\src\Log4D.pas',
  Log4DIndy in '..\src\Log4DIndy.pas',
  Log4DXML in '..\src\Log4DXML.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Log4D Demo';
  Application.CreateForm(TfrmLog4DDemo, frmLog4DDemo);
  Application.Run;
end.
