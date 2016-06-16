program Project3;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form1},
  Spring.Lambda in '..\Spring.Lambda.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
