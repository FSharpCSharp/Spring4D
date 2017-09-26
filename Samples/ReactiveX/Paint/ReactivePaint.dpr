program ReactivePaint;

uses
  Vcl.Forms,
  Unit1 in '..\MouseEvents\Unit1.pas' {Form104};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm104, Form104);
  Application.Run;
end.
