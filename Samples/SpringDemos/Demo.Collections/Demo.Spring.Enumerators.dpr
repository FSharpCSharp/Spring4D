program Demo.Spring.Enumerators;

uses
  Vcl.Forms,
  frmEnumerableDemo in 'frmEnumerableDemo.pas' {EnumerationDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEnumerationDemoForm, EnumerationDemoForm);
  Application.Run;
end.
