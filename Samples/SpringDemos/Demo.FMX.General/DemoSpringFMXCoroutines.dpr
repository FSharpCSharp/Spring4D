program DemoSpringFMXCoroutines;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Spring.Coroutines in '..\..\..\Source\Base\Spring.Coroutines.pas',
  Spring.Collections.Sequences in '..\..\..\Source\Base\Collections\Spring.Collections.Sequences.pas',
  Demo.Spring.FMX.Coroutines.Main in 'Demo.Spring.FMX.Coroutines.Main.pas' {frmCoroutinesMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCoroutinesMain, frmCoroutinesMain);
  Application.Run;
end.
