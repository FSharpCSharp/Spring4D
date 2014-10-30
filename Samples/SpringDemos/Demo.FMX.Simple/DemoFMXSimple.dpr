program DemoFMXSimple;

uses
  System.StartUpCopy,
  System.Classes,
  System.Messaging,
  FMX.MobilePreview,
  FMX.Platform,
  FMX.Forms,
  Spring.Container,
  Demo.FMX.Simple.MainForm in 'Demo.FMX.Simple.MainForm.pas' {Form1};

{$R *.res}

{$IFDEF IOS}
  {$DEFINE MOBILE}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE MOBILE}
{$ENDIF}

procedure RunApp; forward;

{$IFDEF MOBILE}
type
  TStartupMessageHandler = class
    class procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
  end;

{ TStartupMessageHandler }

class procedure TStartupMessageHandler.ApplicationEventHandler(const Sender: TObject;
  const M: TMessage);
begin
  if (M is TApplicationEventMessage) and
    (TApplicationEventMessage(M).Value.Event = TApplicationEvent.FinishedLaunching) then
  begin
    RunApp;
  end;
end;
{$ENDIF}

var
  Container : TContainer;

procedure RunApp;
begin
  //This forces form creation to be synchronous, may or may not be called
  //on certain platforms already by the FMX framework. Andorid
  //will call this prior sending the message but iOS won't. May be called
  //multiple times with no harm done.
  Application.RealCreateForms;
  //Create instance and assign MainForm (would otherwise be done by
  //Application.CreateMainForm)
  Application.MainForm := Container.Resolve<TForm1>;
  //And make it visible as this may not be set in the designer and no window
  //would be displayed
  Application.MainForm.Visible := true;
end;

procedure RegisterServices(const Container: TContainer);
begin
  //Beware using singletons here as reference to the returned instance may be
  //held by mutliple other (FMX) objects
  Container.RegisterType<TForm1>
    .Implements<TForm1>
    .DelegateTo(
      function: TForm1
      begin
        Application.CreateForm(TForm1, Result);
      end);

  Container.Build;
end;

begin
  Application.Initialize;
  Container := TContainer.Create;
  try
    RegisterServices(Container);
{$IFDEF MOBILE}
    //Platform service needs to handle that
    TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage,
      TStartupMessageHandler.ApplicationEventHandler);
{$ELSE}
    //Can be called synchronously
    RunApp;
{$ENDIF}
    Application.Run;
  finally
    Container.Free;
  end;
  ReportMemoryLeaksOnShutdown := True;
end.
