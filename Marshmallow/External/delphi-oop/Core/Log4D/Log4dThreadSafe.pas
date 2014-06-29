unit Log4dThreadSafe;

// These classes implement an abstract thread-safe appender that can be
// used as base class to append to GUI elements or databases, both of which
// may only safely be written to from the main thread.
//
// Contributed on 2007-10-11 by Beat Bolli <beat.bolli@swissqual.com>

interface

uses Log4D, Classes, Windows, SyncObjs;

type
  TAppenderThread = class;

  TLogThreadSafeAppender = class(TLogCustomAppender)
  private
    FThread: TAppenderThread;
    FMessages: TThreadList;
  protected
    procedure DoAppend(const msg: string); override;
  public
    destructor Destroy; override;
    procedure Init; override;
    // This procedure is called in the main thread context
    // Override at least this in your subclass.
    procedure AppendSynchronized(msg: string); virtual; abstract;
  end;

  TAppenderThread = class(TThread)
  // Background thread that is woken after each message.
  // It calls the appender's AppendSynchronized() from the main thread.
  //
  // This design decouples the logging thread from the output.
  private
    FAppender: TLogThreadSafeAppender;
    FReady: TEvent;
    FMessages: TList;
    FMsg: string;
  protected
    procedure Execute; override;
    procedure Append;
    procedure Finis(Sender: TObject);
  public
    constructor Create(appender: TLogThreadSafeAppender);
    procedure Signal;
  end;

implementation

uses SysUtils;

{ TLogThreadSafeAppender }

destructor TLogThreadSafeAppender.Destroy;
var
  pc: PChar;
  AList: TList;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  // clear FMessages
  AList := FMessages.LockList();
  for pc in AList do
    StrDispose(pc);
  AList.Clear;
  FMessages.UnlockList;
  FreeAndNil(FMessages);
  inherited;
end;

procedure TLogThreadSafeAppender.Init;
begin
  inherited;
  FMessages := TThreadList.Create();
  FThread := TAppenderThread.Create(self);
end;

procedure TLogThreadSafeAppender.DoAppend(const msg: string);
var
  pc: PChar;
begin
  if Assigned(FMessages) then
  begin
    // Add a copy of the new message to the queue
    pc := PChar(msg);
    pc := StrNew(pc);
    FMessages.Add(pc);
    // Unblock the GUI appender thread
    FThread.Signal();
  end;
end;

{ TAppenderThread }

constructor TAppenderThread.Create(appender: TLogThreadSafeAppender);
begin
  inherited Create(false);
  // FreeOnTerminate := true;
  OnTerminate := Finis;

  FAppender := appender;
  FReady := TEvent.Create(nil, false, false, '');
  FMessages := TList.Create();
end;

procedure TAppenderThread.Finis(Sender: TObject);
begin
  FreeAndNil(FReady);
  FreeAndNil(FMessages);
end;

procedure TAppenderThread.Execute;
var
  l: TList;
  pc: PChar;
begin
  while not Terminated do
    if FReady.WaitFor(1000) = wrSignaled then
    begin
      // Copy the message list so that it can be unlocked as quickly as possible
      l := FAppender.FMessages.LockList();
      FMessages.Assign(l);
      l.Clear;
      FAppender.FMessages.UnlockList();
      // Display each message in the main thread context
      for pc in FMessages do
      begin
        FMsg := pc;
        StrDispose(pc);
        Synchronize(Append);
      end;
      FMessages.Clear();
    end;
end;

procedure TAppenderThread.Append;
begin
  FAppender.AppendSynchronized(FMsg);
end;

procedure TAppenderThread.Signal;
begin
  if Assigned(FReady) then
    FReady.SetEvent;
end;

end.

