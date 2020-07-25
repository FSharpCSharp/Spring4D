program MeasureEventInvoke;

{$APPTYPE CONSOLE}

uses
  Classes,
  Diagnostics,
  SysUtils,
  Spring;

type
  TTestEvent1 = procedure (Sender: TObject) of object;
  TTestEvent2 = procedure (Sender: TObject; a: Double; b, c, d, e: Integer) of object;
  TEventHandler = class
    procedure HandleEvent1(Sender: TObject);
    procedure HandleEvent2(Sender: TObject; a: Double; b, c, d, e: Integer);
  end;

var
  HandlerCount: Integer = 8;
{$IFDEF POSIX}
  ThreadCount: Integer = 4;
  EventCallCount: Int64 = 10000000;
{$ELSE}
  ThreadCount: Integer = 16;
  EventCallCount: Int64 = 10000000;
{$ENDIF}

function MakeThreadProc(const workload: TProc<Integer>; index: Integer): TProc;
begin
  Result := procedure begin workload(index) end;
end;

function Measure(const workload: TProc<Integer>): Int64;
var
  threads: array of TThread;
  i: Integer;
  sw: TStopwatch;
begin
  SetLength(threads, ThreadCount);
  for i := 0 to ThreadCount-1 do
    threads[i] := TThread.CreateAnonymousThread(MakeThreadProc(workload, i));
  sw := TStopwatch.StartNew;
  for i := 0 to ThreadCount-1 do
  begin
    threads[i].FreeOnTerminate := False;
    threads[i].Start;
  end;

  for i := 0 to ThreadCount-1 do
  begin
    while not threads[i].Started do Sleep(0);
    threads[i].Free;
  end;
  Result := sw.ElapsedMilliseconds;
end;

threadvar
  CallCount: Int64;

procedure TEventHandler.HandleEvent1(Sender: TObject);
begin
  Inc(CallCount);
end;

procedure TEventHandler.HandleEvent2(Sender: TObject; a: Double; b, c, d, e: Integer);
begin
  Inc(CallCount);
end;

procedure Main;
var
  e1: Event<TTestEvent1>;
  e2: Event<TTestEvent2>;
  i: Integer;
  t: TEventHandler;
  TotalCallCount: Int64;
  TotalDuration: Int64;
  params: TStrings;
begin
  if FindCmdLineSwitch('?') then
  begin
    Writeln('Event invokation benchmark');
    Writeln;
    Writeln('  -t=n         Number of threads  - default: ', ThreadCount);
    Writeln('  -h=n         Number of handlers - default: ', HandlerCount);
    Writeln('  -c=n         Invokes per thread - default: ', EventCallCount);
    Exit;
  end;

  params := TStringList.Create;
  try
    for i := 1 to ParamCount do
      params.Add(ParamStr(i));
    ThreadCount := StrToIntDef(params.Values['-t'], ThreadCount);
    HandlerCount := StrToIntDef(params.Values['-h'], HandlerCount);
    EventCallCount := StrToIntDef(params.Values['-c'], EventCallCount);
  finally
    params.Free;
  end;

  Writeln('Number of threads:       ', ThreadCount);
  Writeln('Handlers per event:      ', HandlerCount);
  Writeln('Calls per thread:        ', EventCallCount);
  Writeln;

  t := TEventHandler.Create;
  for i := 1 to HandlerCount do
  begin
    e1.Add(t.HandleEvent1);
    e2.Add(t.HandleEvent2);
  end;

  TotalCallCount := 0;
  Writeln('Benchmark running...');
  TotalDuration := Measure(
    procedure(index: Integer)
    var
      i: Integer;
    begin
      for i := 1 to EventCallCount do
      begin
//        if i mod (EventCallCount div 20) = 0 then
//          e1.Add(t.HandleEvent1);
//        if i mod ((EventCallCount div 20)+5) = 0 then
//          e1.Remove(t.HandleEvent1);
        e1.Invoke(t);
//        e2.Invoke(t, 111, 222, 333, 444, 555);
      end;

      AtomicIncrement(TotalCallCount, CallCount);
    end);

  Writeln('Event invokes/ms:        ', EventCallCount * ThreadCount div TotalDuration);
  Writeln('Handler calls/ms:        ', TotalCallCount div TotalDuration);
  Writeln('Handler calls/ms/thread: ', TotalCallCount div TotalDuration div ThreadCount);
  Writeln('Total duration in ms:    ', TotalDuration);
  Writeln('Total handler calls:     ', TotalCallCount);
  e1.Clear;
  e2.Clear;
  t.Free;
end;

begin
  Main;
  Writeln;
  ReportMemoryLeaksOnShutdown := True;
end.
