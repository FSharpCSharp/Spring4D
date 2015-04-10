program Demo.Spring.Coroutines.Timer;

{$APPTYPE CONSOLE}

uses
  Diagnostics,
  SysUtils,
  Windows,
  Spring.Collections,
  Spring.Coroutines,
  Spring.Collections.Sequences;

procedure Countdown(interval: Integer);
var
  start: Cardinal;
begin
  start := GetTickCount;
  while GetTickCount < start + interval * 1000 do
    Yield;
  Writeln(Format('This message appears after %d seconds!', [interval]));
end;

function KeyPressed(expectedKey: Word):Boolean;
var
  hConsoleInput: THandle;
  lpNumberOfEvents: DWORD;
  lpBuffer: TInputRecord;
  lpNumberOfEventsRead: DWORD;
begin
  Result := False;
  hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(hConsoleInput, lpNumberOfEvents);
  if lpNumberOfEvents <> 0 then
  begin
    PeekConsoleInput(hConsoleInput, lpBuffer, 1, lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
      if lpBuffer.EventType = KEY_EVENT then
        if lpBuffer.Event.KeyEvent.bKeyDown
          and ((expectedKey = 0) or (lpBuffer.Event.KeyEvent.wVirtualKeyCode = expectedKey)) then
          Result := true
        else
          FlushConsoleInputBuffer(hConsoleInput)
      else
        FlushConsoleInputBuffer(hConsoleInput);
  end;
end;

procedure Main;
var
  timers: array[1..2] of TProc;
  timer: TProc;
begin
  timers[1] := TCoroutine.Create(procedure begin Countdown(3) end);
  timers[2] := TCoroutine.Create(procedure begin Countdown(5) end);
  while not KeyPressed(VK_RETURN) do
  begin
    Sleep(1);
    for timer in timers do
      timer;
  end;
end;

begin
  try
    Randomize;
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
//  Readln;
  ReportMemoryLeaksOnShutdown := True;
end.
