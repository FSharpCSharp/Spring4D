{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Reactive.Tests.CurrentThreadScheduler;

interface

uses
  TestFramework,
  Spring,
  Spring.Reactive.Concurrency.Scheduler;

type
  CurrentThreadSchedulerTest = class(TTestCase)
  private
    class procedure EnsureTrampoline(const scheduler: ICurrentThreadScheduler; const action: Action); static;
  published
    procedure CurrentThread_Now;
    procedure CurrentThread_ScheduleAction;
    procedure CurrentThread_ScheduleActionError;
    procedure CurrentThread_ScheduleActionNested;
    procedure CurrentThread_ScheduleActionNested_TimeSpan;
    procedure CurrentThread_ScheduleActionDue;
    procedure CurrentThread_ScheduleActionDueNested;
    procedure CurrentThread_EnsureTrampoline;
    procedure CurrentThread_EnsureTrampoline_Nested;
    procedure CurrentThread_EnsureTrampolineAndCancel;
    procedure CurrentThread_EnsureTrampolineAndCancelTimed;
  end;

implementation

uses
  DateUtils,
  SysUtils,
  Spring.Reactive;


{$REGION 'CurrentThreadSchedulerTest'}

class procedure CurrentThreadSchedulerTest.EnsureTrampoline(
  const scheduler: ICurrentThreadScheduler; const action: Action);
begin
  if scheduler.ScheduleRequired then
    scheduler.Schedule(action)
  else
    action();
end;

procedure CurrentThreadSchedulerTest.CurrentThread_Now;
var
  res: TDateTime;
begin
  res := TScheduler.CurrentThread.Now - Now;
  CheckTrue(SecondOf(res) < 1);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleAction;
var
  id: TThreadID;
  ran: Boolean;
begin
  ran := False;
  TScheduler.CurrentThread.Schedule(
    procedure
    begin
      id := TThread.CurrentThread.ThreadID;
      ran := True;
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleActionError;
var
  ex: Exception;
begin
  ex := Exception.Create('');
  try
    TScheduler.CurrentThread.Schedule(
      procedure
      begin
        raise ex;
      end);
    Check(False);
  except
    on e: Exception do
      CheckSame(e, ex);
  end;
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleActionNested;
var
  id: TThreadID;
  ran: Boolean;
begin
  ran := False;
  TScheduler.CurrentThread.Schedule(
    procedure
    begin
      id := TThread.CurrentThread.ThreadID;
      TScheduler.CurrentThread.Schedule(procedure begin ran := True end);
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleActionNested_TimeSpan;
var
  id: TThreadID;
  ran: Boolean;
begin
  ran := False;
  TScheduler.CurrentThread.Schedule(
    procedure
    begin
      id := TThread.CurrentThread.ThreadID;
      TScheduler.CurrentThread.Schedule(
        TTimeSpan.FromSeconds(1),
        procedure begin ran := True end);
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleActionDue;
var
  id: TThreadID;
  ran: Boolean;
  sw: TStopwatch;
begin
  ran := False;
  sw := TStopwatch.Create;
  sw.Start;
  TScheduler.CurrentThread.Schedule(TTimeSpan.FromSeconds(0.2),
    procedure
    begin
      sw.Stop;
      id := TThread.CurrentThread.ThreadID;
      ran := True;
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
  CheckTrue(sw.ElapsedMilliseconds > 180);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_ScheduleActionDueNested;
var
  id: TThreadID;
  ran: Boolean;
  sw: TStopwatch;
begin
  ran := False;
  sw := TStopwatch.Create;
  sw.Start;
  TScheduler.CurrentThread.Schedule(TTimeSpan.FromSeconds(0.2),
    procedure
    begin
      sw.Stop;
      id := TThread.CurrentThread.ThreadID;
      sw.Start;
      TScheduler.CurrentThread.Schedule(TTimeSpan.FromSeconds(0.2),
        procedure
        begin
          sw.Stop;
          ran := True;
        end);
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
  CheckTrue(sw.ElapsedMilliseconds > 380);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_EnsureTrampoline;
var
  ran1, ran2: Boolean;
begin
  ran1 := False;
  ran2 := False;
  EnsureTrampoline(TScheduler.CurrentThread,
    procedure
    begin
      TScheduler.CurrentThread.Schedule(procedure begin ran1 := True end);
      TScheduler.CurrentThread.Schedule(procedure begin ran2 := True end);
    end);
  CheckTrue(ran1);
  CheckTrue(ran2);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_EnsureTrampoline_Nested;
var
  ran1, ran2: Boolean;
begin
  ran1 := False;
  ran2 := False;
  EnsureTrampoline(TScheduler.CurrentThread,
    procedure
    begin
      EnsureTrampoline(TScheduler.CurrentThread, procedure begin ran1 := True end);
      EnsureTrampoline(TScheduler.CurrentThread, procedure begin ran2 := True end);
    end);
  CheckTrue(ran1);
  CheckTrue(ran2);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_EnsureTrampolineAndCancel;
var
  ran1, ran2: Boolean;
begin
  ran1 := False;
  ran2 := False;
  EnsureTrampoline(TScheduler.CurrentThread,
    procedure
    begin
      TScheduler.CurrentThread.Schedule(
        procedure
        var
          d: IDisposable;
        begin
          ran1 := True;
          d := TScheduler.CurrentThread.Schedule(procedure begin ran2 := True end);
          d.Dispose;
        end);
    end);
  CheckTrue(ran1);
  CheckFalse(ran2);
end;

procedure CurrentThreadSchedulerTest.CurrentThread_EnsureTrampolineAndCancelTimed;
var
  ran1, ran2: Boolean;
begin
  ran1 := False;
  ran2 := False;
  EnsureTrampoline(TScheduler.CurrentThread,
    procedure
    begin
      TScheduler.CurrentThread.Schedule(
        procedure
        var
          d: IDisposable;
        begin
          ran1 := True;
          d := TScheduler.CurrentThread.Schedule(TTimeSpan.FromSeconds(1), procedure begin ran2 := True end);
          d.Dispose;
        end);
    end);
  CheckTrue(ran1);
  CheckFalse(ran2);
end;

{$ENDREGION}


initialization
  RegisterTest(CurrentThreadSchedulerTest.Suite);

end.
