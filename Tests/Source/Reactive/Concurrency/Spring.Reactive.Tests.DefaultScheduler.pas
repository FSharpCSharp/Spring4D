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

unit Spring.Reactive.Tests.DefaultScheduler;

interface

uses
  TestFramework;

type
  DefaultSchedulerTest = class(TTestCase)
  published
    procedure Schedule_ArgumentChecking;
    procedure Get_Now;
    procedure ScheduleAction;
    procedure ScheduleActionDue;
    procedure ScheduleActionCancel;
    procedure Periodic_NonReentrant;
  end;

implementation

uses
  DateUtils,
  SysUtils,
  Utilities,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.DefaultScheduler,
  Spring.TestUtils;

{ DefaultSchedulerTest }

procedure DefaultSchedulerTest.Schedule_ArgumentChecking;
begin
  CheckException(EArgumentNilException, procedure begin TDefaultScheduler.Instance.Schedule(42, Func<IScheduler,TValue,IDisposable>(nil)) end);
//  CheckException(EArgumentNilException, procedure begin TDefaultScheduler.Instance.Schedule(42, TDateTimeOffset.Now, Func<IScheduler,TValue,IDisposable>(nil)) end);
  CheckException(EArgumentNilException, procedure begin TDefaultScheduler.Instance.Schedule(42, TTimeSpan.Zero, Func<IScheduler,TValue,IDisposable>(nil)) end);
  CheckException(EArgumentNilException, procedure begin TDefaultScheduler.Instance.SchedulePeriodic(42, TTimeSpan.FromSeconds(1), nil) end);
  CheckException(EArgumentOutOfRangeException, procedure begin TDefaultScheduler.Instance.SchedulePeriodic(42, TTimeSpan.FromSeconds(-1), function(const _: TValue): TValue begin Result := _ end) end);
end;

procedure DefaultSchedulerTest.Get_Now;
begin
  CheckTrue(SecondsBetween(TDefaultScheduler.Instance.Now, Now) < 1);
end;

procedure DefaultSchedulerTest.ScheduleAction;
var
  id: TThreadID;
  nt: IScheduler;
  evt: TManualResetEvent;
begin
  nt := TDefaultScheduler.Instance;
  evt := TManualResetEvent.Create(False);
  try
    nt.Schedule(
      procedure
      begin
        id := TThread.Current.ThreadID;
        evt.SetEvent;
      end);
    evt.WaitFor;
    CheckNotEquals(TThread.Current.ThreadID, id);
  finally
    evt.Free;
  end
end;

procedure DefaultSchedulerTest.ScheduleActionDue;
var
  id: TThreadID;
  nt: IScheduler;
  evt: TManualResetEvent;
begin
  nt := TDefaultScheduler.Instance;
  evt := TManualResetEvent.Create(False);
  try
    nt.Schedule(TTimeSpan.FromSeconds(0.2),
      procedure
      begin
        id := TThread.Current.ThreadID;
        evt.SetEvent;
      end);
    evt.WaitFor;
    CheckNotEquals(TThread.Current.ThreadID, id);
  finally
    evt.Free;
  end;
end;

procedure DefaultSchedulerTest.ScheduleActionCancel;
var
  nt: IScheduler;
  b: Boolean;
  d: IDisposable;
begin
  nt := TDefaultScheduler.Instance;
  b := False;
  d := nt.Schedule(TTimeSpan.FromSeconds(0.2),
    procedure
    begin
//      CheckTrue(False); // has no effect, PPL does not promote exception to caller of task
      b := True;
    end);
  d.Dispose;
  TThread.Sleep(400);
  CheckFalse(b);
end;

procedure DefaultSchedulerTest.Periodic_NonReentrant;
var
  n: Integer;
  fail: Boolean;
  d: IDisposable;
begin
  n := 0;
  fail := False;

  d := TDefaultScheduler.Instance.SchedulePeriodic(0, TTimeSpan.FromMilliseconds(50),
    function(const x: TValue): TValue
    begin
      try
        if AtomicIncrement(n) > 1 then
          fail := True;
        TThread.Sleep(100);

        Result := x.AsInteger + 1;
      finally
        AtomicDecrement(n);
      end;
    end);

  TThread.Sleep(500);
  d.Dispose;

  CheckFalse(fail);
end;

initialization
  RegisterTest(DefaultSchedulerTest.Suite);

end.
