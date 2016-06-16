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

unit Spring.Reactive.Tests.Scheduler;

interface

uses
  TestFramework,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.Scheduler,
  Spring.Reactive.Testing.ReactiveTest;

type
  SchedulerTest = class(TReactiveTest)
  published
    procedure Scheduler_ScheduleNonRecursive;
    procedure Scheduler_ScheduleRecursive;
//    procedure Scheduler_ScheduleWithTimeNonRecursive;
//    procedure Scheduler_ScheduleWithTimeRecursive;
    procedure Scheduler_ScheduleWithTimeSpanNonRecursive;
    procedure Scheduler_ScheduleWithTimeSpanRecursive;
  end;

  IMyScheduler = interface(IScheduler)
    function GetCheck: Action<Action<TValue>, TValue, TTimeSpan>;
    procedure SetCheck(const value: Action<Action<TValue>, TValue, TTimeSpan>);
    function GetWaitCycles: Int64;
    property Check: Action<Action<TValue>, TValue, TTimeSpan> read GetCheck write SetCheck;
    property WaitCycles: Int64 read GetWaitCycles;
  end;

  TMyScheduler = class(TScheduler, IScheduler, IMyScheduler)
  private
    fNow: TDateTime;
    fCheck: Action<Action<TValue>, TValue, TTimeSpan>;
    fWaitCycles: Int64;
    function GetNow: TDateTime;
    function GetCheck: Action<Action<TValue>, TValue, TTimeSpan>;
    procedure SetCheck(const value: Action<Action<TValue>, TValue, TTimeSpan>);
    function GetWaitCycles: Int64;
  public
    constructor Create; overload;
    constructor Create(const now: TDateTime); overload;

    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
//    function Schedule(const state: TValue; const dueTime: TDateTime;
//      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    property Now: TDateTime read fNow;
    property Check: Action<Action<TValue>, TValue, TTimeSpan> read GetCheck write SetCheck;
  end;

implementation

uses
  SysUtils,
  Spring.Collections;


{$REGION 'SchedulerTest'}

procedure SchedulerTest.Scheduler_ScheduleNonRecursive;
var
  ms: IScheduler;
  res: Boolean;
begin
  ms := TMyScheduler.Create;
  res := False;
  ms.Schedule(procedure(const a: Action) begin res := True end);
  CheckTrue(res);
end;

procedure SchedulerTest.Scheduler_ScheduleRecursive;
var
  ms: IScheduler;
  i: Integer;
begin
  ms := TMyScheduler.Create;
  i := 0;
  ms.Schedule(procedure(const a: Action) begin Inc(i); if i < 10 then a() end);
  CheckEquals(10, i);
end;

//procedure SchedulerTest.Scheduler_ScheduleWithTimeNonRecursive;
//var
//  now: TDateTime;
//  ms: IMyScheduler;
//  res: Boolean;
//begin
//  now := SysUtils.Now;
//  ms := TMyScheduler.Create(now);
//  ms.Check :=
//    procedure(const a: Action<TValue>; const s: TValue; const t: TTimeSpan)
//    begin
//      CheckTrue(t = TTimeSpan.Zero);
//    end;
//  res := False;
//  ms.Schedule(now, procedure(const a: Action<TTimeSpan>) begin res := True; end);
//  CheckTrue(res);
//  CheckTrue(ms.WaitCycles = 0);
//end;

procedure SchedulerTest.Scheduler_ScheduleWithTimeSpanNonRecursive;
var
  now: TDateTime;
  ms: IMyScheduler;
  res: Boolean;
begin
  now := SysUtils.Now;
  ms := TMyScheduler.Create(now);
  ms.Check :=
    procedure(const a: Action<TValue>; const s: TValue; const t: TTimeSpan)
    begin
      CheckTrue(t = TTimeSpan.Zero);
    end;
  res := False;
  ms.Schedule(TTimeSpan.Zero, procedure(const a: Action<TTimeSpan>) begin res := True end);
  CheckTrue(res);
  CheckTrue(ms.WaitCycles = 0);
end;

procedure SchedulerTest.Scheduler_ScheduleWithTimeSpanRecursive;
var
  now: TDateTime;
  ms: IMyScheduler;
  i: Integer;
begin
  now := SysUtils.Now;
  ms := TMyScheduler.Create(now);
  ms.Check :=
    procedure(const a: Action<TValue>; const s: TValue; const t: TTimeSpan)
    begin
      CheckTrue(t < TTimeSpan.FromTicks(10));
    end;
  i := 0;
  ms.Schedule(TTimeSpan.Zero, procedure(const a: Action<TTimeSpan>) begin Inc(i); if i < 10 then a(TTimeSpan.FromTicks(i)) end);
  CheckTrue(ms.WaitCycles = TEnumerable.Range(1, 9).Sum);
  CheckEquals(10, i);
end;

{$ENDREGION}


{$REGION 'TMyScheduler'}

constructor TMyScheduler.Create;
begin
  Create(SysUtils.Now)
end;

constructor TMyScheduler.Create(const now: TDateTime);
begin
  inherited Create;
  fNow := now;
end;

function TMyScheduler.GetNow: TDateTime;
begin
  Result := fNow;
end;

function TMyScheduler.GetWaitCycles: Int64;
begin
  Result := fWaitCycles;
end;

function TMyScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  Result := action(Self, state);
end;

function TMyScheduler.GetCheck: Action<Action<TValue>, TValue, TTimeSpan>;
begin
  Result := fCheck
end;

procedure TMyScheduler.SetCheck(
  const value: Action<Action<TValue>, TValue, TTimeSpan>);
begin
  fCheck := value;
end;

function TMyScheduler.Schedule(const state: TValue; const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  Check(procedure(const o: TValue) begin action(Self, o) end, state, dueTime);
  Inc(fWaitCycles, dueTime.Ticks);
  Result := action(Self, state);
end;

{$ENDREGION}


initialization
  RegisterTest(SchedulerTest.Suite);

end.
