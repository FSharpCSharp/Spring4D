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

unit Spring.Reactive.Concurrency.VirtualTimeScheduler;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.ScheduledItem,
  Spring.Reactive.Concurrency.Scheduler,
  Spring.Reactive.Concurrency.SchedulerQueue;

type
  TVirtualTimeSchedulerBase<TAbsolute, TRelative> = class abstract(TScheduler, IScheduler{, IStopwatchProvider})
  private
    fClock: TAbsolute;
    fComparer: IComparer<TAbsolute>;
    fIsEnabled: Boolean;
    function GetNow: TDateTime;
  protected
    constructor Create; overload;
    constructor Create(const initialClock: TAbsolute; const comparer: IComparer<TAbsolute>); overload;

    function Add(const absolute: TAbsolute; const relative: TRelative): TAbsolute; virtual; abstract;
    function ToDateTimeOffset(const absolute: TAbsolute): TDateTime; virtual; abstract;
    function ToRelative(const timeSpan: TTimeSpan): TRelative; virtual; abstract;

    function ScheduleAbsolute(const state: TValue; const dueTime: TAbsolute; const action: Func<IScheduler, TValue, IDisposable>): IDisposable; virtual; abstract;

    function GetNext: IScheduledItem<TAbsolute>; virtual; abstract;
    property Comparer: IComparer<TAbsolute> read fComparer;
  public
    function ScheduleRelative(const state: TValue; const dueTime: TRelative; const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
    function Schedule(const state: TValue; const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
    function Schedule(const state: TValue; const dueTime: TTimeSpan; const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    procedure Start;
    procedure Stop;
    procedure AdvanceTo(const time: TAbsolute);
    procedure AdvanceBy(const time: TRelative);
    procedure Sleep(const time: TRelative);

    property Clock: TAbsolute read fClock;
    property IsEnabled: Boolean read fIsEnabled;
  end;

  TVirtualTimeScheduler<TAbsolute, TRelative> = class(TVirtualTimeSchedulerBase<TAbsolute, TRelative>)
  private
    fQueue: TSchedulerQueue<TAbsolute>;
    class function Invoke(const scheduler: IScheduler; const action: TValue): IDisposable; static;
  protected
    function GetNext: IScheduledItem<TAbsolute>; override;
  public
    constructor Create; overload;
    constructor Create(const initialClock: TAbsolute; const comparer: IComparer<TAbsolute>); overload;
    destructor Destroy; override;

    function ScheduleAbsolute(const state: TValue; const dueTime: TAbsolute; const action: Func<IScheduler, TValue, IDisposable>): IDisposable; overload; override;

    // extension methods from VirtualTimeScheduler.Extensions.cs
    function ScheduleRelative(const dueTime: TRelative; const action: Action): IDisposable; overload;
    function ScheduleAbsolute(const dueTime: TAbsolute; const action: Action): IDisposable; reintroduce; overload;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TVirtualTimeSchedulerBase<TAbsolute, TRelative>'}

constructor TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Create;
begin
  Create(System.Default(TAbsolute), TComparer<TAbsolute>.Default);
end;

constructor TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Create(
  const initialClock: TAbsolute; const comparer: IComparer<TAbsolute>);
begin
  Guard.CheckNotNull(comparer, 'comparer');

  fClock := initialClock;
  fComparer := comparer;
end;

function TVirtualTimeSchedulerBase<TAbsolute, TRelative>.ScheduleRelative(
  const state: TValue; const dueTime: TRelative;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  runAt: TAbsolute;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  runAt := Add(fClock, dueTime);

  Result := ScheduleAbsolute(state, runAt, action);
end;

function TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Schedule(
  const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  Result := ScheduleAbsolute(state, fClock, action);
end;

function TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Schedule(
  const state: TValue; const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  Result := ScheduleRelative(state, ToRelative(dueTime), action);
end;

procedure TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Start;
var
  next: IScheduledItem<TAbsolute>;
begin
  if not IsEnabled then
  begin
    fIsEnabled := True;
    repeat
      next := GetNext;
      if Assigned(next) then
      begin
        if Comparer.Compare(next.DueTime, Clock) > 0 then
          fClock := next.DueTime;
        next.Invoke;
      end
      else
        fIsEnabled := False;
    until not IsEnabled;
  end;
end;

procedure TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Stop;
begin
  fIsEnabled := False;
end;

procedure TVirtualTimeSchedulerBase<TAbsolute, TRelative>.AdvanceTo(
  const time: TAbsolute);
var
  dueToClock: Integer;
  next: IScheduledItem<TAbsolute>;
begin
  dueToClock := Comparer.Compare(time, Clock);
  Guard.CheckRange(dueToClock >= 0, 'time');
  if dueToClock = 0 then
    Exit;

  if not fIsEnabled then
  begin
    fIsEnabled := True;
    repeat
      next := GetNext;
      if Assigned(next) and (Comparer.Compare(next.DueTime, time) <= 0) then
      begin
        if Comparer.Compare(next.DueTime, Clock) > 0 then
          fClock := next.DueTime;
        next.Invoke;
      end
      else
        fIsEnabled := False;
    until not fIsEnabled;
    fClock := time;
  end
  else
    raise EInvalidOperationException.Create('cannot advance while running');
end;

procedure TVirtualTimeSchedulerBase<TAbsolute, TRelative>.AdvanceBy(
  const time: TRelative);
var
  dt: TAbsolute;
  dueToClock: Integer;
begin
  dt := Add(Clock, time);
  dueToClock := Comparer.Compare(dt, Clock);
  Guard.CheckRange(dueToClock >= 0, 'time');
  if dueToClock = 0 then
    Exit;

  if not IsEnabled then
    AdvanceTo(dt)
  else
    raise EInvalidOperationException.Create('cannot advance while running');
end;

procedure TVirtualTimeSchedulerBase<TAbsolute, TRelative>.Sleep(
  const time: TRelative);
var
  dt: TAbsolute;
  dueToClock: Integer;
begin
  dt := Add(Clock, time);
  dueToClock := Comparer.Compare(dt, Clock);
  Guard.CheckRange(dueToClock >= 0, 'time');

  fClock := dt;
end;

function TVirtualTimeSchedulerBase<TAbsolute, TRelative>.GetNow: TDateTime;
begin
  Result := ToDateTimeOffset(Clock);
end;

{$ENDREGION}


{$REGION 'TVirtualTimeScheduler<TAbsolute, TRelative>'}

constructor TVirtualTimeScheduler<TAbsolute, TRelative>.Create;
begin
  inherited;
  fQueue := TSchedulerQueue<TAbsolute>.Create;
end;

constructor TVirtualTimeScheduler<TAbsolute, TRelative>.Create(
  const initialClock: TAbsolute; const comparer: IComparer<TAbsolute>);
begin
  inherited Create(initialClock, comparer);
  fQueue := TSchedulerQueue<TAbsolute>.Create;
end;

destructor TVirtualTimeScheduler<TAbsolute, TRelative>.Destroy;
begin
  fQueue.Free;
  inherited;
end;

function TVirtualTimeScheduler<TAbsolute, TRelative>.GetNext: IScheduledItem<TAbsolute>;
var
  next: IScheduledItem<TAbsolute>;
begin
  MonitorEnter(fQueue);
  try
    while fQueue.Count > 0 do
    begin
      next := fQueue.Peek;
      if next.IsCanceled then
        fQueue.Dequeue
      else
        Exit(next);
    end;
  finally
    MonitorExit(fQueue);
  end;
  Result := nil;
end;

function TVirtualTimeScheduler<TAbsolute, TRelative>.ScheduleAbsolute(
  const state: TValue; const dueTime: TAbsolute;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  [Unsafe]si: IScheduledItem<TAbsolute>;
  run: Func<IScheduler, TValue, IDisposable>;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  run :=
    function(const scheduler: IScheduler; const state1: TValue): IDisposable
    begin
      MonitorEnter(fQueue);
      try
        fQueue.Remove(si);
      finally
        MonitorExit(fQueue);
      end;

      Result := action(scheduler, state1);
    end;

  si := TScheduledItem<TAbsolute, TValue>.Create(Self, state, run, dueTime, Comparer);

  MonitorEnter(fQueue);
  try
    fQueue.Enqueue(si);
  finally
    MonitorExit(fQueue);
  end;

  Result := Disposable.Create(procedure begin si.Cancel end);
end;

function TVirtualTimeScheduler<TAbsolute, TRelative>.ScheduleRelative(
  const dueTime: TRelative; const action: Action): IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  Result := scheduleRelative(TValue.From<Spring.Action>(action), dueTime, Invoke);
end;

function TVirtualTimeScheduler<TAbsolute, TRelative>.ScheduleAbsolute(
  const dueTime: TAbsolute; const action: Action): IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  Result := scheduleAbsolute(TValue.From<Spring.Action>(action), dueTime, Invoke);
end;

class function TVirtualTimeScheduler<TAbsolute, TRelative>.Invoke(
  const scheduler: IScheduler; const action: TValue): IDisposable;
begin
  action.AsType<Spring.Action>()();
  Result := Disposable.Empty;
end;

{$ENDREGION}


end.
