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

unit Spring.Reactive.Concurrency.DefaultScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.LocalScheduler,
  Spring.Reactive.Concurrency.Scheduler;

type
  TDefaultScheduler = class(TLocalScheduler, IDefaultScheduler, ISchedulerPeriodic)
  private
    class var fInstance: IDefaultScheduler;
    class var fCAL: IConcurrencyAbstractionLayer;
  public
    class constructor Create;
    class destructor Destroy;

    // IScheduler
    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    // ISchedulerPeriodic
    function SchedulePeriodic(const period: TTimeSpan;
      const action: Action): IDisposable; overload;
    function SchedulePeriodic(const state: TValue; const period: TTimeSpan;
      const action: Func<TValue, TValue>): IDisposable; overload;

    class property Instance: IDefaultScheduler read fInstance;
  end;

implementation

uses
  SyncObjs,
  SysUtils,
{$IFDEF DELPHIXE7_UP}
  Classes,
  Threading,
{$ENDIF}
  Spring.Reactive.Concurrency.ConcurrencyAbstractionLayer,
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Stubs;


{$REGION 'TDefaultScheduler'}

class constructor TDefaultScheduler.Create;
begin
  fInstance := TDefaultScheduler.Create;
  fCAL := TConcurrencyAbstractionLayer.Create; // TODO move this somewhere else to make it pluggable
end;

class destructor TDefaultScheduler.Destroy;
begin
  fInstance := nil;
end;

function TDefaultScheduler.SchedulePeriodic(const period: TTimeSpan;
  const action: Action): IDisposable;
begin
  Result := SchedulePeriodic(TValue.From<Spring.Action>(action), period,
    function(const a: TValue): TValue
    begin
      a.AsType<Spring.Action>()();
      Result := a;
    end);
end;

function TDefaultScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  _state: TValue;
  d: ISingleAssignmentDisposable;
  cancel: IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  _state := state;
  d := TSingleAssignmentDisposable.Create;

  cancel := fCAL.QueueUserWorkItem(
    procedure(const _: TValue)
    begin
      if not d.IsDisposed then
        d.Disposable := action(Self, _state);
    end, TValue.Empty);

  Result := TStableCompositeDisposable.Create(d, cancel);
end;

function TDefaultScheduler.Schedule(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  _state: TValue;
  dt: TTimeSpan;
  d: ISingleAssignmentDisposable;
  cancel: IDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  _state := state;
  dt := Normalize(dueTime);
  if dt.Ticks = 0 then
    Exit(Schedule(state, action));

  d := TSingleAssignmentDisposable.Create;

  cancel := fCAL.StartTimer(
    procedure(const _: TValue)
    var
      res: IDisposable;
    begin
      if not d.IsDisposed then
      begin
        res := action(Self, _state);
        if Assigned(d) then // d can become nil during call of action
          d.Disposable := res;
      end;
    end, TValue.Empty, dt);

  Result := TStableCompositeDisposable.Create(d, cancel);
end;

function TDefaultScheduler.SchedulePeriodic(const state: TValue;
  const period: TTimeSpan; const action: Func<TValue, TValue>): IDisposable;
var
  state1: TValue;
  cancel: IDisposable;
begin
  Guard.CheckRange(period >= TTimeSpan.Zero, 'period');
  Guard.CheckNotNull(Assigned(action), 'action');

  // guard

  state1 := state;
  cancel := fCAL.StartPeriodicTimer(
    procedure
    begin
      state1 := action(state1);
    end, period);

  Result := Disposable.Create(
    procedure
    begin
      cancel.Dispose;
    end);
end;

{$ENDREGION}


end.
