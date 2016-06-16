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
  Spring.Reactive.Concurrency.LocalScheduler;

type
  TDefaultScheduler = class(TLocalScheduler, ISchedulerPeriodic)
  private
    class var fInstance: ISchedulerPeriodic;

    // TODO move to ConcurrencyAbstractionLayerImpl
    class function StartTimer(const action: Action<TValue>; const state: TValue;
      const dueTime: TTimeSpan): IDisposable; static;
    class function QueueUserWorkItem(const action: Action<TValue>;
      const state: TValue): IDisposable; static;
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

    class property Instance: ISchedulerPeriodic read fInstance;
  end;

implementation

uses
  SyncObjs,
  SysUtils,
{$IFDEF DELPHIXE7_UP}
  Classes,
  Threading,
{$ENDIF}
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Stubs;


{$REGION 'TDefaultScheduler'}

class constructor TDefaultScheduler.Create;
begin
  fInstance := TDefaultScheduler.Create;
end;

class destructor TDefaultScheduler.Destroy;
begin
  fInstance := nil;
end;

function TDefaultScheduler.SchedulePeriodic(const period: TTimeSpan;
  const action: Action): IDisposable;
var
  _period: TTimeSpan;
  isDisposed: Boolean;
begin
  _period := period;
{$IFDEF DELPHIXE7_UP}
  isDisposed := False;
  TTask.Run(
    procedure
    begin
      while not isDisposed do
      begin
        // naive impl without canceling
        TThread.Sleep(_period);

        if TThread.Current.Terminated then
          Exit;

        if not isDisposed then
          TThread.Synchronize(nil, TThreadProcedure(action));
      end;
    end);
{$ELSE}
{$ENDIF}
  Result := Disposable.Create(
    procedure
    begin
      isDisposed := True;
    end);
end;

// TODO move into own unit
type
  TTimer = class(TDisposableObject)
  private
    fAction: Action<TValue>;
    fState: TValue;
    fTimer: TThread;
    fEvent: TEvent;
  public
    constructor Create(const action: Action<TValue>; const state: TValue;
      const dueTime: TTimeSpan);
    destructor Destroy; override;
    procedure Dispose; override;
  end;

  TPeriodicTimer = class(TDisposableObject)
  private
    fAction: Action;
    fTimer: TThread;
  public
    constructor Create(const action: Action; const period: TTimeSpan);
    destructor Destroy; override;
    procedure Dispose; override;
  end;

{ TTimer }

constructor TTimer.Create(const action: Action<TValue>; const state: TValue;
  const dueTime: TTimeSpan);
var
  _dueTime: TTimeSpan;
  caller: TThread;
begin
  inherited Create;
  fAction := action;
  fState := state;
  _dueTime := dueTime;
  fEvent := TEvent.Create(nil, True, False, '');
  caller := TThread.CurrentThread;

  // TODO: implement this using a timer - actions need to occur on the same thread that caused them
  fTimer :=
    TThread.CreateAnonymousThread(
      procedure
      begin
        if fEvent.WaitFor(_dueTime) = wrTimeout then
          if Assigned(fAction) then
            TThread.Synchronize(caller, procedure begin fAction(fState); end);
      end);
  fTimer.FreeOnTerminate := False;
  fTimer.Start;
end;

destructor TTimer.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TTimer.Dispose;
begin
  if Assigned(fEvent) then
  begin
    fEvent.SetEvent;
    fAction := nil;//Stubs.Nop;
//    fTimer.Terminate;
    FreeAndNil(fTimer);
    fEvent.Free;
    fEvent := nil;
  end;
  inherited Dispose;
end;

{ TPeriodicTimer }

constructor TPeriodicTimer.Create(const action: Action;
  const period: TTimeSpan);
var
  _period: TTimeSpan;
  caller: TThread;
begin
  inherited Create;
  fAction := action;
  _period := period;
  caller := TThread.CurrentThread;
  fTimer :=
    TThread.CreateAnonymousThread(
      procedure
      begin
        while Assigned(fAction) do
        begin
          TThread.Sleep(_period);
          if Assigned(fAction) then
            TThread.Synchronize(caller, procedure begin fAction(); end);
        end;
      end);
  fTimer.FreeOnTerminate := False;
  fTimer.Start;
end;

destructor TPeriodicTimer.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TPeriodicTimer.Dispose;
begin
  if Assigned(fTimer) then
  begin
    fAction := nil;//Stubs.Nop;
    FreeAndNil(fTimer);
  end;
  inherited Dispose;
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

  cancel := QueueUserWorkItem(
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

  cancel := StartTimer(
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
  // guard

  state1 := state;
  cancel := TPeriodicTimer.Create(
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

class function TDefaultScheduler.StartTimer(const action: Action<TValue>;
  const state: TValue; const dueTime: TTimeSpan): IDisposable;
begin
  Result := TTimer.Create(action, state, Normalize(dueTime));
end;

class function TDefaultScheduler.QueueUserWorkItem(const action: Action<TValue>;
  const state: TValue): IDisposable;
var
  _state: TValue;
begin
  _state := state;
{$IFDEF DELPHIXE7_UP}
  TThreadPool.Default.QueueWorkItem(
    procedure
    begin
      action(_state);
    end);
{$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      action(_state);
    end).Start;
{$ENDIF}
  Result := Disposable.Empty;
end;


{$ENDREGION}


end.
