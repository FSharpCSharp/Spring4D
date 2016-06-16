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

unit Spring.Reactive.Concurrency.Scheduler;

interface

uses
  Spring,
  Spring.Reactive;

type
  ICurrentThreadScheduler = interface(IScheduler)
    function GetScheduleRequired: Boolean;
    property ScheduleRequired: Boolean read GetScheduleRequired;
  end;

  IImmediateScheduler = interface(IScheduler)

  end;

  TScheduler = class(TInterfacedObject)
  private
    class function Invoke(const scheduler: IScheduler; const action: TValue): IDisposable; static;
    class function InvokeRec1(const scheduler: IScheduler; const pair: TValue): IDisposable; static;
    class function InvokeRec2(const scheduler: IScheduler; const pair: TValue): IDisposable; static;
    class function GetCurrentThread: ICurrentThreadScheduler; static;
    class function GetImmediate: IImmediateScheduler; static;
    class function GetNow: TDateTime; static;

    type
      TPair<T1,T2> = record
        First: T1;
        Second: T2;
      end;
  public
    // static methods from Scheduler.cs
    class function Normalize(const timeSpan: TTimeSpan): TTimeSpan; static;

    // IScheduler
    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; overload; virtual; abstract;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; overload; virtual; abstract;
    function Schedule(const state: TValue; const dueTime: TDateTime;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; overload; virtual; abstract;

    // extension methods from Scheduler.Simple.cs
    function Schedule(const action: Action): IDisposable; overload;
    function Schedule(const dueTime: TTimeSpan; const action: Action): IDisposable; overload;

    // extension methods from Scheduler.Recursive.cs
    function Schedule(const action: Action<Action>): IDisposable; overload;
    function Schedule(const state: TValue;
      const action: Action<TValue, Action<TValue>>): IDisposable; overload;
    function Schedule(const dueTime: TTimeSpan;
      const action: Action<Action<TTimeSpan>>): IDisposable; overload;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Action<TValue, Action<TValue, TTimeSpan>>): IDisposable; overload;

    class property CurrentThread: ICurrentThreadScheduler read GetCurrentThread;
    class property Immediate: IImmediateScheduler read GetImmediate;
    class property Now: TDateTime read GetNow;
  end;

implementation

uses
  SysUtils,
  Spring.Reactive.Concurrency.CurrentThreadScheduler,
  Spring.Reactive.Concurrency.ImmediateScheduler,
  Spring.Reactive.Disposables;


{$REGION 'TScheduler'}

class function TScheduler.GetCurrentThread: ICurrentThreadScheduler;
begin
  Result := TCurrentThreadScheduler.Instance;
end;

class function TScheduler.GetImmediate: IImmediateScheduler;
begin
  Result := TImmediateScheduler.Instance;
end;

function TScheduler.Schedule(const action: Action): IDisposable;
begin
  Result := Schedule(TValue.From<Spring.Action>(action), Invoke);
end;

function TScheduler.Schedule(const dueTime: TTimeSpan;
  const action: Action): IDisposable;
begin
  Result := Schedule(TValue.From<Spring.Action>(action), dueTime, Invoke);
end;

class function TScheduler.GetNow: TDateTime;
begin
  Result := SysUtils.Now;
end;

class function TScheduler.Invoke(const scheduler: IScheduler;
  const action: TValue): IDisposable;
begin
  action.AsType<Spring.Action>()(); // TODO: review - consider internal generic method to be used
  Result := Disposable.Empty;
end;

function TScheduler.Schedule(const action: Action<Action>): IDisposable;
begin
  Result := Schedule(TValue.From<Action<Spring.Action>>(action),
    procedure(const _action: TValue; const _self: Action<TValue>)
    var
      a: TValue;
    begin
      a := _action;
      _action.AsType<Action<Spring.Action>>()(
        procedure
        begin
          _self(a);
        end);
    end);
// return scheduler.Schedule(action, (_action, self) => _action(() => self(_action)));
end;

function TScheduler.Schedule(const state: TValue;
  const action: Action<TValue, Action<TValue>>): IDisposable;
var
  pair: TPair<TValue,Action<TValue, Action<TValue>>>;
begin
  pair.First := state;
  pair.Second := action;
  Result := Schedule(TValue.From(pair), InvokeRec1);
// return scheduler.Schedule(new Pair<TState, Action<TState, Action<TState>>> { First = state, Second = action }, InvokeRec1);
end;

class function TScheduler.InvokeRec1(const scheduler: IScheduler; const pair: TValue): IDisposable;
var
  group: ICompositeDisposable;
  gate: IInterface;
  _pair: TPair<TValue,Action<TValue, Action<TValue>>>;
  state: TValue;
  action: Action<TValue, Action<TValue>>;
  recursiveAction: Action<TValue>;
  _recursiveAction: Pointer;
begin
  group := TCompositeDisposable.Create([]);
  gate := TInterfacedObject.Create; // TODO: review
  _pair := pair.AsType<TPair<TValue,Action<TValue, Action<TValue>>>>;
  state := _pair.First;
  action := _pair.Second;

  recursiveAction :=
    procedure(const state1: TValue)
    begin
      action(state1,
        procedure(const state2: TValue)
        var
          isAdded: Boolean;
          isDone: Boolean;
          d: IDisposable;
        begin
          isAdded := False;
          isDone := False;

          d := scheduler.Schedule(state2,
            function(const scheduler1: IScheduler; const state3: TValue): IDisposable
            begin
              MonitorEnter(gate as TObject);
              try
                if isAdded then
                  group.Remove(d)
                else
                  isDone := True;
              finally
                MonitorExit(gate as TObject);
              end;

              Action<TValue>(_recursiveAction)(state3); // recursiveAction(state3);
              Result := Disposable.Empty;
            end);

          MonitorEnter(gate as TObject);
          try
            if not isDone then
            begin
              group.Add(d);
              isAdded := True;
            end;
          finally
            MonitorExit(gate as TObject);
          end;
        end);
    end;

   // avoid circular reference because of capturing itself
   // recursiveAction will be kept alive by its sub delegates that keep a reference to it
  _recursiveAction := PPointer(@recursiveAction)^;

  recursiveAction(state);
  Result := group;
end;

function TScheduler.Schedule(const dueTime: TTimeSpan;
  const action: Action<Action<TTimeSpan>>): IDisposable;
begin
  Result := Schedule(TValue.From<Action<Action<TTimeSpan>>>(action), dueTime,
    procedure(const _action: TValue; const _self: Action<TValue, TTimeSpan>)
    var
      a: TValue;
    begin
      a := _action;
      _action.AsType<Action<Action<TTimeSpan>>>()(
        procedure(const dt: TTimeSpan)
        begin
          _self(a, dt);
        end);
    end);
//  return scheduler.Schedule(action, dueTime, (_action, self) => _action(dt => self(_action, dt)));
end;

function TScheduler.Schedule(const state: TValue; const dueTime: TTimeSpan;
  const action: Action<TValue, Action<TValue, TTimeSpan>>): IDisposable;
var
  pair: TPair<TValue, Action<TValue, Action<TValue, TTimeSpan>>>;
begin
  pair.First := state;
  pair.Second := action;
  Result := Schedule(TValue.From(pair), dueTime, InvokeRec2);
//  return scheduler.Schedule(new Pair<TState, Action<TState, Action<TState, TimeSpan>>> { First = state, Second = action }, dueTime, InvokeRec2);
end;

class function TScheduler.InvokeRec2(const scheduler: IScheduler;
  const pair: TValue): IDisposable;
var
  _pair: TPair<TValue, Action<TValue, Action<TValue, TTimeSpan>>>;
  group: ICompositeDisposable;
  gate: IInterface;
  state: TValue;
  action: Action<TValue, Action<TValue, TTimeSpan>>;
  recursiveAction: Action<TValue>;
  _recursiveAction: Pointer;
begin
  _pair := pair.AsType<TPair<TValue, Action<TValue, Action<TValue, TTimeSpan>>>>;
  group := TCompositeDisposable.Create([]); // TODO: capacity overload?
  gate := TInterfacedObject.Create; // TODO: review
  state := _pair.First;
  action := _pair.Second;

  recursiveAction :=
    procedure(const state1: TValue)
    begin
      action(state1,
        procedure(const state2: TValue; const dueTime1: TTimeSpan)
        var
          isAdded: Boolean;
          isDone: Boolean;
          d: IDisposable;
        begin
          d := scheduler.Schedule(state2, dueTime1,
            function(const scheduler: IScheduler; const state3: TValue): IDisposable
            begin
              MonitorEnter(gate as TObject);
              try
                if isAdded then
                  group.Remove(d)
                else
                  isDone := True;
              finally
                MonitorExit(gate as TObject);
              end;
              Action<TValue>(_recursiveAction)(state3);
              Result := Disposable.Empty;
            end);

          MonitorEnter(gate as TObject);
          try
            if not isDone then
            begin
              group.Add(d);
              isAdded := True;
            end;
          finally
            MonitorExit(gate as TObject);
          end;
        end);
    end;

  _recursiveAction := PPointer(@recursiveAction)^;

  recursiveAction(state);
  Result := group;
end;

class function TScheduler.Normalize(const timeSpan: TTimeSpan): TTimeSpan;
begin
  if timeSpan.Ticks < 0 then
    Result := TTimeSpan.Zero
  else
    Result := timeSpan;
end;

{$ENDREGION}


end.
