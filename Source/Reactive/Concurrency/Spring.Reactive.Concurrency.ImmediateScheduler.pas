{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Reactive.Concurrency.ImmediateScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.AsyncLock,
  Spring.Reactive.Concurrency.LocalScheduler,
  Spring.Reactive.Concurrency.Scheduler;

type
  TImmediateScheduler = class(TLocalScheduler, IImmediateScheduler)
  private
    class var fInstance: IImmediateScheduler;
  public
    class constructor Create;
    class destructor Destroy;

    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    class property Instance: IImmediateScheduler read fInstance;

    type
      TAsyncLockScheduler = class(TLocalScheduler)
      private
        fAsyncLock: TAsyncLock;
      public
        destructor Destroy; override;

        function Schedule(const state: TValue;
          const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
        function Schedule(const state: TValue; const dueTime: TTimeSpan;
          const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
      end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TImmediateScheduler'}

class constructor TImmediateScheduler.Create;
begin
  fInstance := TImmediateScheduler.Create;
end;

class destructor TImmediateScheduler.Destroy;
begin
  fInstance := nil;
end;

function TImmediateScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  asyncLockScheduler: IScheduler;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  asyncLockScheduler := TAsyncLockScheduler.Create;
  Result := action(asyncLockScheduler, state);
end;

function TImmediateScheduler.Schedule(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  dt: TTimeSpan;
  asyncLockScheduler: IScheduler;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  dt := TScheduler.Normalize(dueTime);
  if dt.Ticks > 0 then
    TThread.Sleep(dt);
//    ConcurrentAbstractLayer.Current.Sleep;

  asyncLockScheduler := TAsyncLockScheduler.Create;
  Result := action(asyncLockScheduler, state);
end;

{$ENDREGION}


{$REGION 'TImmediateScheduler.TAsyncLockScheduler'}

destructor TImmediateScheduler.TAsyncLockScheduler.Destroy;
begin
  fAsyncLock.Free;
  inherited;
end;

function TImmediateScheduler.TAsyncLockScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  _state: TValue;
  m: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  m := TSingleAssignmentDisposable.Create;

  if fAsyncLock = nil then
    fAsyncLock := TAsyncLock.Create;

  _state := state;
  fAsyncLock.Wait(
    procedure
    begin
      if not m.IsDisposed then
        m.Disposable := action(Self, _state);
    end);

  Result := m;
end;

function TImmediateScheduler.TAsyncLockScheduler.Schedule(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  _state: TValue;
  _dueTime: TTimeSpan;
  timer: TStopwatch;
  m: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  if dueTime.Ticks <= 0 then
    Exit(Schedule(state, action));

  timer := TStopwatch.Create;

  m := TSingleAssignmentDisposable.Create;

  if fAsyncLock = nil then
    fAsyncLock := TAsyncLock.Create;

  _state := state;
  _dueTime := dueTime;
  fAsyncLock.Wait(
    procedure
    var
      sleep: TTimeSpan;
    begin
      if not m.IsDisposed then
      begin
        sleep := _dueTime - timer.Elapsed;
        if sleep.Ticks > 0 then
          TThread.Sleep(sleep);
        if not m.IsDisposed then
          m.Disposable := action(Self, _state);
      end;
    end);

  Result := m;
end;

{$ENDREGION}


end.
