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

unit Spring.Reactive.Concurrency.MainThreadScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.LocalScheduler;

type
  TMainThreadScheduler = class(TLocalScheduler{, ISchedulerPeriodic})
  private
    class var fInstance: IScheduler;
    function ScheduleSlow(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
  public
    class constructor Create;
    class property Instance: IScheduler read fInstance;

    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TMainThreadScheduler'}

class constructor TMainThreadScheduler.Create;
begin
  fInstance := TMainThreadScheduler.Create;
end;

function TMainThreadScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  _state: TValue;
  d: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  _state := state;
  d := TSingleAssignmentDisposable.Create;
  TThread.Queue(nil,
    procedure
    begin
      if not d.IsDisposed then
        d.Disposable := action(Self, _state);
    end);

  Result := d;
end;

function TMainThreadScheduler.Schedule(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  dt: TTimeSpan;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  dt := Normalize(dueTime);
  if dt.Ticks = 0 then
    Result := Schedule(state, action)
  else
    Result := ScheduleSlow(state, dt, action);
end;

function TMainThreadScheduler.ScheduleSlow(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  raise ENotImplementedException.Create('ScheduleSlow');
end;

{$ENDREGION}


end.
