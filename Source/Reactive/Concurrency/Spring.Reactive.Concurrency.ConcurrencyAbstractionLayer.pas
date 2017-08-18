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

unit Spring.Reactive.Concurrency.ConcurrencyAbstractionLayer;

interface

uses
  System.Threading,
  Spring,
  Spring.Reactive;

type
  TConcurrencyAbstractionLayer = class(TInterfacedObject, IConcurrencyAbstractionLayer)
  private
    class var fThreadPool: TThreadPool;
    function Normalize(const dueTime: TTimeSpan): TTimeSpan;
  public
    class constructor Create;
    class procedure ShutDown;

    function StartTimer(const action: Action<TValue>; const state: TValue; const dueTime: TTimeSpan): IDisposable;
    function StartPeriodicTimer(const action: Action; const period: TTimeSpan): IDisposable;
    function QueueUserWorkItem(const action: Action<TValue>; const state: TValue): IDisposable;
    procedure Sleep(const timeout: TTimeSpan);
    function StartStopwatch: IStopwatch;
    function SupportsLongRunning: Boolean;
    procedure StartThread(const action: Action<TValue>; const state: TValue);
  end;

implementation

uses
  SyncObjs,
  SysUtils,
  Spring.Reactive.Disposables;

// TODO move into own unit
type
  TTimer = class(TDisposableObject)
  private
    fEvent: TEvent;
  public
    constructor Create(const action: Action<TValue>; const state: TValue;
      const dueTime: TTimeSpan);
    procedure Dispose; override;
  end;

  TPeriodicTimer = class(TDisposableObject)
  public
    constructor Create(const action: Action; const period: TTimeSpan);
  end;

{ TTimer }

constructor TTimer.Create(const action: Action<TValue>; const state: TValue;
  const dueTime: TTimeSpan);
var
  _dueTime: TTimeSpan;
  _state: TValue;
begin
  inherited Create;
  _state := state;
  _dueTime := dueTime;
  fEvent := TEvent.Create(nil, True, False, '');

  // TODO: implement this using an abstraction (timer or PPL)
  TTask.Run(
    procedure
    begin
      try
        if not IsDisposed and (fEvent.WaitFor(_dueTime) = wrTimeout) then
          action(_state);
      finally
        FreeAndNil(fEvent);
      end;
    end);
end;

procedure TTimer.Dispose;
begin
  inherited Dispose;
  FreeAndNil(fEvent);
end;

{ TPeriodicTimer }

constructor TPeriodicTimer.Create(const action: Action;
  const period: TTimeSpan);
var
  _period: TTimeSpan;
begin
  inherited Create;
  _period := period;
  TTask.Run(
    procedure
    begin
      while not IsDisposed do
      begin
        TThread.Sleep(_period);
        action;
      end;
    end);
end;


{$REGION 'TConcurrencyAbstractionLayer'}

class constructor TConcurrencyAbstractionLayer.Create;
begin
  fThreadPool := TThreadPool.Create;
end;

class procedure TConcurrencyAbstractionLayer.ShutDown;
begin
  fThreadPool.Free;
end;

function TConcurrencyAbstractionLayer.StartTimer(const action: Action<TValue>;
  const state: TValue; const dueTime: TTimeSpan): IDisposable;
begin
  Result := TTimer.Create(action, state, Normalize(dueTime));
end;

function TConcurrencyAbstractionLayer.StartPeriodicTimer(const action: Action;
  const period: TTimeSpan): IDisposable;
begin
  Result := TPeriodicTimer.Create(action, period);
end;

function TConcurrencyAbstractionLayer.QueueUserWorkItem(
  const action: Action<TValue>; const state: TValue): IDisposable;
var
  _state: TValue;
begin
  _state := state;
{$IFDEF DELPHIXE7_UP}
  fThreadPool.QueueWorkItem(
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

procedure TConcurrencyAbstractionLayer.Sleep(const timeout: TTimeSpan);
begin
  TThread.Sleep(Normalize(timeout));
end;

function TConcurrencyAbstractionLayer.StartStopwatch: IStopwatch;
begin

end;

function TConcurrencyAbstractionLayer.SupportsLongRunning: Boolean;
begin
  Result := False;
end;

procedure TConcurrencyAbstractionLayer.StartThread(const action: Action<TValue>;
  const state: TValue);
begin
  raise ENotSupportedException.Create('');
end;

function TConcurrencyAbstractionLayer.Normalize(
  const dueTime: TTimeSpan): TTimeSpan;
begin
  if dueTime < TTimeSpan.Zero then
    Result := TTimeSpan.Zero
  else
    Result := dueTime;
end;

{$ENDREGION}


end.
