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
  Spring.Reactive.Disposables;

type
  IEvent = interface
    function WaitFor(const Timeout: TTimeSpan): TWaitResult;
    procedure SetEvent;
    procedure ResetEvent;
  end;

  TInterfacedEvent = class(TInterfacedObject, IEvent)
  private
    fEvent: TEvent;
    property Event: TEvent read fEvent implements IEvent;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTimer = class(TDisposableObject)
  private
    fEvent: IEvent;
  public
    constructor Create(const action: Action<TValue>; const state: TValue;
      const dueTime: TTimeSpan);
    procedure Dispose; override;
  end;

  TPeriodicTimer = class(TDisposableObject)
  private
    fEvent: IEvent;
  public
    constructor Create(const action: Action; const period: TTimeSpan);
    procedure Dispose; override;
  end;


{$REGION 'TInterfacedEvent'}

constructor TInterfacedEvent.Create;
begin
  inherited Create;
  fEvent := TEvent.Create(nil, True, False, '');
end;

destructor TInterfacedEvent.Destroy;
begin
  fEvent.Free;
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TTimer'}

constructor TTimer.Create(const action: Action<TValue>; const state: TValue;
  const dueTime: TTimeSpan);
var
  _dueTime: TTimeSpan;
  _state: TValue;
  event: IEvent;
begin
  inherited Create;
  _state := state;
  _dueTime := dueTime;
  event := TInterfacedEvent.Create;
  fEvent := event;

  TTask.Run(
    procedure
    begin
      if (event.WaitFor(_dueTime) = wrTimeout) // TODO: cancel this when application shuts down
        and not TThread.Current.Terminated then
        action(_state);
    end);
end;

procedure TTimer.Dispose;
begin
  inherited Dispose;
  fEvent.SetEvent;
end;

{$ENDREGION}


{$REGION 'TPeriodicTimer'}

constructor TPeriodicTimer.Create(const action: Action;
  const period: TTimeSpan);
var
  _period: TTimeSpan;
  event: IEvent;
begin
  inherited Create;
  _period := period;
  event := TInterfacedEvent.Create;
  fEvent := event;

  TTask.Run(
    procedure
    begin
      while (event.WaitFor(_period) = wrTimeout) // TODO: cancel this when application shuts down
        and not TThread.Current.Terminated do
        action();
    end);
end;

procedure TPeriodicTimer.Dispose;
begin
  inherited Dispose;
  fEvent.SetEvent;
end;

{$ENDREGION}


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
