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
  SyncObjs,
  System.Threading,
  Spring,
  Spring.Reactive;

type
  TConcurrencyAbstractionLayer = class(TInterfacedObject, IConcurrencyAbstractionLayer)
  private
    class var fThreadPool: TThreadPool;
    class var fTerminated: TEvent;
    function Normalize(const dueTime: TTimeSpan): TTimeSpan;

    type
      IWaitHandle = interface(IDisposable)
        function WaitFor(const Timeout: TTimeSpan): TWaitResult;
      end;

      TWaitHandle = class(TInterfacedObject, IWaitHandle, IDisposable)
      private
        fEvent: TEvent;
        function WaitFor(const Timeout: TTimeSpan): TWaitResult;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Dispose;
      end;
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
  Spring.Reactive.Disposables;


{$REGION 'TConcurrencyAbstractionLayer'}

class constructor TConcurrencyAbstractionLayer.Create;
begin
  fThreadPool := TThreadPool.Create;
  fTerminated := TEvent.Create;
end;

class procedure TConcurrencyAbstractionLayer.ShutDown;
begin
  fTerminated.SetEvent;
  fThreadPool.Free;
  fTerminated.Free;
end;

function TConcurrencyAbstractionLayer.StartTimer(const action: Action<TValue>;
  const state: TValue; const dueTime: TTimeSpan): IDisposable;
var
  _dueTime: TTimeSpan;
  _state: TValue;
  cancel: IWaitHandle;
begin
  _state := state;
  _dueTime := dueTime;

  cancel := TWaitHandle.Create;
  fThreadPool.QueueWorkItem(
    procedure
    begin
      if cancel.WaitFor(_dueTime) = wrTimeout then
        action(_state);
    end);
  Result := cancel;
end;

function TConcurrencyAbstractionLayer.StartPeriodicTimer(const action: Action;
  const period: TTimeSpan): IDisposable;
var
  _period: TTimeSpan;
  cancel: IWaitHandle;
begin
  _period := period;

  cancel := TWaitHandle.Create;
  fThreadPool.QueueWorkItem(
    procedure
    begin
      while cancel.WaitFor(_period) = wrTimeout do
        action();
    end);
  Result := cancel;
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


{$REGION 'TConcurrencyAbstractionLayer.TWaitHandle'}

constructor TConcurrencyAbstractionLayer.TWaitHandle.Create;
begin
  inherited Create;
  fEvent := TEvent.Create(nil, True, False, '');
end;

destructor TConcurrencyAbstractionLayer.TWaitHandle.Destroy;
begin
  fEvent.Free;
  inherited Destroy;
end;

procedure TConcurrencyAbstractionLayer.TWaitHandle.Dispose;
begin
  fEvent.SetEvent;
end;

function TConcurrencyAbstractionLayer.TWaitHandle.WaitFor(
  const Timeout: TTimeSpan): TWaitResult;
{$IFDEF MSWINDOWS}
var
  obj: THandleObject;
begin
  Result := TEvent.WaitForMultiple(
    [TConcurrencyAbstractionLayer.fTerminated, fEvent],
    Trunc(Timeout.TotalMilliseconds),
    False,
    obj);
{$ELSE}
begin
  Result := fEvent.WaitFor(Trunc(Timeout.TotalMilliseconds));
{$ENDIF}
end;

{$ENDREGION}


end.
