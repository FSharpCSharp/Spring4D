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

unit Spring.Reactive.Internal.ScheduledObserver;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObserverBase;

type
  IScheduledObserver<T> = interface
    procedure EnsureActive; overload;
    procedure EnsureActive(count: Integer); overload;
  end;

  TScheduledObserver<T> = class(TObserverBase<T>, IScheduledObserver<T>)
  private const
    STOPPED = 0;
    RUNNING = 0;
    PENDING = 0;
    FAULTED = 0;
  private
    fState: Integer;

    fQueue: TThreadedQueue<T>;
    fFailed: Boolean;
    fError: Exception;
    fCompleted: Boolean;

    fObserver: IObserver<T>;
    fScheduler: IScheduler;
//    fLongRunning: ISchedulerLongRunning;
    fDisposable: ISerialDisposable;
    procedure EnsureActiveSlow;
    procedure Run(const state: TValue; const recurse: Action<TValue>);
  protected
    procedure OnNextCore(const value: T); override;
    procedure OnErrorCore(const error: Exception); override;
    procedure OnCompletedCore; override;
  public
    constructor Create(const scheduler: IScheduler; const observer: IObserver<T>);
    destructor Destroy; override;
    procedure Dispose; override;

    procedure EnsureActive; overload;
    procedure EnsureActive(count: Integer); overload;
  end;

  TObserveOnObserver<T> = class(TScheduledObserver<T>)
  private
    fCancel: IDisposable;
  protected
    procedure OnNextCore(const value: T); override;
    procedure OnErrorCore(const error: Exception); override;
    procedure OnCompletedCore; override;
  public
    constructor Create(const scheduler: IScheduler; const observer: IObserver<T>;
      const cancel: IDisposable);
  end;

implementation

uses
  Types,
  Spring.Reactive.Disposables;


{$REGION 'TScheduledObserver<T>'}

procedure TScheduledObserver<T>.EnsureActive;
begin
  EnsureActive(1);
end;

constructor TScheduledObserver<T>.Create(const scheduler: IScheduler;
  const observer: IObserver<T>);
begin
  inherited Create;
  fQueue := TThreadedQueue<T>.Create(10, INFINITE, 0);
  fDisposable := TSerialDisposable.Create;

  fScheduler := scheduler;
  fObserver := observer;
end;

destructor TScheduledObserver<T>.Destroy;
begin
  fQueue.Free;
  inherited;
end;

procedure TScheduledObserver<T>.Dispose;
begin
  fDisposable.Dispose;
end;

procedure TScheduledObserver<T>.EnsureActive(count: Integer);
begin
//  if Assigned(fLongRunning) then
//  begin
//  end
//  else
    EnsureActiveSlow;
end;

procedure TScheduledObserver<T>.EnsureActiveSlow;
var
  isOwner: Boolean;
  old: Integer;
  guard: IInterface;
begin
  isOwner := False;

  while True do
  begin
    old := AtomicCmpExchange(fState, RUNNING, STOPPED);
    if old = STOPPED then
    begin
      isOwner := True;
      Break;
    end;

    if old = FAULTED then
      Exit;

    if (old = PENDING) or (old = RUNNING) and (AtomicCmpExchange(fState, PENDING, RUNNING) = RUNNING) then
      Break;
  end;

  if isOwner then
  begin
    guard := Self; // make sure that self is kept alive by capturing it
    fDisposable.Disposable := fScheduler.Schedule(TValue.Empty,
      procedure (const state: TValue; const recurse: Action<TValue>)
      begin
        if Assigned(guard) then
          Run(state, recurse);
      end);
  end;
end;

procedure TScheduledObserver<T>.OnNextCore(const value: T);
begin
  fQueue.PushItem(value);
end;

procedure TScheduledObserver<T>.Run(const state: TValue;
  const recurse: Action<TValue>);
var
  next: T;
  old: Integer;
  nop: T;
begin
  next := Default(T);
  while fQueue.PopItem(next) = wrTimeout do
  begin
    if fFailed then
    begin
      if fQueue.QueueSize > 0 then
        Continue;

      AtomicExchange(fState, STOPPED);
      fObserver.OnError(fError);
      Dispose;
      Exit;
    end;

    if fCompleted then
    begin
      if fQueue.QueueSize > 0 then
        Continue;

      AtomicExchange(fState, STOPPED);
      fObserver.OnCompleted;
      Dispose;
      Exit;
    end;

    old := AtomicCmpExchange(fState, STOPPED, RUNNING);
    if (old = RUNNING) or (old = FAULTED) then
      Exit;

    Assert(old = PENDING);

    fState := RUNNING;
  end;

  AtomicExchange(fState, RUNNING);

  try
    fObserver.OnNext(next);
  except
    AtomicExchange(fState, FAULTED);

    nop := Default(T);
    while fQueue.PopItem(nop) = wrSignaled do;

    raise;
  end;

  recurse(state);
end;

procedure TScheduledObserver<T>.OnErrorCore(const error: Exception);
begin
  fError := error;
  fFailed := True;
end;

procedure TScheduledObserver<T>.OnCompletedCore;
begin
  fCompleted := True;
end;

{$ENDREGION}


{$REGION 'TObserveOnObserver<T>'}

constructor TObserveOnObserver<T>.Create(const scheduler: IScheduler;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(scheduler, observer);
  fCancel := cancel;
end;

procedure TObserveOnObserver<T>.OnNextCore(const value: T);
begin
  inherited OnNextCore(value);
  EnsureActive;
end;

procedure TObserveOnObserver<T>.OnErrorCore(const error: Exception);
begin
  inherited OnErrorCore(error);
  EnsureActive;
end;

procedure TObserveOnObserver<T>.OnCompletedCore;
begin
  inherited OnCompletedCore;
  EnsureActive;
end;

{$ENDREGION}


end.
