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

unit Spring.Reactive.Observable.Throttle;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TThrottle<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fDueTime: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TThrottle<T>;
        fValue: T;
        fHasValue: Boolean;
        fCancelable: ISerialDisposable;
        fId: UInt64;
        function Propagate(const scheduler: IScheduler; const currentId: TValue): IDisposable;
      public
        constructor Create(const parent: TThrottle<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
        destructor Destroy; override;
        procedure Dispose; override;
        function Run: IDisposable;
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const dueTime: TTimeSpan;
      const scheduler: IScheduler);
  end;

implementation

uses
  Rtti, // H2443
  Spring.Reactive.Disposables;


{$REGION 'TThrottle<T>}

constructor TThrottle<T>.Create(const source: IObservable<T>;
  const dueTime: TTimeSpan; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fDueTime := dueTime;
  fScheduler := scheduler;
end;

function TThrottle<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TThrottle<T>.TSink'}

constructor TThrottle<T>.TSink.Create(const parent: TThrottle<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TThrottle<T>.TSink.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TThrottle<T>.TSink.Dispose;
begin
  if not IsDisposed then
  begin
    fParent._Release;
    fParent := nil;
  end;

  inherited Dispose;
end;

procedure TThrottle<T>.TSink.OnNext(const value: T);
var
  currentId: UInt64;
  d: ISingleAssignmentDisposable;
begin
  currentId := 0;
  MonitorEnter(Self);
  try
    fHasValue := True;
    fValue := value;
    Inc(fId);
    currentId := fId;
  finally
    MonitorExit(Self);
  end;

  d := TSingleAssignmentDisposable.Create;
  fCancelable.Disposable := d;
  d.Disposable := fParent.fScheduler.Schedule(currentId, fParent.fDueTime, Propagate);
end;

procedure TThrottle<T>.TSink.OnError(const error: Exception);
begin
  fCancelable.Dispose;

  MonitorEnter(Self);
  try
    fObserver.OnError(error);
    Dispose;

    fHasValue := False;
    Inc(fId);
  finally
    MonitorExit(Self);
  end;
end;

procedure TThrottle<T>.TSink.OnCompleted;
begin
  fCancelable.Dispose;

  MonitorEnter(Self);
  try
    if fHasValue then
      fObserver.OnNext(fValue);

    fObserver.OnCompleted;
    Dispose;

    fHasValue := False;
    Inc(fId);
  finally
    MonitorExit(Self);
  end;
end;

function TThrottle<T>.TSink.Propagate(const scheduler: IScheduler;
  const currentId: TValue): IDisposable;
begin
  MonitorEnter(Self);
  try
    if fHasValue and (fId = currentId) then
      fObserver.OnNext(fValue);
    fHasValue := False;
  finally
    MonitorExit(Self);
  end;
  Result := Disposable.Empty;
end;

function TThrottle<T>.TSink.Run: IDisposable;
var
  subscription: IDisposable;
begin
  fValue := Default(T);
  fHasValue := False;
  fCancelable := TSerialDisposable.Create;
  fId := 0;

  subscription := fParent.fSource.Subscribe(Self);
  Result := TStableCompositeDisposable.Create(subscription, fCancelable);
end;

{$ENDREGION}


end.
