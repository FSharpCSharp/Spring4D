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
  TThrottle<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fDueTime: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TThrottle<TSource>;
        fValue: TSource;
        fHasValue: Boolean;
        fCancelable: ISerialDisposable;
        fId: UInt64;
        function Propagate(const scheduler: IScheduler; const currentId: UInt64): IDisposable;
      public
        constructor Create(const parent: TThrottle<TSource>; const observer: IObserver<TSource>;
          const cancel: IDisposable);
        destructor Destroy; override;
        procedure Dispose; override;
        function Run: IDisposable;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const dueTime: TTimeSpan;
      const scheduler: IScheduler);
  end;

implementation

uses
  Rtti, // H2443
  Spring.Reactive.Disposables;


{$REGION 'TThrottle<TSource>}

constructor TThrottle<TSource>.Create(const source: IObservable<TSource>;
  const dueTime: TTimeSpan; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fDueTime := dueTime;
  fScheduler := scheduler;
end;

function TThrottle<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TThrottle<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TThrottle<TSource>.TSink'}

constructor TThrottle<TSource>.TSink.Create(const parent: TThrottle<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TThrottle<TSource>.TSink.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TThrottle<TSource>.TSink.Dispose;
begin
  if not IsDisposed then
  begin
    fParent._Release;
    fParent := nil;
  end;

  inherited Dispose;
end;

procedure TThrottle<TSource>.TSink.OnNext(const value: TSource);
var
  currentId: UInt64;
  d: ISingleAssignmentDisposable;
  guard: IInterface;
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
  guard := Self; // make sure that self is kept alive by capturing it
  d.Disposable := fParent.fScheduler.Schedule(currentId, fParent.fDueTime,
    function (const scheduler: IScheduler; const currentId: TValue): IDisposable
    begin
      if Assigned(guard) then
        Result := Propagate(scheduler, currentId.AsUInt64);
    end);
end;

procedure TThrottle<TSource>.TSink.OnError(const error: Exception);
begin
  fCancelable.Dispose;

  MonitorEnter(Self);
  try
    Observer.OnError(error);
    Dispose;

    fHasValue := False;
    Inc(fId);
  finally
    MonitorExit(Self);
  end;
end;

procedure TThrottle<TSource>.TSink.OnCompleted;
begin
  fCancelable.Dispose;

  MonitorEnter(Self);
  try
    if fHasValue then
      Observer.OnNext(fValue);

    Observer.OnCompleted;
    Dispose;

    fHasValue := False;
    Inc(fId);
  finally
    MonitorExit(Self);
  end;
end;

function TThrottle<TSource>.TSink.Propagate(const scheduler: IScheduler;
  const currentId: UInt64): IDisposable;
begin
  MonitorEnter(Self);
  try
    if fHasValue and (fId = currentId) then
      Observer.OnNext(fValue);
    fHasValue := False;
  finally
    MonitorExit(Self);
  end;
  Result := Disposable.Empty;
end;

function TThrottle<TSource>.TSink.Run: IDisposable;
var
  subscription: IDisposable;
begin
  fValue := Default(TSource);
  fHasValue := False;
  fCancelable := TSerialDisposable.Create;
  fId := 0;

  subscription := fParent.fSource.Subscribe(Self);
  Result := TStableCompositeDisposable.Create(subscription, fCancelable);
end;

{$ENDREGION}


end.
