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

unit Spring.Reactive.Observable.Sample;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSample<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fInterval: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TSample<T>;
        fSourceSubscription: IDisposable;
        fHasValue: Boolean;
        fValue: T;
        fAtEnd: Boolean;
        procedure Tick;
      public
        constructor Create(const parent: TSample<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const interval: TTimeSpan;
      const scheduler: IScheduler);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSample<T>'}

constructor TSample<T>.Create(const source: IObservable<T>;
  const interval: TTimeSpan; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fInterval := interval;
  fScheduler := scheduler;
end;

function TSample<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TSample<T>.TSink'}

constructor TSample<T>.TSink.Create(const parent: TSample<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSample<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSample<T>.TSink.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fAtEnd := True;
    fSourceSubscription.Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TSample<T>.TSink.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TSample<T>.TSink.OnNext(const value: T);
begin
  MonitorEnter(Self);
  try
    fHasValue := True;
    fValue := value;
  finally
    MonitorExit(Self);
  end;
end;

function TSample<T>.TSink.Run: IDisposable;
var
  sourceSubscription: ISingleAssignmentDisposable;
begin
  sourceSubscription := TSingleAssignmentDisposable.Create;
  fSourceSubscription := sourceSubscription;
  sourceSubscription.Disposable := fParent.fSource.Subscribe(Self);
  Result := TStableCompositeDisposable.Create(
    sourceSubscription,
    (fParent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(fParent.fInterval, Tick));
end;

procedure TSample<T>.TSink.Tick;
begin
  MonitorEnter(Self);
  try
    if fHasValue then
    begin
      fHasValue := False;
      fObserver.OnNext(fValue);
    end;

    if fAtEnd then
    begin
      fObserver.OnCompleted;
      Dispose;
    end;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


end.
