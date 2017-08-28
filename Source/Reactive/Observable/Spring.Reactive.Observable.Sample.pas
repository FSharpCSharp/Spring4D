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
  TSample<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fInterval: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TSample<TSource>;
        fSourceSubscription: IDisposable;
        fHasValue: Boolean;
        fValue: TSource;
        fAtEnd: Boolean;
        procedure Tick;
      public
        constructor Create(const parent: TSample<TSource>; const observer: IObserver<TSource>;
          const cancel: IDisposable);
        destructor Destroy; override;
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
    constructor Create(const source: IObservable<TSource>; const interval: TTimeSpan;
      const scheduler: IScheduler);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSample<TSource>'}

constructor TSample<TSource>.Create(const source: IObservable<TSource>;
  const interval: TTimeSpan; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fInterval := interval;
  fScheduler := scheduler;
end;

function TSample<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TSample<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TSample<TSource>.TSink'}

constructor TSample<TSource>.TSink.Create(const parent: TSample<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSample<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSample<TSource>.TSink.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fAtEnd := True;
    fSourceSubscription.Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TSample<TSource>.TSink.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    Observer.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TSample<TSource>.TSink.OnNext(const value: TSource);
begin
  MonitorEnter(Self);
  try
    fHasValue := True;
    fValue := value;
  finally
    MonitorExit(Self);
  end;
end;

function TSample<TSource>.TSink.Run: IDisposable;
var
  sourceSubscription: ISingleAssignmentDisposable;
  guard: IInterface;
begin
  sourceSubscription := TSingleAssignmentDisposable.Create;
  fSourceSubscription := sourceSubscription;
  sourceSubscription.Disposable := fParent.fSource.Subscribe(Self);
  guard := Self;
  Result := TStableCompositeDisposable.Create(
    sourceSubscription,
    (fParent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(fParent.fInterval,
    procedure
    begin
      if Assigned(guard) then
        Tick;
    end));
end;

procedure TSample<TSource>.TSink.Tick;
begin
  MonitorEnter(Self);
  try
    if fHasValue then
    begin
      fHasValue := False;
      Observer.OnNext(fValue);
    end;

    if fAtEnd then
    begin
      Observer.OnCompleted;
      Dispose;
    end;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


end.
