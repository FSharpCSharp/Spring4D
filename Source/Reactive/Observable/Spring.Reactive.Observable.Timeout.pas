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

unit Spring.Reactive.Observable.Timeout;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTimeout<TSource> = class
  public type
    TRelative =  class(TProducer<TSource>)
    private
      fSource: IObservable<TSource>;
      fDueTime: TTimeSpan;
      fOther: IObservable<TSource>;
      fScheduler: IScheduler;

      type
        TSink = class(TSink<TSource>, IObserver<TSource>)
        private
          fDueTime: TTimeSpan;
          fOther: IObservable<TSource>;
          fScheduler: IScheduler;
          fSubscription: ISerialDisposable;
          fTimer: ISerialDisposable;
          fId: UInt64;
          fSwitched: Boolean;
          procedure CreateTimer;
          function Timeout(const _: IScheduler; const myid: UInt64): IDisposable;
        public
          constructor Create(const parent: TRelative;
            const observer: IObserver<TSource>; const cancel: IDisposable);
          function Run(const source: IObservable<TSource>): IDisposable;

          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(const observer: IObserver<TSource>; const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>;
        const dueTime: TTimeSpan; const other: IObservable<TSource>;
        const scheduler: IScheduler);
    end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TTimeout<TSource>.TRelative'}

constructor TTimeout<TSource>.TRelative.Create(
  const source: IObservable<TSource>; const dueTime: TTimeSpan;
  const other: IObservable<TSource>; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fDueTime := dueTime;
  fOther := other;
  fScheduler := scheduler;
end;

function TTimeout<TSource>.TRelative.CreateSink(
  const observer: IObserver<TSource>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TTimeout<TSource>.TRelative.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fSource);
end;

{$ENDREGION}


{$REGION 'TTimeout<TSource>.TRelative.TSink'}

constructor TTimeout<TSource>.TRelative.TSink.Create(const parent: TRelative;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fDueTime := parent.fDueTime;
  fOther := parent.fOther;
  fScheduler := parent.fScheduler;

  fSubscription := TSerialDisposable.Create;
  fTimer := TSerialDisposable.Create;
end;

function TTimeout<TSource>.TRelative.TSink.Run(
  const source: IObservable<TSource>): IDisposable;
var
  original: ISingleAssignmentDisposable;
begin
  original := TSingleAssignmentDisposable.Create;

  fSubscription.Disposable := original;

  fId := 0;
  fSwitched := False;

  CreateTimer;

  original.Disposable := source.Subscribe(Self);

  Result := TStableCompositeDisposable.Create(fSubscription, fTimer);
end;

procedure TTimeout<TSource>.TRelative.TSink.CreateTimer;
var
  guard: IInterface;
begin
  guard := Self; // make sure that self is kept alive by capturing it
  fTimer.Disposable := fScheduler.Schedule(fId, fDueTime,
    function(const scheduler: IScheduler; const state: TValue): IDisposable
    begin
      if Assigned(guard) then
        Result := Timeout(scheduler, state.AsType<UInt64>);
    end);
end;

function TTimeout<TSource>.TRelative.TSink.Timeout(const _: IScheduler;
  const myid: UInt64): IDisposable;
var
  timerWins: Boolean;
begin
  timerWins := False;

  MonitorEnter(Self);
  try
    fSwitched := fId = myid;
    timerWins := fSwitched;
  finally
    MonitorExit(Self);
  end;

  if timerWins then
    fSubscription.Disposable := fOther.Subscribe(GetForwarder);

  Result := Disposable.Empty;
end;

procedure TTimeout<TSource>.TRelative.TSink.OnNext(const value: TSource);
var
  onNextWins: Boolean;
begin
  MonitorEnter(Self);
  try
    onNextWins := not fSwitched;
    if onNextWins then
      fId := fId + 1; // unchecked
  finally
    MonitorExit(Self);
  end;

  if onNextWins then
  begin
    Observer.OnNext(value);
    CreateTimer;
  end;
end;

procedure TTimeout<TSource>.TRelative.TSink.OnError(const error: Exception);
var
  onErrorWins: Boolean;
begin
  MonitorEnter(Self);
  try
    onErrorWins := not fSwitched;
    if onErrorWins then
      fId := fId + 1; // unchecked
  finally
    MonitorExit(Self);
  end;

  if onErrorWins then
  begin
    Observer.OnError(error);
    Dispose;
  end;
end;

procedure TTimeout<TSource>.TRelative.TSink.OnCompleted;
var
  onCompletedWins: Boolean;
begin
  MonitorEnter(Self);
  try
    onCompletedWins := not fSwitched;
    if onCompletedWins then
      fId := fId + 1; // unchecked
  finally
    MonitorExit(Self);
  end;

  if onCompletedWins then
  begin
    Observer.OnCompleted;
    Dispose;
  end;
end;

{$ENDREGION}


end.
