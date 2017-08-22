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

unit Spring.Reactive.ObservableBase;

interface

uses
  Spring,
  Spring.Reactive;

type
  TObservableBase<T> = class(TDisposableObject, IObservable<T>)
  private
    function ScheduledSubscribe(const _: IScheduler; const autoDetachObserver: TValue): IDisposable;
  protected
    function Subscribe: IDisposable; overload;
    function Subscribe(const onNext: Action<T>): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onError: Action<Exception>): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onCompleted: Action): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onError: Action<Exception>;
      const onCompleted: Action): IDisposable; overload;
    function Subscribe(const observer: IObserver<T>): IDisposable; overload; virtual;
    function SubscribeCore(const observer: IObserver<T>): IDisposable; virtual;

    function ObserveOn(const scheduler: IScheduler): IObservable<T>;
    function SubscribeOn(const scheduler: IScheduler): IObservable<T>;

    constructor Create;
  public
    destructor Destroy; override;
    function All(const predicate: Predicate<T>): IObservable<Boolean>;
    function Any: IObservable<Boolean>; overload;
    function Any(const predicate: Predicate<T>): IObservable<Boolean>; overload;
    function Concat(const second: IObservable<T>): IObservable<T>;
    function Distinct: IObservable<T>;
    function DistinctUntilChanged: IObservable<T>;
    function DoAction(const onNext: Action<T>): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onCompleted: Action): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onError: Action<Exception>): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onError: Action<Exception>; const onCompleted: Action): IObservable<T>; overload;
    function DoAction(const observer: IObserver<T>): IObservable<T>; overload;
    procedure ForEach(const onNext: Action<T>);
    function IgnoreElements: IObservable<T>;
    function Sample(const interval: TTimeSpan): IObservable<T>;
    function Skip(count: Integer): IObservable<T>;
    function SkipLast(count: Integer): IObservable<T>;
    function SkipWhile(const predicate: Predicate<T>): IObservable<T>; overload;
    function SkipWhile(const predicate: Func<T, Integer, Boolean>): IObservable<T>; overload;
    function Take(count: Integer): IObservable<T>;
    function TakeLast(count: Integer): IObservable<T>;
    function TakeWhile(const predicate: Predicate<T>): IObservable<T>; overload;
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IObservable<T>; overload;
    function Throttle(const dueTime: TTimeSpan): IObservable<T>;
    function Where(const predicate: Predicate<T>): IObservable<T>;

//    function _: IObservableExtensions<T>;
  end;

implementation

uses
  SyncObjs,
  Spring.Reactive.AnonymousObservable,
  Spring.Reactive.AnonymousObserver,
  Spring.Reactive.Concurrency.CurrentThreadScheduler,
  Spring.Reactive.Concurrency.SchedulerDefaults,
  Spring.Reactive.Concurrency.Synchronization.ObserveOn,
  Spring.Reactive.Internal.Stubs,
  Spring.Reactive.Observable.All,
  Spring.Reactive.Observable.Any,
  Spring.Reactive.Observable.Concat,
  Spring.Reactive.Observable.Distinct,
  Spring.Reactive.Observable.DistinctUntilChanged,
  Spring.Reactive.Observable.DoAction,
  Spring.Reactive.Observable.IgnoreElements,
  Spring.Reactive.Observable.Sample,
  Spring.Reactive.Observable.Skip,
  Spring.Reactive.Observable.SkipLast,
  Spring.Reactive.Observable.SkipWhile,
  Spring.Reactive.Observable.Take,
  Spring.Reactive.Observable.TakeLast,
  Spring.Reactive.Observable.TakeWhile,
  Spring.Reactive.Observable.Throttle,
  Spring.Reactive.Observable.Where,
  Spring.Reactive.Observable.Window,
  Spring.Reactive.Disposables;


{$REGION 'TObservableBase<T>'}

constructor TObservableBase<T>.Create;
begin
  inherited Create;
end;

destructor TObservableBase<T>.Destroy;
begin
  inherited Destroy;
end;

function TObservableBase<T>.Subscribe(
  const observer: IObserver<T>): IDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');
  // TODO TAutoDetachObserver<T>.Create(observer)
  if TCurrentThreadScheduler.IsScheduleRequired then
  begin
    Result := TCurrentThreadScheduler.Instance.Schedule(TValue.From(observer), ScheduledSubscribe);
  end
  else
  begin
    Result := SubscribeCore(observer);
  end;
end;

function TObservableBase<T>.SubscribeCore(
  const observer: IObserver<T>): IDisposable;
begin
  // abstract in .NET
end;

function TObservableBase<T>.SubscribeOn(
  const scheduler: IScheduler): IObservable<T>;
begin
  Guard.CheckNotNull(scheduler, 'scheduler');

  Result := TAnonymousObservable<T>.Create(
    function(const observer: IObserver<T>): IDisposable
    var
      m: ISingleAssignmentDisposable;
      d: ISerialDisposable;
    begin
      m := TSingleAssignmentDisposable.Create;
      d := TSerialDisposable.Create;
      d.Disposable := m;

      Self._AddRef;
      m.Disposable := scheduler.Schedule(
        procedure
        begin
          d.Disposable := TScheduledDisposable.Create(scheduler, Subscribe(observer)); // TODO: check if SubscribeCore is the correct one
          Self._Release;
        end);

      Result := d;
    end);
end;

function TObservableBase<T>.ScheduledSubscribe(const _: IScheduler;
  const autoDetachObserver: TValue): IDisposable;
begin
  Result := SubscribeCore(autoDetachObserver.AsType<IObserver<T>>);
end;

function TObservableBase<T>.Subscribe: IDisposable;
begin
  Result := Subscribe(TAnonymousObserver<T>.Create(Stubs<T>.Ignore, Stubs.Throw, Stubs.Nop) as IObserver<T>);
end;

function TObservableBase<T>.Subscribe(const onNext: Action<T>): IDisposable;
begin
  Result := Subscribe(TAnonymousObserver<T>.Create(onNext, Stubs.Throw, Stubs.Nop) as IObserver<T>);
end;

function TObservableBase<T>.Subscribe(const onNext: Action<T>;
  const onError: Action<Exception>): IDisposable;
begin
  Result := Subscribe(TAnonymousObserver<T>.Create(onNext, onError, Stubs.Nop) as IObserver<T>);
end;

function TObservableBase<T>.Subscribe(const onNext: Action<T>;
  const onCompleted: Action): IDisposable;
begin
  Result := Subscribe(TAnonymousObserver<T>.Create(onNext, Stubs.Throw, onCompleted) as IObserver<T>);
end;

function TObservableBase<T>.Subscribe(const onNext: Action<T>;
  const onError: Action<Exception>; const onCompleted: Action): IDisposable;
begin
  Result := Subscribe(TAnonymousObserver<T>.Create(onNext, onError, onCompleted) as IObserver<T>);
end;

function TObservableBase<T>.All(
  const predicate: Predicate<T>): IObservable<Boolean>;
begin
  Result := TAll<T>.Create(Self, predicate);
end;

function TObservableBase<T>.Any: IObservable<Boolean>;
begin
  Result := TAny<T>.Create(Self);
end;

function TObservableBase<T>.Any(
  const predicate: Predicate<T>): IObservable<Boolean>;
begin
  Result := TAny<T>.Create(Self, predicate);
end;

function TObservableBase<T>.Concat(
  const second: IObservable<T>): IObservable<T>;
begin
  Result := TConcat<T>.Create([Self, second]);
end;

function TObservableBase<T>.Distinct: IObservable<T>;
begin
  Result := TDistinct<T>.Create(Self);
end;

function TObservableBase<T>.DistinctUntilChanged: IObservable<T>;
begin
  Result := TDistinctUntilChanged<T>.Create(Self);
end;

function TObservableBase<T>.DoAction(const onNext: Action<T>): IObservable<T>;
begin
  Result := TDoAction<T>.TOnNext.Create(Self, onNext);
end;

function TObservableBase<T>.DoAction(const onNext: Action<T>;
  const onCompleted: Action): IObservable<T>;
begin
  Result := DoAction(onNext, Stubs<Exception>.Ignore, onCompleted);
end;

function TObservableBase<T>.DoAction(const onNext: Action<T>;
  const onError: Action<Exception>): IObservable<T>;
begin
  Result := DoAction(onNext, onError, Stubs.Nop);
end;

function TObservableBase<T>.DoAction(
  const observer: IObserver<T>): IObservable<T>;
begin
  Result := TDoAction<T>.TObserver.Create(Self, observer);
end;

function TObservableBase<T>.DoAction(const onNext: Action<T>;
  const onError: Action<Exception>; const onCompleted: Action): IObservable<T>;
begin
  Result := TDoAction<T>.TActions.Create(Self, onNext, onError, onCompleted);
end;

procedure TObservableBase<T>.ForEach(const onNext: Action<T>);
var
  evt: TEvent;
  ex: Exception;
  d: IDisposable;
begin
  // TODO: possibly implement the !NO_PERF version from RX.net
  evt := TEvent.Create(nil, True, False, '');
  ex := nil;
  try
    d := Subscribe(
      procedure(const x: T)
      begin
        try
          onNext(x);
        except
          on e: Exception do
          begin
            ex := Exception(AcquireExceptionObject);
            evt.SetEvent;
          end;
        end;
      end,
      procedure(const e: Exception)
      begin
        ex := e;
        evt.SetEvent;
      end,
      procedure
      begin
        evt.SetEvent;
      end);
    try
      evt.WaitFor;
    finally
      d.Dispose;
    end;
  finally
    evt.Free;
  end;

  if Assigned(ex) then
    raise ex;
end;

function TObservableBase<T>.IgnoreElements: IObservable<T>;
begin
  Result := TIgnoreElements<T>.Create(Self);
end;

function TObservableBase<T>.ObserveOn(
  const scheduler: IScheduler): IObservable<T>;
begin
  Result := TObserveOn<T>.TScheduler.Create(Self, scheduler);
end;

function TObservableBase<T>.Sample(const interval: TTimeSpan): IObservable<T>;
begin
  Result := TSample<T>.Create(Self, interval, SchedulerDefaults.TimeBasedOperations);
end;

function TObservableBase<T>.Skip(count: Integer): IObservable<T>;
begin
  Result := TSkip<T>.Create(Self, count);
end;

function TObservableBase<T>.SkipLast(count: Integer): IObservable<T>;
begin
  Result := TSkipLast<T>.Create(Self, count);
end;

function TObservableBase<T>.SkipWhile(
  const predicate: Predicate<T>): IObservable<T>;
begin
  Result := TSkipWhile<T>.Create(Self, predicate);
end;

function TObservableBase<T>.SkipWhile(
  const predicate: Func<T, Integer, Boolean>): IObservable<T>;
begin
  Result := TSkipWhile<T>.Create(Self, predicate);
end;

function TObservableBase<T>.Take(count: Integer): IObservable<T>;
begin
  Result := TTake<T>.Create(Self, count);
end;

function TObservableBase<T>.TakeLast(count: Integer): IObservable<T>;
begin
  Result := TTakeLast<T>.Create(Self, count, SchedulerDefaults.Iteration);
end;

function TObservableBase<T>.TakeWhile(
  const predicate: Predicate<T>): IObservable<T>;
begin
  Result := TTakeWhile<T>.Create(Self, predicate);
end;

function TObservableBase<T>.TakeWhile(
  const predicate: Func<T, Integer, Boolean>): IObservable<T>;
begin
  Result := TTakeWhile<T>.Create(Self, predicate);
end;

function TObservableBase<T>.Throttle(const dueTime: TTimeSpan): IObservable<T>;
begin
  Result := TThrottle<T>.Create(Self, dueTime, SchedulerDefaults.TimeBasedOperations);
end;

function TObservableBase<T>.Where(
  const predicate: Predicate<T>): IObservable<T>;
begin
  Result := TWhere<T>.Create(Self, predicate);
end;

//function TObservableBase<T>._: IObservableExtensions<T>;
//begin
//  Result := IObservableExtensions<T>(Self);
//end;

{$ENDREGION}


end.
