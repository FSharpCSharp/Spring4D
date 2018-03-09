{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Reactive;

type
  TInfiniteIterator<T> = class(TIterator<T>)
  private
    fValue: T;
  protected
    function Clone: TIterator<T>; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const value: T);
  end;

  TObservableBase<T> = class(TInterfacedObject, IObservable<T>)
  private
    function LastOrDefaultInternal(raiseOnEmpty: Boolean): T;
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
    function Distinct: IObservable<T>;
    function DistinctUntilChanged: IObservable<T>;
    function DoAction(const onNext: Action<T>): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onCompleted: Action): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onError: Action<Exception>): IObservable<T>; overload;
    function DoAction(const onNext: Action<T>; const onError: Action<Exception>; const onCompleted: Action): IObservable<T>; overload;
    function DoAction(const observer: IObserver<T>): IObservable<T>; overload;
    procedure ForEach(const onNext: Action<T>);
//    function GetEnumerator: IEnumerator<T>;
    function IgnoreElements: IObservable<T>;
    function Publish: IConnectableObservable<T>;
    function Repeated: IObservable<T>; overload;
    function Repeated(repeatCount: Integer): IObservable<T>; overload;
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

    // "extension" methods (QueryLanguage.Blocking.cs)
    function Wait: T;

    // "extension" methods (QueryLanguage.Multiple.cs)
    function Amb(const second: IObservable<T>): IObservable<T>;
    function Concat(const second: IObservable<T>): IObservable<T>;
//    function Merge(const second: IObservable<T>): IObservable<T>; // not possible to implement here because of IObservable<IObservable<T>>
    function SkipUntil(const other: IObservable<T>): IObservable<T>;
    function TakeUntil(const other: IObservable<T>): IObservable<T>;

    // "extension" methods (QueryLanguage.Time.cs)
    function Timeout(const dueTime: TTimeSpan): IObservable<T>; overload;
    function Timeout(const dueTime: TTimeSpan; const scheduler: IScheduler): IObservable<T>; overload;
    function Timeout(const dueTime: TTimeSpan; const other: IObservable<T>): IObservable<T>; overload;
    function Timeout(const dueTime: TTimeSpan; const other: IObservable<T>; const scheduler: IScheduler): IObservable<T>; overload;

    function _: IObservableExtensions;
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
  Spring.Reactive.Observable.Amb,
  Spring.Reactive.Observable.Any,
  Spring.Reactive.Observable.Concat,
  Spring.Reactive.Observable.Distinct,
  Spring.Reactive.Observable.DistinctUntilChanged,
  Spring.Reactive.Observable.DoAction,
  Spring.Reactive.Observable.IgnoreElements,
  Spring.Reactive.Observable.Sample,
  Spring.Reactive.Observable.Skip,
  Spring.Reactive.Observable.SkipLast,
  Spring.Reactive.Observable.SkipUntil,
  Spring.Reactive.Observable.SkipWhile,
  Spring.Reactive.Observable.Take,
  Spring.Reactive.Observable.TakeLast,
  Spring.Reactive.Observable.TakeUntil,
  Spring.Reactive.Observable.TakeWhile,
  Spring.Reactive.Observable.Throttle,
  Spring.Reactive.Observable.Timeout,
  Spring.Reactive.Observable.Where,
  Spring.Reactive.Observable.Window,
  Spring.Reactive.Subjects.ConnectableObservable,
  Spring.Reactive.Subjects.Subject,
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

function TObservableBase<T>.Amb(const second: IObservable<T>): IObservable<T>;
begin
  Result := TAmb<T>.Create(Self, second);
end;

function TObservableBase<T>.Any: IObservable<Boolean>;
begin
  Result := TAny<T>.TCount.Create(Self);
end;

function TObservableBase<T>.Any(
  const predicate: Predicate<T>): IObservable<Boolean>;
begin
  Result := TAny<T>.TPredicate.Create(Self, predicate);
end;

function TObservableBase<T>.Concat(
  const second: IObservable<T>): IObservable<T>;
begin
  Result := TConcat<T>.Create(TEnumerable.From<IObservable<T>>(
    TArray<IObservable<T>>.Create(Self, second)));
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

//function TObservableBase<T>.GetEnumerator: IEnumerator<T>;
//begin
//  Result := TGetEnumerator<T>.Create.Run(source);
//end;

function TObservableBase<T>.IgnoreElements: IObservable<T>;
begin
  Result := TIgnoreElements<T>.Create(Self);
end;

function TObservableBase<T>.LastOrDefaultInternal(raiseOnEmpty: Boolean): T;
var
  value: T;
  seenValue: Boolean;
  ex: Exception;
  evt: TEvent;
  using: IDisposable;
begin
  value := Default(T);
  seenValue := False;
  evt := TEvent.Create(nil, False, False, '');

  using := Subscribe(
    procedure(const v: T)
    begin
      seenValue := True;
      value := v;
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
    evt.Free;
    using.Dispose;
    using := nil;
  end;

  if Assigned(ex) then
    raise ex;

  if raiseOnEmpty and not seenValue then
    raise EInvalidOperationException.Create('Source sequence doesn''t contain any elements.');

  Result := value;
end;

function TObservableBase<T>.ObserveOn(
  const scheduler: IScheduler): IObservable<T>;
begin
  Result := TObserveOn<T>.TScheduler.Create(Self, scheduler);
end;

function TObservableBase<T>.Publish: IConnectableObservable<T>;
begin
  Result := TConnectableObservable<T,T>.Create(Self, TSubject<T>.Create as ISubject<T>);
end;

function TObservableBase<T>.Repeated: IObservable<T>;
begin
  Result := TConcat<T>.Create(TInfiniteIterator<IObservable<T>>.Create(Self));
end;

function TObservableBase<T>.Repeated(repeatCount: Integer): IObservable<T>;
begin
  Result := TConcat<T>.Create(TEnumerable.Repeated<IObservable<T>>(Self, repeatCount));
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

function TObservableBase<T>.SkipUntil(
  const other: IObservable<T>): IObservable<T>;
begin
  Result := TSkipUntil<T,T>.Create(Self, other);
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

function TObservableBase<T>.TakeUntil(
  const other: IObservable<T>): IObservable<T>;
begin
  Result := TTakeUntil<T,T>.Create(Self, other);
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

function TObservableBase<T>.Timeout(const dueTime: TTimeSpan): IObservable<T>;
begin
  Result := TTimeout<T>.TRelative.Create(Self, dueTime, TObservable.Throw<T>(ETimeoutException.Create('')), SchedulerDefaults.TimeBasedOperations);
end;

function TObservableBase<T>.Timeout(const dueTime: TTimeSpan;
  const scheduler: IScheduler): IObservable<T>;
begin
  Result := TTimeout<T>.TRelative.Create(Self, dueTime, TObservable.Throw<T>(ETimeoutException.Create('')), scheduler);
end;

function TObservableBase<T>.Timeout(const dueTime: TTimeSpan;
  const other: IObservable<T>): IObservable<T>;
begin
  Result := TTimeout<T>.TRelative.Create(Self, dueTime, other, SchedulerDefaults.TimeBasedOperations);
end;

function TObservableBase<T>.Timeout(const dueTime: TTimeSpan;
  const other: IObservable<T>; const scheduler: IScheduler): IObservable<T>;
begin
  Result := TTimeout<T>.TRelative.Create(Self, dueTime, other, scheduler);
end;

function TObservableBase<T>.Wait: T;
begin
  Result := LastOrDefaultInternal(True);
end;

function TObservableBase<T>.Where(
  const predicate: Predicate<T>): IObservable<T>;
begin
  Result := TWhere<T>.Create(Self, predicate);
end;

function TObservableBase<T>._: IObservableExtensions;
begin
  Result := IObservableExtensions(Self);
end;

{$ENDREGION}


{$REGION 'TInfiniteIterator<T>'}

constructor TInfiniteIterator<T>.Create(const value: T);
begin
  inherited Create;
  fValue := value;
end;

function TInfiniteIterator<T>.Clone: TIterator<T>;
begin
  Result := TInfiniteIterator<T>.Create(fValue);
end;

function TInfiniteIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  current := fValue;
  Result := True;
end;

{$ENDREGION}


end.
