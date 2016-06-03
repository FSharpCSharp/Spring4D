unit Spring.Reactive;

{$O+,W-}

interface

uses
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Reactive.TimeInterval;

type
  Exception = SysUtils.Exception;

  {$M+}
  Action = reference to procedure;
  Action<T> = reference to procedure(const arg: T);
  Action<T1, T2> = reference to procedure(const arg1: T1; const arg2: T2);
  Action<T1, T2, T3> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3);
  Action<T1, T2, T3, T4> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4);

  Func<T, TResult> = reference to function(const arg: T): TResult;
  Func<T1, T2, TResult> = reference to function(const arg1: T1; const arg2: T2): TResult;
  Func<T1, T2, T3, TResult> = reference to function(const arg1: T1; const arg2: T2;  const arg3: T3): TResult;
  Func<T1, T2, T3, T4, TResult> = reference to function(const arg1: T1; const arg2: T2;  const arg3: T3; const arg4: T4): TResult;
  {$M-}

  IDisposable = interface // TODO: move to Spring.pas
    ['{97A20E65-EA03-4F3E-8620-CD5B7E402F42}']
    procedure Dispose;
  end;

  ICancelable = interface(IDisposable)
    ['{D5B5852E-0634-480A-917D-456EC32BC152}']
    function GetIsDisposed: Boolean;
    property IsDisposed: Boolean read GetIsDisposed;
  end;

  ISingleAssignmentDisposable = interface(ICancelable)
    function GetDisposable: IDisposable;
    procedure SetDisposable(const value: IDisposable);
    property Disposable: IDisposable read GetDisposable write SetDisposable;
  end;

  IScheduler = interface
    ['{61722BD1-AD2B-4B8C-A95A-A0A612FEEE8A}']
    function Schedule(const action: Action): IDisposable; overload;
  end;

  ISchedulerPeriodic = interface(IScheduler)
    ['{9B55DD0F-2B7E-4CFB-A95B-3D6F673E8036}']
    function SchedulePeriodic(const period: TTimeSpan; const action: Action): IDisposable; overload;
  end;

  Scheduler = record
    class function Schedule<TState>(const this: IScheduler; const state: TState;
      const action: Action<TState, Action<TState>>): IDisposable; static;
  end;

  IObserver<T> = interface(IDisposable)
    ['{3E391579-B8FE-41AB-99E4-B2BCAFAB2410}']
    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

  IObservable<T> = interface
    ['{E20F7E99-4952-47A3-8F02-8F37972C0E5D}']
    function Subscribe(const observer: IObserver<T>): IDisposable; overload;

    // "extension" methods - they redirect to Subscribe(IObserver<T>)
    function Subscribe: IDisposable; overload;
    function Subscribe(const onNext: Action<T>): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onError: Action<Exception>): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onCompleted: Action): IDisposable; overload;
    function Subscribe(const onNext: Action<T>;
      const onError: Action<Exception>;
      const onCompleted: Action): IDisposable; overload;

    // "extension" methods for reducing
    function Distinct: IObservable<T>;
    function DistinctUntilChanged: IObservable<T>;
    function IgnoreElements: IObservable<T>;
    function Skip(count: Integer): IObservable<T>;
    function Take(count: Integer): IObservable<T>;
    function Where(const predicate: Func<T, Boolean>): IObservable<T>;

    // TODO PPL overloads / cancellation token
  end;

  ISubject<T> = interface(IObservable<T>)
    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

  TDisposableObject = class(TInterfacedObject, IDisposable)
  public
    destructor Destroy; override;

    procedure Dispose; virtual;
  end;

  TInterlocked = Spring.TInterlocked;
  TInterlockedHelper = class helper for TInterlocked // TODO: move to Spring.pas
    class function CompareExchange<T: IInterface>(var Target: T; const Value, Comparand: T): T; overload;
    class function Exchange<T: IInterface>(var Target: T; const Value: T): T;
  end;

  TArrayHelper = class helper for TArray // TODO: move to Spring.pas
    class function Add<T>(const items: TArray<T>; const item: T): TArray<T>; static;
    class function Remove<T>(const items: TArray<T>; const item: T): TArray<T>; static;
  end;

  Observable = record
  private
    class function Timer(const dueTime, period: TTimeSpan; const scheduler: IScheduler): IObservable<Integer>; overload; static;
  public
    class function Create<T>(const subscribe: Func<IObserver<T>, IDisposable>): IObservable<T>; overload; static;

    class function Empty<T>: IObservable<T>; overload; static;

    class function Generate<TState,TResult>(const initialState: TState;
      const condition: Func<TState, Boolean>;
      const iterate: Func<TState, TState>;
      const resultSelector: Func<TState, TResult>): IObservable<TResult>; overload; static;

    class function Interval(const period: TTimeSpan): IObservable<Integer>; overload; static;

    class function Never<T>: IObservable<T>; overload; static;

    class function Range(start, count: Integer): IObservable<Integer>; overload; static;

    class function Return<T>(const value: T): IObservable<T>; overload; static;

    class function Throw<T>(const error: Exception): IObservable<T>; overload; static;
  end;

  Enumerable = record
  public
    class function Subscribe<T>(const source: IEnumerable<T>; const observer: IObserver<T>): IDisposable; static;
    class function ToObservable<T>(const source: IEnumerable<T>): IObservable<T>; static;
  end;

implementation

uses
  Spring.Reactive.AnonymousObservable,
  Spring.Reactive.Concurrency.SchedulerDefaults,
  Spring.Reactive.Observable.Empty,
  Spring.Reactive.Observable.Never,
  Spring.Reactive.Observable.Range,
  Spring.Reactive.Observable.Return,
  Spring.Reactive.Observable.Throw,
  Spring.Reactive.Observable.Timer,
  Spring.Reactive.Observable.ToObservable,
  Spring.Reactive.Stubs;


{$REGION 'TInterlockedHelper'}

class function TInterlockedHelper.CompareExchange<T>(var Target: T;
  const Value, Comparand: T): T;
begin
  PInterface(@Result)^ := IInterface(inherited CompareExchange(PPointer(@Target)^, PPointer(@Value)^, PPointer(@Comparand)^));
  if PPointer(@Result)^ = PPointer(@Comparand)^ then
  begin
    if Assigned(Value) then
      Value._AddRef;
    if Assigned(Result) then
      Result._Release;
  end;
end;

class function TInterlockedHelper.Exchange<T>(var Target: T;
  const Value: T): T;
begin
  PInterface(@Result)^ := IInterface(inherited Exchange(PPointer(@Target)^, PPointer(@Value)^));
  if Assigned(Value) then
    Value._AddRef;
  if Assigned(Result) then
    Result._Release;
end;

{$ENDREGION}


{$REGION 'TArrayHelper'}

class function TArrayHelper.Add<T>(const items: TArray<T>;
  const item: T): TArray<T>;
var
  n: Integer;
begin
//  Result := items + [item];
  Result := items;
  n := Length(Result);
  SetLength(Result, n + 1);
  Result[n] := item;
end;

class function TArrayHelper.Remove<T>(const items: TArray<T>;
  const item: T): TArray<T>;
var
  i: Integer;
begin
  i := TArray.IndexOf<T>(items, item);
  if i < 0 then
    Exit(items);
  if Length(items) = 1 then
    Exit(nil);
//  Result := System.Copy(items, 0, i) + System.Copy(items, i + 1);
  Result := TArray.Concat<T>([System.Copy(items, 0, i), System.Copy(items, i + 1)]);
end;

{$ENDREGION}


{$REGION 'TDisposableObject'}

destructor TDisposableObject.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TDisposableObject.Dispose;
begin
end;

{$ENDREGION}


{$REGION 'Observable'}

class function Observable.Create<T>(
  const subscribe: Func<IObserver<T>, IDisposable>): IObservable<T>;
begin
  Result := TAnonymousObservable<T>.Create(subscribe);
end;

class function Observable.Empty<T>: IObservable<T>;
begin
  Result := TEmpty<T>.Create(SchedulerDefaults.ConstantTimeOperations);
end;

class function Observable.Generate<TState, TResult>(const initialState: TState;
  const condition: Func<TState, Boolean>; const iterate: Func<TState, TState>;
  const resultSelector: Func<TState, TResult>): IObservable<TResult>;
begin
//  Result := TGenerate<TState, TResult>.Create(
//    initialState, condition, iterate, resultSelector);
end;

class function Observable.Interval(const period: TTimeSpan): IObservable<Integer>;
begin
  Result := Timer(period, period, SchedulerDefaults.TimeBasedOperations);
end;

class function Observable.Never<T>: IObservable<T>;
begin
  Result := TNever<T>.Create;
end;

class function Observable.Range(start, count: Integer): IObservable<Integer>;
begin
  Result := TRange.Create(start, count, SchedulerDefaults.Iteration);
end;

class function Observable.Return<T>(const value: T): IObservable<T>;
begin
  Result := TReturn<T>.Create(value, SchedulerDefaults.ConstantTimeOperations);
end;

class function Observable.Throw<T>(const error: Exception): IObservable<T>;
begin
  Result := TThrow<T>.Create(error, SchedulerDefaults.ConstantTimeOperations);
end;

class function Observable.Timer(const dueTime, period: TTimeSpan;
  const scheduler: IScheduler): IObservable<Integer>;
begin
  Result := TTimer.Create(dueTime, period, scheduler);
end;

{$ENDREGION}


{$REGION 'Scheduler'}

class function Scheduler.Schedule<TState>(const this: IScheduler;
  const state: TState;
  const action: Action<TState, Action<TState>>): IDisposable;
begin
  Result := this.Schedule(
    procedure
    begin
    end);
end;

{$ENDREGION}


{$REGION 'Enumerable'}

class function Enumerable.Subscribe<T>(const source: IEnumerable<T>;
  const observer: IObserver<T>): IDisposable;
var
  scheduler: IScheduler;
  e: IEnumerator<T>;
begin
  scheduler := SchedulerDefaults.Iteration;
{$IFNDEF NOPERF}
  Result := TToObservable<T>.Create(source).Subscribe(observer);
{$ELSE}
  e := source.GetEnumerator;
  scheduler.Schedule(
    procedure
    var
      hasNext: Boolean;
      ex: Exception;
      current: T;
    begin
      hasNext := False;
      ex := nil;
      current := Default(T);

      try
        hasNext := e.MoveNext;
        if hasNext then
          current := e.Current;
      except
        on e: Exception do
          ex := e;
      end;

      if not hasNext or Assigned(ex) then
        e := nil;
      if Assigned(ex) then
      begin
        observer.OnError(ex);
        Exit;
      end;
      if not hasNext then
      begin
        observer.OnCompleted;
        Exit;
      end;
      observer.OnNext(current);
    end);
{$ENDIF}
end;

class function Enumerable.ToObservable<T>(
  const source: IEnumerable<T>): IObservable<T>;
begin
{$IFNDEF NOPERF}
  Result := TToObservable<T>.Create(source);
{$ELSE}
  Result := TAnonymousObservable<T>.Create(
    function(const observer: IObserver<T>): IDisposable
    begin
      Subscribe<T>(source, observer);
    end);
{$ENDIF}
end;

{$ENDREGION}


end.
