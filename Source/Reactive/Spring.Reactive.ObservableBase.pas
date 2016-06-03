unit Spring.Reactive.ObservableBase;

interface

uses
  Spring.Reactive;

type
  TObservableBase<T> = class(TDisposableObject, IObservable<T>)
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
    function Subscribe(const observer: IObserver<T>): IDisposable; overload; virtual; abstract;
  public
    function Distinct: IObservable<T>;
    function DistinctUntilChanged: IObservable<T>;
    function IgnoreElements: IObservable<T>;
    function Skip(count: Integer): IObservable<T>;
    function Take(count: Integer): IObservable<T>;
    function Where(const predicate: Func<T, Boolean>): IObservable<T>;
  end;


implementation

uses
  Spring.Reactive.AnonymousObserver,
  Spring.Reactive.Observable.Distinct,
  Spring.Reactive.Observable.DistinctUntilChanged,
  Spring.Reactive.Observable.IgnoreElements,
  Spring.Reactive.Observable.Skip,
  Spring.Reactive.Observable.Take,
  Spring.Reactive.Observable.Where,
  Spring.Reactive.Stubs;


{$REGION 'TObservableBase<T>'}

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

function TObservableBase<T>.Distinct: IObservable<T>;
begin
  Result := TDistinct<T>.Create(Self);
end;

function TObservableBase<T>.DistinctUntilChanged: IObservable<T>;
begin
  Result := TDistinctUntilChanged<T>.Create(Self);
end;

function TObservableBase<T>.IgnoreElements: IObservable<T>;
begin
  Result := TIgnoreElements<T>.Create(Self);
end;

function TObservableBase<T>.Skip(count: Integer): IObservable<T>;
begin
  Result := TSkip<T>.Create(Self, count);
end;

function TObservableBase<T>.Take(count: Integer): IObservable<T>;
begin
  Result := TTake<T>.Create(Self, count);
end;

function TObservableBase<T>.Where(
  const predicate: Func<T, Boolean>): IObservable<T>;
begin
  Result := TWhere<T>.Create(Self, predicate);
end;

{$ENDREGION}


end.
