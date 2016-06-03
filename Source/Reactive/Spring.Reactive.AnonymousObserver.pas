unit Spring.Reactive.AnonymousObserver;

interface

uses
  Spring,
  Spring.Reactive;

type
  TAnonymousObserver<T> = class sealed(TDisposableObject, IObserver<T>)
  private
    fOnNext: Action<T>;
    fOnError: Action<Exception>;
    fOnComplected: Action;
  public
    constructor Create(const onNext: Action<T>;
      const onError: Action<Exception>; const onCompleted: Action); overload;
    constructor Create(const onNext: Action<T>); overload;

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

implementation

uses
  Spring.Reactive.Stubs;


{$REGION 'TAnonymousObserver<T>'}

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>;
  const onError: Action<Exception>; const onCompleted: Action);
begin
  inherited Create;
  fOnNext := onNext;
  fOnError := onError;
  fOnComplected := onCompleted;
end;

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>);
begin
  Create(onNext, Stubs.Throw, Stubs.Nop);
end;

procedure TAnonymousObserver<T>.OnCompleted;
begin
  fOnComplected();
end;

procedure TAnonymousObserver<T>.OnError(const error: Exception);
begin
  fOnError(error);
end;

procedure TAnonymousObserver<T>.OnNext(const value: T);
begin
  fOnNext(value);
end;

{$ENDREGION}


end.
