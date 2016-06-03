unit Spring.Reactive.Observable.Where;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TWhere<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fPredicate: Func<T, Boolean>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TWhere<T>;
      public
        constructor Create(const parent: TWhere<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const predicate: Func<T, Boolean>);
  end;


implementation

{$REGION 'TWhere<T>'}

constructor TWhere<T>.Create(const source: IObservable<T>;
  const predicate: Func<T, Boolean>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TWhere<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  if Assigned(fPredicate) then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    // TODO implement SubscribeSafe
    Result := fSource.Subscribe(sink);
  end;
end;

{$ENDREGION}


{$REGION 'TWhere<T>.TSink'}

constructor TWhere<T>.TSink.Create(const parent: TWhere<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TWhere<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TWhere<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Dispose;
end;

procedure TWhere<T>.TSink.OnNext(const value: T);
var
  shouldRun: Boolean;
begin
  shouldRun := False;
  try
    shouldRun := fParent.fPredicate(value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if shouldRun then
    fObserver.OnNext(value);
end;

{$ENDREGION}


end.
