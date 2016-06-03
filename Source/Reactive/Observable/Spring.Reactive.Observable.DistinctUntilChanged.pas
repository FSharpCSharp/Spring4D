unit Spring.Reactive.Observable.DistinctUntilChanged;

interface

uses
  Generics.Defaults,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TDistinctUntilChanged<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
//    fKeySelector: Func<TSource,TKey>
    fComparer: IEqualityComparer<T>;//TKey

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TDistinctUntilChanged<T>;
        fCurrentKey: T;//TKey
        fHasCurrentKey: Boolean;
      public
        constructor Create(const parent: TDistinctUntilChanged<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    // TODO keySelector and comparer
    constructor Create(const source: IObservable<T>);
  end;

implementation


{$REGION 'TDistinctUntilChanged<T>'}

constructor TDistinctUntilChanged<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
  fComparer := TEqualityComparer<T>.Default;
end;

function TDistinctUntilChanged<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  // TODO implement SubscribeSafe
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TDistinctUntilChanged<T>.TSink'}

constructor TDistinctUntilChanged<T>.TSink.Create(
  const parent: TDistinctUntilChanged<T>; const observer: IObserver<T>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fCurrentKey := Default(T);
  fHasCurrentKey := False;
end;

procedure TDistinctUntilChanged<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TDistinctUntilChanged<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Dispose;
end;

procedure TDistinctUntilChanged<T>.TSink.OnNext(const value: T);
var
  comparerEquals: Boolean;
begin
  if fHasCurrentKey then
  try
    comparerEquals := fParent.fComparer.Equals(fCurrentKey, value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if not fHasCurrentKey or not comparerEquals then
  begin
    fHasCurrentKey := True;
    fCurrentKey := value;
    fObserver.OnNext(value);
  end;
end;

{$ENDREGION}


end.
