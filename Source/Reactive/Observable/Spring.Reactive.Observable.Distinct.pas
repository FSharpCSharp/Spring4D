unit Spring.Reactive.Observable.Distinct;

interface

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TDistinct<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TDistinct<T>;
        fHashSet: ISet<T>;
      public
        constructor Create(const parent: TDistinct<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
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


{$REGION 'TDistinct<T>'}

constructor TDistinct<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TDistinct<T>.Run(const observer: IObserver<T>;
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


{$REGION 'TDistinct<T>.TSink'}

constructor TDistinct<T>.TSink.Create(const parent: TDistinct<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fHashSet := TCollections.CreateSet<T>;
end;

procedure TDistinct<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TDistinct<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Dispose;
end;

procedure TDistinct<T>.TSink.OnNext(const value: T);
var
  hasAdded: Boolean;
begin
  // TODO keySelector and comparer
  try
    hasAdded := fHashSet.Add(value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if hasAdded then
    fObserver.OnNext(value);
end;

{$ENDREGION}


end.
