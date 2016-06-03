unit Spring.Reactive.Observable.IgnoreElements;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TIgnoreElements<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      public
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>);
  end;

implementation


{$REGION 'TIgnoreElements<T>'}

constructor TIgnoreElements<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TIgnoreElements<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(observer, cancel);
  setSink(sink);
  // TODO implement SubscribeSafe
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TIgnoreElements<T>.TSink'}

procedure TIgnoreElements<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TIgnoreElements<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Dispose;
end;

procedure TIgnoreElements<T>.TSink.OnNext(const value: T);
begin
end;

{$ENDREGION}


end.
