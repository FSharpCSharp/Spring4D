unit Spring.Reactive.Observable.Take;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TTake<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
//    fDuration: TTimeSpan;
//    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TTake<T>;
        fRemaining: Integer;
      public
        constructor Create(const parent: TTake<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; count: Integer);
  end;

implementation


{$REGION 'TTake<T>'}

constructor TTake<T>.Create(const source: IObservable<T>; count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TTake<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  // TODO implement SubscribeSafe
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TTake<T>.TSink'}

constructor TTake<T>.TSink.Create(const parent: TTake<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fRemaining := fParent.fCount;
end;

procedure TTake<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TTake<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Disposed;
end;

procedure TTake<T>.TSink.OnNext(const value: T);
begin
  if fRemaining > 0 then
  begin
    Dec(fRemaining);
    fObserver.OnNext(value);
    if fRemaining = 0 then
    begin
      fObserver.OnCompleted;
      Dispose;
    end;
  end;
end;

{$ENDREGION}


end.
