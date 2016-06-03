unit Spring.Reactive.Observable.Skip;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TSkip<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
//    fDuration: TTimeSpan;
//    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TSkip<T>;
        fRemaining: Integer;
      public
        constructor Create(const parent: TSkip<T>;
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


{$REGION 'TSkip<T>'}

constructor TSkip<T>.Create(const source: IObservable<T>; count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TSkip<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
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


{$REGION 'TSkip<T>.TSink'}

constructor TSkip<T>.TSink.Create(const parent: TSkip<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fRemaining := fParent.fCount;
end;

procedure TSkip<T>.TSink.OnCompleted;
begin
  fObserver.OnCompleted;
  Dispose;
end;

procedure TSkip<T>.TSink.OnError(const error: Exception);
begin
  fObserver.OnError(error);
  Disposed;
end;

procedure TSkip<T>.TSink.OnNext(const value: T);
begin
  if fRemaining <= 0 then
    fObserver.OnNext(value)
  else
    Dec(fRemaining);
end;

{$ENDREGION}


end.
