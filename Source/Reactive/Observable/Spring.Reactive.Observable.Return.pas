unit Spring.Reactive.Observable.Return;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TReturn<T> = class(TProducer<T>)
  private
    fValue: T;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TReturn<T>;
        procedure Invoke;
      public
        constructor Create(const parent: TReturn<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const value: T; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TReturn<T>'}

constructor TReturn<T>.Create(const value: T; const scheduler: IScheduler);
begin
  inherited Create;
  fValue := value;
  fScheduler := scheduler;
end;

function TReturn<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TReturn<T>.TSink'}

constructor TReturn<T>.TSink.Create(const parent: TReturn<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TReturn<T>.TSink.Invoke;
begin
  fObserver.OnNext(fParent.fValue);
  fObserver.OnCompleted;
  Dispose;
end;

function TReturn<T>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
