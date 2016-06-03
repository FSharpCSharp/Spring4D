unit Spring.Reactive.Observable.Empty;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TEmpty<T> = class(TProducer<T>)
  private
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TEmpty<T>;
        procedure Invoke;
      public
        constructor Create(const parent: TEmpty<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const scheduler: IScheduler);
  end;

implementation


{$REGION 'TEmpty<T>'}

constructor TEmpty<T>.Create(const scheduler: IScheduler);
begin
  inherited Create;
  fScheduler := scheduler;
end;

function TEmpty<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TEmpty<T>.TSink'}

constructor TEmpty<T>.TSink.Create(const parent: TEmpty<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TEmpty<T>.TSink.Invoke;
begin
  fObserver.OnCompleted;
  Dispose;
end;

function TEmpty<T>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
