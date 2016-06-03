unit Spring.Reactive.Observable.Throw;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TThrow<T> = class(TProducer<T>)
  private
    fError: Exception;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TThrow<T>;
        procedure Invoke;
      public
        constructor Create(const parent: TThrow<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const error: Exception; const scheduler: IScheduler);
    procedure Dispose; override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TThrow<T>'}

constructor TThrow<T>.Create(const error: Exception;
  const scheduler: IScheduler);
begin
  inherited Create;
  fError := error;
  fScheduler := scheduler;
end;

procedure TThrow<T>.Dispose;
begin
  FreeAndNil(fError);
  inherited;
end;

function TThrow<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TThrow<T>.TSink'}

constructor TThrow<T>.TSink.Create(const parent: TThrow<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TThrow<T>.TSink.Invoke;
begin
  fObserver.OnError(fParent.fError);
  Dispose;
end;

function TThrow<T>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
