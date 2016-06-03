unit Spring.Reactive.Observable.ToObservable;

interface

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TToObservable<T> = class(TProducer<T>)
  private
    fSource: IEnumerable<T>;
//    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TToObservable<T>;
      public
        constructor Create(const parent: TToObservable<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        procedure Dispose; override;
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IEnumerable<T>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TToObservable<T>'}

constructor TToObservable<T>.Create(const source: IEnumerable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TToObservable<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
end;

{$ENDREGION}


{$REGION 'TToObservable<T>.TSink'}

constructor TToObservable<T>.TSink.Create(const parent: TToObservable<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

procedure TToObservable<T>.TSink.Dispose;
begin
  inherited;
  fParent._Release;
end;

function TToObservable<T>.TSink.Run: IDisposable;
var
  e: IEnumerator<T>;
begin
  try
    e := fParent.fSource.GetEnumerator;
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Result := Disposable.Empty;
    end;
  end;
end;

{$ENDREGION}


end.
