unit Spring.Reactive.AnonymousObservable;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TAnonymousObservable<T> = class(TObservableBase<T>)
  private
    fSubscribe: Func<IObserver<T>, IDisposable>;
  protected
    function Subscribe(const observer: IObserver<T>): IDisposable; override;
  public
    constructor Create(const subscribe: Func<IObserver<T>, IDisposable>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAnonymousObservable<T>'}

constructor TAnonymousObservable<T>.Create(
  const subscribe: Func<IObserver<T>, IDisposable>);
begin
  inherited Create;
  fSubscribe := subscribe;
end;

function TAnonymousObservable<T>.Subscribe(
  const observer: IObserver<T>): IDisposable;
begin
  Result := fSubscribe(observer);
  if not Assigned(Result) then
    Result := Disposable.Empty;
end;

{$ENDREGION}


end.
