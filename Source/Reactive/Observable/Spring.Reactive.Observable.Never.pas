unit Spring.Reactive.Observable.Never;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TNever<T> = class(TObservableBase<T>, IObservable<T>)
  public
    function Subscribe(const observer: IObserver<T>): IDisposable; override;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TNever<T>'}

function TNever<T>.Subscribe(const observer: IObserver<T>): IDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  Result := Disposable.Empty;
end;

{$ENDREGION}


end.
