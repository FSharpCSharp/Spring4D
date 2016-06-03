unit Spring.Reactive.Producer;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TProducer<T> = class(TObservableBase<T>)
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; virtual; abstract;
  public
    function Subscribe(const observer: IObserver<T>): IDisposable; override;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TProducer<T>'}

function TProducer<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  sink: ISingleAssignmentDisposable;
  subscription: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  sink := TSingleAssignmentDisposable.Create;
  subscription := TSingleAssignmentDisposable.Create;

  // TODO: scheduler
  subscription.Disposable := Run(observer, subscription,
    procedure(const s: IDisposable)
    begin
      sink.Disposable := s;
    end);

  // TODO
  Result := TCompositeDisposable.Create([sink, subscription]);
end;

{$ENDREGION}


end.
