unit Spring.Reactive.Sink;

interface

uses
  Spring,
  Spring.Reactive;

type
  TSink<T> = class(TDisposableObject)
  protected
    fObserver: IObserver<T>;
    fCancel: IDisposable;
  public
    constructor Create(const observer: IObserver<T>; const cancel: IDisposable);
    procedure Dispose; override;
  end;

implementation

uses
  Spring.Reactive.Observers;


{$REGION 'TSink<T>'}

constructor TSink<T>.Create(const observer: IObserver<T>;
  const cancel: IDisposable);
begin
  inherited Create;
  fObserver := observer;
  fCancel := cancel;
end;

procedure TSink<T>.Dispose;
var
  cancel: IDisposable;
begin
  fObserver := TNopObserver<T>.Instance;
  cancel := TInterlocked.Exchange<IDisposable>(fCancel, nil);
  if Assigned(cancel) then
    cancel.Dispose;
end;

{$ENDREGION}


end.
