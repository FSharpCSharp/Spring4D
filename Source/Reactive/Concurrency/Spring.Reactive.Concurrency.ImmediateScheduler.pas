unit Spring.Reactive.Concurrency.ImmediateScheduler;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Concurrency.LocalScheduler;

type
  TImmediateScheduler = class(TLocalScheduler)
  private
    class var fInstance: IScheduler;
  public
    class constructor Create;
    class destructor Destroy;

    function Schedule(const action: Action): IDisposable; override;

    class property Instance: IScheduler read fInstance;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TImmediateScheduler'}

class constructor TImmediateScheduler.Create;
begin
  fInstance := TImmediateScheduler.Create;
end;

class destructor TImmediateScheduler.Destroy;
begin
  fInstance := nil;
end;

function TImmediateScheduler.Schedule(const action: Action): IDisposable;
begin
  action;
  Result := Disposable.Empty;
end;

{$ENDREGION}


end.
