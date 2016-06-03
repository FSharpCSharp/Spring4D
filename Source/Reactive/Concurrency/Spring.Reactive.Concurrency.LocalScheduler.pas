unit Spring.Reactive.Concurrency.LocalScheduler;

interface

uses
  Spring.Reactive;

type
  TLocalScheduler = class(TInterfacedObject, IScheduler)
  protected
//    function Invoke(const scheduler: IScheduler; const action: Action): IDisposable;
  public
    function Schedule(const action: Action): IDisposable; virtual; abstract;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TLocalScheduler'}

//function TLocalScheduler.Invoke(const scheduler: IScheduler;
//  const action: Action): IDisposable;
//begin
//  action();
//  Result := Disposable.Empty;
//end;

{$ENDREGION}


end.
