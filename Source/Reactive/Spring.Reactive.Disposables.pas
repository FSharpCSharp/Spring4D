unit Spring.Reactive.Disposables;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Disposables.AnonymousDisposable,
  Spring.Reactive.Disposables.BooleanDisposable,
  Spring.Reactive.Disposables.CompositeDisposable,
  Spring.Reactive.Disposables.DefaultDisposable,
  Spring.Reactive.Disposables.RefCountDisposable,
  Spring.Reactive.Disposables.SerialDisposable,
  Spring.Reactive.Disposables.SingleAssignmentDisposable,
  Spring.Reactive.Disposables.StableCompositeDisposable;

type
  TAnonymousDisposable = Spring.Reactive.Disposables.AnonymousDisposable.TAnonymousDisposable;
  TBooleanDisposable = Spring.Reactive.Disposables.BooleanDisposable.TBooleanDisposable;
  TCompositeDisposable = Spring.Reactive.Disposables.CompositeDisposable.TCompositeDisposable;
  TDefaultDisposable = Spring.Reactive.Disposables.DefaultDisposable.TDefaultDisposable;
  TRefCountDisposable = Spring.Reactive.Disposables.RefCountDisposable.TRefCountDisposable;
  TSerialDisposable = Spring.Reactive.Disposables.SerialDisposable.TSerialDisposable;
  TSingleAssignmentDisposable = Spring.Reactive.Disposables.SingleAssignmentDisposable.TSingleAssignmentDisposable;
  TStableCompositeDisposable = Spring.Reactive.Disposables.StableCompositeDisposable.TStableCompositeDisposable;

  EObjectDisposedException = class(EInvalidOperationException);

  Disposable = record
  strict private
    class function GetEmpty: IDisposable; static;
  public
    class function Create(const dispose: Action): IDisposable; static;
    class property Empty: IDisposable read GetEmpty;
  end;

implementation


{$REGION 'Disposable'}

class function Disposable.Create(const dispose: Action): IDisposable;
begin
  Result := TAnonymousDisposable.Create(dispose);
end;

class function Disposable.GetEmpty: IDisposable;
begin
  Result := TDefaultDisposable.Instance;
end;

{$ENDREGION}


end.
