unit Spring.Reactive.Disposables;

interface

uses
  Spring,
  Spring.Reactive;

type
  Disposable = record
  private
    class function GetEmpty: IDisposable; static; inline;
  public
    class function Create(const dispose: Action): IDisposable; static;
    class property Empty: IDisposable read GetEmpty;
  end;

  TAnonymousDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    fDispose: Action;
    function GetIsDisposed: Boolean; inline;
  public
    constructor Create(const dispose: Action);
    destructor Destroy; override;
    procedure Dispose;
    property IsDisposed: Boolean read GetIsDisposed;
  end;

  TBooleanDisposable = class(TInterfaceBase, IDisposable, ICancelable)
  strict private class var
    fTrue: TBooleanDisposable;
  private
    fIsDisposed: Boolean;
    function GetIsDisposed: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(isDisposed: Boolean);
    destructor Destroy; override;
    procedure Dispose;
    class property True: TBooleanDisposable read fTrue;
  end;

  TCompositeDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    fDisposed: Boolean;
    fDisposables: TArray<IDisposable>;
    function GetIsDisposed: Boolean;
  public
    constructor Create(const disposables: array of IDisposable);
    destructor Destroy; override;
    procedure Dispose;
  end;

  TDefaultDisposable = class(TInterfaceBase, IDisposable)
  strict private class var
    fInstance: TDefaultDisposable;
  public
    class constructor Create;
    class destructor Destroy;
    destructor Destroy; override;
    procedure Dispose;
    class property Instance: TDefaultDisposable read fInstance;
  end;

  TSingleAssignmentDisposable = class(TInterfacedObject,
    IDisposable, ICancelable, ISingleAssignmentDisposable)
  private
    fCurrent: IDisposable;
    function GetIsDisposed: Boolean;
    function GetDisposable: IDisposable;
    procedure SetDisposable(const value: IDisposable);
  public
    destructor Destroy; override;
    procedure Dispose;
    property Disposable: IDisposable read GetDisposable write SetDisposable;
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


{$REGION 'TAnonymousDisposable'}

constructor TAnonymousDisposable.Create(const dispose: Action);
begin
  inherited Create;
  fDispose := dispose;
end;

destructor TAnonymousDisposable.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TAnonymousDisposable.Dispose;
var
  dispose: Action;
begin
  dispose := TInterlocked.Exchange<Action>(fDispose, nil);
  if Assigned(dispose) then
    dispose;
end;

function TAnonymousDisposable.GetIsDisposed: Boolean;
begin
  Result := not Assigned(fDispose);
end;

{$ENDREGION}


{$REGION 'TBooleanDisposable'}

class constructor TBooleanDisposable.Create;
begin
  fTrue := TBooleanDisposable.Create(System.True);
end;

class destructor TBooleanDisposable.Destroy;
begin
  fTrue.Free;
end;

constructor TBooleanDisposable.Create(isDisposed: Boolean);
begin
  inherited Create;
  fIsDisposed := isDisposed;
end;

destructor TBooleanDisposable.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TBooleanDisposable.Dispose;
begin
  fIsDisposed := System.True;
end;

function TBooleanDisposable.GetIsDisposed: Boolean;
begin
  Result := fIsDisposed;
end;

{$ENDREGION}


{$REGION 'TCompositeDisposable'}

constructor TCompositeDisposable.Create(
  const disposables: array of IDisposable);
begin
  inherited Create;
  fDisposables := TArray.Copy<IDisposable>(disposables);
end;

destructor TCompositeDisposable.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TCompositeDisposable.Dispose;
var
  currentDisposables: TArray<IDisposable>;
  d: IDisposable;
begin
  if not fDisposed then
  begin
    fDisposed := True;
    currentDisposables := fDisposables;
    fDisposables := nil;
  end;

  for d in currentDisposables do
    d.Dispose;
end;

function TCompositeDisposable.GetIsDisposed: Boolean;
begin
  Result := fDisposed;
end;

{$ENDREGION}


{$REGION 'TDefaultDisposable'}

class constructor TDefaultDisposable.Create;
begin
  fInstance := TDefaultDisposable.Create;
end;

class destructor TDefaultDisposable.Destroy;
begin
  fInstance.Free;
end;

destructor TDefaultDisposable.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TDefaultDisposable.Dispose;
begin
end;

{$ENDREGION}


{$REGION 'TSingleAssignmentDisposable'}

destructor TSingleAssignmentDisposable.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TSingleAssignmentDisposable.Dispose;
var
  old: IDisposable;
begin
  old := TInterlocked.Exchange<IDisposable>(fCurrent, TBooleanDisposable.True);
  if Assigned(old) then
    old.Dispose;
end;

function TSingleAssignmentDisposable.GetDisposable: IDisposable;
var
  current: IDisposable;
begin
  current := fCurrent;
  if current = IDisposable(TBooleanDisposable.True) then
    Result := TDefaultDisposable.Instance
  else
    Result := current;
end;

function TSingleAssignmentDisposable.GetIsDisposed: Boolean;
begin
  Result := fCurrent = IDisposable(TBooleanDisposable.True);
end;

procedure TSingleAssignmentDisposable.SetDisposable(const value: IDisposable);
var
  old: IDisposable;
begin
  old := TInterlocked.CompareExchange<IDisposable>(fCurrent, value, nil);
  if old = nil then
    Exit;

  if old <> IDisposable(TBooleanDisposable.True) then
    raise EInvalidOperationException.Create('disposable already assigned');

  if value <> nil then
    value.Dispose;
end;

{$ENDREGION}


end.
