unit Freezable;

interface

uses
  Rtti,
  SysUtils,
  HasCount,
  Spring,
  Spring.Collections,
  Spring.Interception;

type
  IFreezable = interface
    ['{EC4A628B-5137-4144-8589-F74FDAD6143F}']
    function GetIsFrozen: Boolean;
    procedure Freeze;
    property IsFrozen: Boolean read GetIsFrozen;
  end;

  TFreezable = record
  private
    class var
      fGenerator: TProxyGenerator;
    class function AsFreezable(const target: TObject): IFreezable; static;
  public
    class constructor Create;
    class destructor Destroy;

    class function IsFreezable(const obj: TObject): Boolean; static;
    class procedure Freeze(const freezable: TObject); static;
    class function IsFrozen(const obj: TObject): Boolean; static;

    class function MakeFreezable<T: class, constructor>: T; static;
  end;

  TFreezableInterceptor = class(TInterfacedObject, IInterceptor, IFreezable, IHasCount)
  private
    fCount: Integer;
    fIsFrozen: Boolean;
    function GetCount: Integer;
    function GetIsFrozen: Boolean;
  public
    procedure Freeze;
    procedure Intercept(const invocation: IInvocation);
  end;

  TFreezableInterceptorSelector = class(TInterfacedObject, IInterceptorSelector)
  public
    function SelectInterceptors(const method: TRttiMethod;
      const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
  end;

  TFreezableProxyGenerationHook = class(TInterfacedObject, IProxyGenerationHook)
  public
    procedure NonVirtualMemberNotification(const method: TRttiMethod);
    function ShouldInterceptMethod(const method: TRttiMethod): Boolean;
  end;

  ENotFreezableObjectException = class(Exception);
  EObjectFrozenException = class(Exception);

implementation

uses
  CallLoggingInterceptor,
  Spring.Reflection.Core,
  StrUtils;

{ TFreezable }

class constructor TFreezable.Create;
begin
  fGenerator := TProxyGenerator.Create;
end;

class destructor TFreezable.Destroy;
begin
  fGenerator.Free;
end;

class function TFreezable.AsFreezable(const target: TObject): IFreezable;
var
  hack: IProxyTargetAccessor;
begin
  if target = nil then
    Exit(nil);
  if not target.GetInterface(IProxyTargetAccessor, hack) then
    Exit(nil);
  Result := hack.GetInterceptors.FirstOrDefault(
    function(const i: IInterceptor): Boolean
    begin
      Result := Supports(i, IFreezable);
    end) as IFreezable;
end;

class function TFreezable.IsFreezable(const obj: TObject): Boolean;
begin
  Result := AsFreezable(obj) <> nil;
end;

class procedure TFreezable.Freeze(const freezable: TObject);
var
  interceptor: IFreezable;
begin
  interceptor := AsFreezable(freezable);
  if interceptor = nil then
    raise ENotFreezableObjectException.Create(freezable.ToString);
  interceptor.Freeze;
end;

class function TFreezable.IsFrozen(const obj: TObject): Boolean;
var
  freezable: IFreezable;
begin
  freezable := AsFreezable(obj);
  Result := Assigned(freezable) and freezable.IsFrozen;
end;

class function TFreezable.MakeFreezable<T>: T;
var
  freezableInterceptor: TFreezableInterceptor;
  options: TProxyGenerationOptions;
  proxy: TObject;
begin
  freezableInterceptor := TFreezableInterceptor.Create;
  options := TProxyGenerationOptions.Create(TFreezableProxyGenerationHook.Create);
  options.Selector := TFreezableInterceptorSelector.Create;
  proxy := fGenerator.CreateClassProxy(TClass(T), options, [
    TCallLoggingInterceptor.Create, freezableInterceptor]);
  Result := T(proxy)
end;

{ TFreezableInterceptor }

procedure TFreezableInterceptor.Freeze;
begin
  fIsFrozen := True;
end;

function TFreezableInterceptor.GetCount: Integer;
begin
  Result := fCount;
end;

function TFreezableInterceptor.GetIsFrozen: Boolean;
begin
  Result := fIsFrozen;
end;

procedure TFreezableInterceptor.Intercept(const invocation: IInvocation);
begin
  Inc(fCount);
  if fIsFrozen and StartsText('Set', invocation.Method.Name) then
    raise EObjectFrozenException.Create('');
  invocation.Proceed;
end;

{ TFreezableInterceptorSelector }

function TFreezableInterceptorSelector.SelectInterceptors(
  const method: TRttiMethod;
  const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
begin
  if StartsText('Set', method.Name) then
    Exit(interceptors);
  Result := interceptors.Where(
    function(const i: IInterceptor): Boolean
    begin
      Result := not (i is TFreezableInterceptor);
    end);
end;

{ TFreezableProxyGenerationHook }

procedure TFreezableProxyGenerationHook.NonVirtualMemberNotification(
  const method: TRttiMethod);
begin
  if StartsText('Set', method.Name) then
    raise EInvalidOperationException.CreateFmt('Property %s is not virtual. ' +
      'Cannot freeze classes with non-virtual properties.', [Copy(method.Name, 4)]);
end;

function TFreezableProxyGenerationHook.ShouldInterceptMethod(
  const method: TRttiMethod): Boolean;
begin
  Result := StartsText('Set', method.Name) or StartsText('Get', method.Name);
end;

end.
