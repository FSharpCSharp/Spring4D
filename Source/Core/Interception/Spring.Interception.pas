{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Interception;

interface

//{$I Spring.inc}

uses
  Classes,
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
{$IF Compilerversion = 22}
  Spring.Reflection.Compatibility,
{$IFEND}
  Spring.Reflection.Core;

type
  IInvocation = interface
    ['{A307FB1B-CA96-4B20-84FE-A71B286F0924}']
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetResult: TValue;
    function GetTarget: TValue;
    procedure SetResult(const value: TValue);

    procedure Proceed;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Result: TValue read GetResult write SetResult;
    property Target: TValue read GetTarget;
  end;

  IInterceptor = interface
    ['{B33137BC-AAEE-40D7-86F3-BD6A65F4F7C7}']
    procedure Intercept(const invocation: IInvocation);
  end;

  IProxyTargetAccessor = interface
    ['{4A1EBB4E-ED31-44FB-AA87-E349610DC82F}']
    function GetInterceptors: IEnumerable<IInterceptor>;
    function GetTarget: TValue;
  end;

  IChangeProxyTarget = interface
    ['{2578A89C-8485-4F96-B841-E4FB849F3F9B}']
    procedure ChangeInvocationTarget(const target: TValue);
  end;

  IProxyGenerationHook = interface
    ['{53C88475-2A5A-4FC3-B0AE-04CB85758746}']
    procedure NonVirtualMemberNotification(const method: TRttiMethod);
    function ShouldInterceptMethod(const method: TRttiMethod): Boolean;
  end;

  IInterceptorSelector = interface
    ['{4BE1377C-2710-4867-8CCF-E098FF0DC783}']
    function SelectInterceptors(const method: TRttiMethod;
      const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
  end;

  TProxyGenerationOptions = record
  private
    type
      TAllMethodsHook = class(TInterfacedObject, IProxyGenerationHook)
      public
        procedure NonVirtualMemberNotification(const method: TRttiMethod);
        function ShouldInterceptMethod(const method: TRttiMethod): Boolean;
      end;

      TAllInterceptorsSelector = class(TInterfacedObject, IInterceptorSelector)
      public
        function SelectInterceptors(const method: TRttiMethod;
          const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
      end;
  private
    fHook: IProxyGenerationHook;
    fSelector: IInterceptorSelector;
    class var fDefault: TProxyGenerationOptions;
  public
    constructor Create(const hook: IProxyGenerationHook);
    class constructor Create;
    class property Default: TProxyGenerationOptions read fDefault;
    property Hook: IProxyGenerationHook read fHook write fHook;
    property Selector: IInterceptorSelector read fSelector write fSelector;
  end;

  TMethodIntercept = class
  private
    FImplementation: TMethodImplementation;
    FMethod: TRttiMethod;
    function GetCodeAddress: Pointer;
    function GetVirtualIndex: SmallInt;
  public
    constructor Create(const Method: TRttiMethod;
      const Callback: TMethodImplementationCallback);
    destructor Destroy; override;
    property CodeAddress: Pointer read GetCodeAddress;
    property Method: TRttiMethod read FMethod;
    property VirtualIndex: SmallInt read GetVirtualIndex;
  end;

  TAbstractInvocation = class(TInterfacedObject, IInvocation)
  private
    fArguments: TArray<TValue>;
    fCurrentInterceptorIndex: Integer;
    fInterceptors: TArray<IInterceptor>;
    fMethod: TRttiMethod;
    fResult: TValue;
    fTarget: TValue;
  {$REGION 'Property Accessors'}
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetResult: TValue;
    function GetTarget: TValue;
    procedure SetResult(const value: TValue);
  {$ENDREGION}
  protected
    procedure InvokeMethodOnTarget; virtual; abstract;
  public
    constructor Create(const target: TValue;
      const interceptors: TArray<IInterceptor>; const method: TRttiMethod;
      const arguments: TArray<TValue>);

    procedure Proceed;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Result: TValue read GetResult;
    property Target: TValue read GetTarget;
  end;

  TVirtualClass = class
  private
    fClassProxy: TClass;
    function GetClassProxyData: PClassData; inline;
  public
    constructor Create(classType: TClass);
    destructor Destroy; override;

    property ClassProxy: TClass read fClassProxy;
    property ClassProxyData: PClassData read GetClassProxyData;
  end;

  TProxyTargetAccessor = class(TInterfacedObject, IProxyTargetAccessor)
  private
    fInterceptors: IEnumerable<IInterceptor>;
    fTarget: TValue;
    function GetInterceptors: IEnumerable<IInterceptor>;
    function GetTarget: TValue;
  public
    constructor Create(const target: TValue;
      const interceptors: IEnumerable<IInterceptor>);
  end;

  TClassProxy = class(TVirtualClass)
  private
    type
      TInvocation = class(TAbstractInvocation)
      protected
        procedure InvokeMethodOnTarget; override;
      end;
    class var fProxies: IDictionary<TObject, TClassProxy>;
  private
    fIntercepts: IList<TMethodIntercept>;
    fInterceptors: IList<IInterceptor>;
    fInterceptorSelector: IInterceptorSelector;
    fAdditionalInterfaces: TArray<IInterface>;
    class procedure GetProxyTargetAccessor(Self: TObject; var Result: IProxyTargetAccessor); static;
    class procedure ProxyFreeInstance(Self: TObject); static;
  protected
    function CollectInterceptableMethods(
      const hook: IProxyGenerationHook): IEnumerable<TRttiMethod>;
    procedure GenerateIntercepts(const options: TProxyGenerationOptions);
    procedure GenerateInterfaces(const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions);
    procedure HandleInvoke(UserData: Pointer; const Args: TArray<TValue>;
      out Result: TValue); virtual;
  public
    constructor Create(proxyType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor);
    destructor Destroy; override;
    class constructor Create;

    function CreateInstance: TObject;
  end;

  TInterfaceProxy = class(TVirtualInterface, IProxyTargetAccessor)
  private
    type
      TInvocation = class(TAbstractInvocation, IChangeProxyTarget)
      protected
        procedure ChangeInvocationTarget(const target: TValue);
        procedure InvokeMethodOnTarget; override;
      end;
  private
    fInterceptors: IList<IInterceptor>;
    fInterceptorSelector: IInterceptorSelector;
    fAdditionalInterfaces: TArray<IInterface>;
    fTarget: TValue;
    function GetInterceptors: IEnumerable<IInterceptor>;
    function GetTarget: TValue;
  protected
    procedure GenerateInterfaces(const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions);
    procedure HandleInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue); virtual;
  public
    constructor Create(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const target: IInterface;
      const interceptors: array of IInterceptor);

    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

  TProxyGenerator = class
  public
    function CreateClassProxy<T: class>(
      const interceptors: array of IInterceptor): T; overload;
    function CreateClassProxy<T: class>(
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload;

    function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const options: TProxyGenerationOptions;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload;

    function CreateInterfaceProxyWithTarget<T: IInterface>(const target: T;
      const interceptors: array of IInterceptor): T; overload;
    function CreateInterfaceProxyWithTarget<T: IInterface>(const target: T;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload;

    function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const target: IInterface;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const target: IInterface; const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const target: IInterface;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const target: IInterface; const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;

    function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const interceptor: IInterceptor): T; overload;
    function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const interceptors: array of IInterceptor): T; overload;
    function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload;

    function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const interceptor: IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;
    function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload;
  end;

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);

implementation

uses
  RTLConsts,
  Spring.Helpers,
  Spring.Reflection;

resourcestring
  SProceedCalledMoreTimesThanExpected = 'Proceed has been called more times than expected.';


{$REGION 'Routines'}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
{$IF Defined(CPUX86)}
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
    tkVariant: // like tkRecord, but hard-coded size
      Result := IsConst or not (CC in [ccCdecl, ccStdCall, ccSafeCall]);
{$ELSEIF Defined(CPUX64)}
    tkRecord:
      Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4,8]);
    tkMethod,
    tkVariant:
      Result := True;
{$ELSEIF Defined(CPUARM)}
    tkRecord:
      Result := (CC = ccReg) or (CC = ccPascal);
    tkMethod,
    tkVariant:
      Result := True;
{$IFEND}
{$IF DEFINED(NEXTGEN)}
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
{$IFEND}
  else
    Result := False;
  end;
end;

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);
begin
  if Par.ParamType = nil then
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData) // untyped var or const
  else if Par.Flags * [pfVar, pfOut] <> [] then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData);
  end
  else if (pfConst in Par.Flags) and
    PassByRef(Par.ParamType.Handle, CC, True) then
  begin
    if TypeInfo(TValue) = Par.ParamType.Handle then
      ArgDest := TValue.From(ArgSrc)
    else
    begin
      if Par.ParamType.Handle <> ArgSrc.TypeInfo then
        raise EInvalidCast.CreateRes(@SByRefArgMismatch);
      ArgDest := TValue.From(ArgSrc.GetReferenceToRawData);
    end
  end
  else
    ArgDest := ArgSrc.Cast(Par.ParamType.Handle);
end;

{$ENDREGION}


{$REGION 'TMethodIntercept'}

constructor TMethodIntercept.Create(const Method: TRttiMethod;
  const Callback: TMethodImplementationCallback);
begin
  FImplementation := Method.CreateImplementation(Self, Callback);
  FMethod := Method;
end;

destructor TMethodIntercept.Destroy;
begin
  FImplementation.Free;
  inherited;
end;

function TMethodIntercept.GetCodeAddress: Pointer;
begin
  Result := FImplementation.CodeAddress;
end;

function TMethodIntercept.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

{$ENDREGION}


{$REGION 'TVirtualClass'}

constructor TVirtualClass.Create(classType: TClass);
begin
  inherited Create;
  fClassProxy := CreateVirtualClass(classType);
end;

destructor TVirtualClass.Destroy;
begin
  DestroyVirtualClass(fClassProxy);
  inherited;
end;

function TVirtualClass.GetClassProxyData: PClassData;
begin
  Result := GetClassData(fClassProxy);
end;

{$ENDREGION}


{$REGION 'TProxyTargetAccessor'}

constructor TProxyTargetAccessor.Create(const target: TValue;
  const interceptors: IEnumerable<IInterceptor>);
begin
  inherited Create;
  fInterceptors := interceptors;
  fTarget := target;
end;

function TProxyTargetAccessor.GetInterceptors: IEnumerable<IInterceptor>;
begin
  Result := fInterceptors;
end;

function TProxyTargetAccessor.GetTarget: TValue;
begin
  Result := fTarget;
end;

{$ENDREGION}


{$REGION 'TClassProxy'}

constructor TClassProxy.Create(proxyType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor);
begin
  inherited Create(proxyType);
  ClassProxyData.FreeInstance := ProxyFreeInstance;
  fIntercepts := TCollections.CreateObjectList<TMethodIntercept>;
  fInterceptors := TCollections.CreateInterfaceList<IInterceptor>(interceptors);
  fInterceptorSelector := options.Selector;
  GenerateIntercepts(options);
  GenerateInterfaces(additionalInterfaces, options);
end;

destructor TClassProxy.Destroy;
begin
  FreeMem(ClassProxyData.IntfTable);
  inherited;
end;

class constructor TClassProxy.Create;
begin
  fProxies := TCollections.CreateDictionary<TObject, TClassProxy>([doOwnsValues]);
end;

function TClassProxy.CreateInstance: TObject;
var
  table: PInterfaceTable;
  i: Integer;
begin
  Result := ClassProxy.Create;
  fProxies.Add(Result, Self);

  table := ClassProxyData.IntfTable;
  for i := 1 to table.EntryCount - 1 do
    PPointer(@PByte(Result)[table.Entries[i].ImplGetter and $00FFFFFF])^ :=
      Pointer(fAdditionalInterfaces[i - 1]);
end;

function TClassProxy.CollectInterceptableMethods(
  const hook: IProxyGenerationHook): IEnumerable<TRttiMethod>;
begin
  Result := TType.GetType(fClassProxy).Methods.Where(
    function(const method: TRttiMethod): Boolean
    begin
      if not (method.MethodKind in [mkFunction, mkProcedure]) then
        Exit(False);
      if not method.HasExtendedInfo then
        Exit(False);
      if method.DispatchKind <> dkVtable then
      begin
        hook.NonVirtualMemberNotification(method);
        Exit(False);
      end;
      if method.VirtualIndex < 0 then
        Exit(False);
      if fIntercepts[method.VirtualIndex] <> nil then
        Exit(False);
      Result := not Assigned(hook) or hook.ShouldInterceptMethod(method);
    end);
end;

procedure TClassProxy.HandleInvoke(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  method: TRttiMethod;
  arguments: TArray<TValue>;
  interceptors: TArray<IInterceptor>;
  invocation: IInvocation;
  i: Integer;
begin
  method := TMethodIntercept(UserData).Method;
  arguments := Copy(Args, 1);
  interceptors := fInterceptorSelector.SelectInterceptors(method, fInterceptors).ToArray;
  invocation := TInvocation.Create(Args[0], interceptors, method, arguments);
  try
    invocation.Proceed;
  finally
    for i := 1 to High(Args) do
      Args[i] := arguments[i - 1];
    Result := invocation.Result;
  end;
end;

procedure TClassProxy.GenerateIntercepts(const options: TProxyGenerationOptions);
var
  method: TRttiMethod;
  virtualMethodCount: Integer;
  intercept: TMethodIntercept;
begin
  virtualMethodCount := GetVirtualMethodCount(fClassProxy);
  fIntercepts.Count := virtualMethodCount;

  for method in CollectInterceptableMethods(options.Hook) do
  begin
    intercept := TMethodIntercept.Create(method, HandleInvoke);
    fIntercepts[method.VirtualIndex] := intercept;
    PVirtualMethodTable(fClassProxy)[method.VirtualIndex] := intercept.CodeAddress;
  end;
end;

procedure TClassProxy.GenerateInterfaces(
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions);
var
  entryCount, size: Integer;
  table: PInterfaceTable;
  i: Integer;
  offset: NativeUInt;
begin
  entryCount := Length(additionalInterfaces) + 1;
  size := SizeOf(Integer) + SizeOf(TInterfaceEntry) * EntryCount;
{$IFDEF CPUX64}
  Inc(size, SizeOf(LongWord));
{$ENDIF}
  GetMem(table, size);
//  table := AllocMem(size);
  table.EntryCount := EntryCount;
  ClassProxyData.IntfTable := table;

  // add IProxyTargetAccessor
  table.Entries[0].IID := IProxyTargetAccessor;
  table.Entries[0].VTable := nil;
  table.Entries[0].IOffset := 0;
  table.Entries[0].ImplGetter := NativeUInt(@GetProxyTargetAccessor);

  SetLength(fAdditionalInterfaces, Length(additionalInterfaces));

  offset := ClassProxyData.InstanceSize - hfFieldSize;
  Inc(ClassProxyData.InstanceSize, Length(additionalInterfaces) * SizeOf(Pointer));

  // add other interfaces
  for i := 1 to Length(additionalInterfaces) do
  begin
    TInterfaceProxy.Create(additionalInterfaces[i - 1], [], options, nil,
      fInterceptors.ToArray).QueryInterface(
      GetTypeData(additionalInterfaces[i - 1]).Guid, fAdditionalInterfaces[i - 1]);
    table.Entries[i].IID := GetTypeData(additionalInterfaces[i - 1]).Guid;
    table.Entries[i].VTable := nil;
    table.Entries[i].IOffset := 0;
{$IFDEF CPUX64}
    table.Entries[i].ImplGetter := offset or $FF00000000000000;
{$ELSE}
    table.Entries[i].ImplGetter := offset or $FF000000;
{$ENDIF}
    Inc(offset, SizeOf(Pointer));
  end;
end;

class procedure TClassProxy.GetProxyTargetAccessor(Self: TObject;
  var Result: IProxyTargetAccessor);
begin
  Result := TProxyTargetAccessor.Create(
    Self, fProxies[Self].fInterceptors);
end;

class procedure TClassProxy.ProxyFreeInstance(Self: TObject);
begin
  GetClassData(Self.ClassParent).FreeInstance(Self); // inherited
  PPointer(Self)^ := Self.ClassParent;
  fProxies.Remove(Self);
end;

{$ENDREGION}


{$REGION 'TClassProxy.TInvocation'}

procedure TClassProxy.TInvocation.InvokeMethodOnTarget;
var
  params: TArray<TRttiParameter>;
  args: TArray<TValue>;
  i: Integer;
begin
  params := fMethod.GetParameters;
  SetLength(args, Length(fArguments) + 1);
  args[0] := fTarget;

  // convert arguments for Invoke call (like done in the DispatchInvoke methods
  for i := Low(fArguments) to High(fArguments) do
    PassArg(params[i], fArguments[i], args[i + 1], fMethod.CallingConvention);

  if not fTarget.IsEmpty then
    fResult := Rtti.Invoke(fMethod.CodeAddress, args, fMethod.CallingConvention, fMethod.ReturnTypeHandle);
end;

{$ENDREGION}


{$REGION 'TInterfaceProxy'}

constructor TInterfaceProxy.Create(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions; const target: IInterface;
  const interceptors: array of IInterceptor);
begin
  inherited Create(proxyType, HandleInvoke);
  fInterceptors := TCollections.CreateInterfaceList<IInterceptor>(interceptors);
  fInterceptorSelector := options.Selector;
  fTarget := TValue.From(target);
  GenerateInterfaces(additionalInterfaces, options);
end;

procedure TInterfaceProxy.GenerateInterfaces(
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions);
var
  i: Integer;
begin
  SetLength(fAdditionalInterfaces, Length(additionalInterfaces));
  for i := Low(additionalInterfaces) to High(additionalInterfaces) do
  begin
    TInterfaceProxy.Create(additionalInterfaces[i], [], options, nil,
      fInterceptors.ToArray).QueryInterface(
      GetTypeData(additionalInterfaces[i]).Guid, fAdditionalInterfaces[i]);
  end;
end;

function TInterfaceProxy.GetInterceptors: IEnumerable<IInterceptor>;
begin
  Result := fInterceptors;
end;

function TInterfaceProxy.GetTarget: TValue;
begin
  Result := fTarget;
end;

procedure TInterfaceProxy.HandleInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  arguments: TArray<TValue>;
  interceptors: TArray<IInterceptor>;
  invocation: IInvocation;
  i: Integer;
begin
  arguments := Copy(Args, 1);
  interceptors := fInterceptorSelector.SelectInterceptors(method, fInterceptors).ToArray;
  invocation := TInvocation.Create(fTarget, interceptors, Method, arguments);
  try
    invocation.Proceed;
  finally
    for i := 1 to High(Args) do
      Args[i] := arguments[i - 1];
    Result := invocation.Result;
  end;
end;

function TInterfaceProxy.QueryInterface(const IID: TGUID; out Obj): HResult;
var
  i: Integer;
begin
  Result := inherited;
  if Result = S_OK then
    Exit;
  if not fTarget.IsEmpty then
  begin
    Result := fTarget.AsInterface.QueryInterface(IID, Obj);
    if Result = S_OK then
      Exit;
  end;
  for i := Low(fAdditionalInterfaces) to High(fAdditionalInterfaces) do
    if fAdditionalInterfaces[i].QueryInterface(IID, obj) = S_OK then
      Exit(S_OK);
end;

{$ENDREGION}


{$REGION 'TInterfaceProxy.TInvocation'}

procedure TInterfaceProxy.TInvocation.ChangeInvocationTarget(
  const target: TValue);
begin
  fTarget := target;
end;

procedure TInterfaceProxy.TInvocation.InvokeMethodOnTarget;
begin
  if not fTarget.IsEmpty then
    fResult := fMethod.Invoke(fTarget, fArguments)
  else
    raise ENotImplementedException.Create(fMethod.Parent.DefaultName + '.' + fMethod.Name);
end;

{$ENDREGION}


{$REGION 'TProxyGenerator'}

function TProxyGenerator.CreateClassProxy<T>(
  const interceptors: array of IInterceptor): T;
begin
  Result := T(CreateClassProxy(
    TClass(T), TProxyGenerationOptions.Default, interceptors));
end;

function TProxyGenerator.CreateClassProxy<T>(
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Result := T(CreateClassProxy(TClass(T), options, interceptors));
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, additionalInterfaces,
    TProxyGenerationOptions.Default, interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const options: TProxyGenerationOptions;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, [], options, constructorArguments, interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, [], TProxyGenerationOptions.Default,
    constructorArguments, interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, [], TProxyGenerationOptions.Default, [], interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, [], options, [], interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, additionalInterfaces, options, [], interceptors);
end;

function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
var
  proxy: TClassProxy;
begin
  proxy := TClassProxy.Create(
    classType, additionalInterfaces, options, interceptors);
  Result := proxy.CreateInstance;
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget<T>(const target: T;
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithTarget(
    TypeInfo(T), IInterface(target), TProxyGenerationOptions.Default, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget<T>(const target: T;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithTarget(
    TypeInfo(T), IInterface(target), options, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
  const target: IInterface; const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(
    proxyType, [], target, TProxyGenerationOptions.Default, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
  const target: IInterface; const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(
    proxyType, [], target, options, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo; const target: IInterface;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(
    proxyType, additionalInterfaces, target, TProxyGenerationOptions.Default, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo; const target: IInterface;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := TInterfaceProxy.Create(
    proxyType, additionalInterfaces, options, target, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const interceptor: IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), interceptor),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), options, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
  const interceptor: IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], TProxyGenerationOptions.Default, [interceptor]);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], TProxyGenerationOptions.Default, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, additionalInterfaces, TProxyGenerationOptions.Default, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], options, interceptors);
end;

function TProxyGenerator.CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := TInterfaceProxy.Create(
    proxyType, additionalInterfaces, options, nil, interceptors);
end;

{$ENDREGION}


{$REGION 'TAbstractInvocation'}

constructor TAbstractInvocation.Create(const target: TValue;
  const interceptors: TArray<IInterceptor>; const method: TRttiMethod;
  const arguments: TArray<TValue>);
begin
  inherited Create;
  fArguments := arguments;
  fCurrentInterceptorIndex := -1;
  fInterceptors := interceptors;
  fMethod := method;
  fTarget := target;
end;

function TAbstractInvocation.GetArguments: TArray<TValue>;
begin
  Result := fArguments;
end;

function TAbstractInvocation.GetMethod: TRttiMethod;
begin
  Result := fMethod;
end;

function TAbstractInvocation.GetResult: TValue;
begin
  Result := fResult;
end;

function TAbstractInvocation.GetTarget: TValue;
begin
  Result := fTarget;
end;

procedure TAbstractInvocation.Proceed;
begin
  if not Assigned(fInterceptors) then
    InvokeMethodOnTarget
  else
  begin
    Inc(fCurrentInterceptorIndex);
    try
      if fCurrentInterceptorIndex = Length(fInterceptors) then
        InvokeMethodOnTarget
      else if fCurrentInterceptorIndex > Length(fInterceptors) then
        raise EInvalidOperationException.CreateRes(@SProceedCalledMoreTimesThanExpected)
      else
        fInterceptors[fCurrentInterceptorIndex].Intercept(Self);
    finally
      Dec(fCurrentInterceptorIndex);
    end;
  end;
end;

procedure TAbstractInvocation.SetResult(const value: TValue);
begin
  fResult := value;
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions'}

class constructor TProxyGenerationOptions.Create;
begin
  fDefault := TProxyGenerationOptions.Create(TAllMethodsHook.Create);
  fDefault.Selector := TAllInterceptorsSelector.Create;
end;

constructor TProxyGenerationOptions.Create(
  const hook: IProxyGenerationHook);
begin
  fHook := hook;
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions.TAllMethodsHook'}

procedure TProxyGenerationOptions.TAllMethodsHook.NonVirtualMemberNotification(
  const method: TRttiMethod);
begin
end;

function TProxyGenerationOptions.TAllMethodsHook.ShouldInterceptMethod(
  const method: TRttiMethod): Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions.TAllInterceptorsSelector'}

function TProxyGenerationOptions.TAllInterceptorsSelector.SelectInterceptors(
  const method: TRttiMethod;
  const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
begin
  Result := interceptors;
end;

{$ENDREGION}


end.
