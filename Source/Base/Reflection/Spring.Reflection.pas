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

{$I Spring.inc}

unit Spring.Reflection;

interface

uses
  Rtti,
  SyncObjs,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Extensions,
  Spring.DesignPatterns;

type

  ///	<summary>
  ///	  Specifies the kind of a lazy type.
  ///	</summary>
  TLazyKind = (
    ///	<summary>
    ///	  Not a lazy type.
    ///	</summary>
    lkNone,

    ///	<summary>
    ///	  Type is <see cref="SysUtils|TFunc&lt;T&gt;" />.
    ///	</summary>
    lkFunc,

    ///	<summary>
    ///	  Type is <see cref="Spring|Lazy&lt;T&gt;" />.
    ///	</summary>
    lkRecord,

    ///	<summary>
    ///	  Type is <see cref="Spring|ILazy&lt;T&gt;" />.
    ///	</summary>
    lkInterface
  );

  {$REGION 'TType'}

  ///	<summary>
  ///	  Provides static methods to get RTTI information of a type.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="caller">
  ///	    When using this class, a shared instance of the TRttiContext class
  ///	    will be kept, which will make all instances of RTTI types live during
  ///	    the lifetime.
  ///	  </note>
  ///	</remarks>
  TType = class
  strict private
    class var fContext: TRttiContext;
    class var fSection: TCriticalSection;
    class var fInterfaceTypes: IDictionary<TGuid, TRttiInterfaceType>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    class function GetType<T>: TRttiType; overload;
    class function GetType(typeInfo: PTypeInfo): TRttiType; overload;
    class function GetType(classType: TClass): TRttiType; overload;
    class function GetType(const value: TValue): TRttiType; overload;
//    class function GetTypes: IEnumerable<TRttiType>;
    class function GetFullName(typeInfo: PTypeInfo): string; overload;
    class function GetFullName<T>: string; overload;
    class function FindType(const qualifiedName: string): TRttiType;

    ///	<summary>
    ///	  Returns true if the typeFrom is assignable to the typeTo.
    ///	</summary>
    class function IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean; inline;

    ///	<summary>
    ///	  Returns <c>True</c> if the typeInfo is a delegate type.
    ///	</summary>
    class function IsDelegate(typeInfo: PTypeInfo): Boolean; overload;
    class function TryGetInterfaceType(const guid: TGUID; out aType: TRttiInterfaceType): Boolean;

    ///	<summary>
    ///	  Returns the <see cref="TLazyKind" /> of the typeInfo.
    ///	</summary>
    class function GetLazyKind(typeInfo: PTypeInfo): TLazyKind;

    ///	<summary>
    ///	  Returns the underlying type name of the lazy type.
    ///	</summary>
    class function GetLazyTypeName(typeInfo: PTypeInfo): string;

    ///	<summary>
    ///	  Returns <c>True</c> if the type is a lazy type.
    ///	</summary>
    class function IsLazy(typeInfo: PTypeInfo): Boolean;

    class property Context: TRttiContext read fContext;
  end;

//  IRttiPackage = interface
//    ['{7365872F-36E1-424F-96F4-522357F0A9A4}']
//    {$REGION 'Property Accessors
//      function GetHandle: HINST;
//      function GetTypes: IEnumerable<TRttiType>;
//    {$ENDREGION}
//
//    property Handle: HINST read GetHandle;
//    function FindType(const qualifiedName: string): TRttiType;
//    property Types: IEnumerable<TRttiType> read GetTypes;
//  end;

  IReflection = interface
    ['{E3B66C0B-4827-44C4-BDD9-27F1A856FDDD}']
  {$REGION 'Property Accessors'}
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
  {$ENDREGION}

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
//    function GetType(const interfaceGuid: TGuid): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;

    function FindType(const qualifiedName: string): TRttiType;

//    function FindAllWhere(): IEnumerable<TRttiType>;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TReflection = class(TInterfacedObject, IReflection)
  strict private
    class var fContext: TRttiContext;
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TRttiTypeIterator<T: TRttiType> = class(TIterator<T>)
  private
    fContext: TRttiContext;
    fIndex: Integer;
    fTypes: TArray<TRttiType>;
  public
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMemberIterator<T>'}

  TRttiMemberIterator<T: TRttiMember> = class(TIterator<T>)
  private
    fParentType: TRttiType;
    fSelector: TFunc<TRttiType,TArray<T>>;
    fEnumerateBaseType: Boolean;
    fPredicate: TPredicate<T>;
    fTargetType: TRttiType;
    fMembers: TArray<T>;
    fIndex: Integer;
    procedure Initialize(const targetType: TRttiType);
  public
    constructor Create(const parentType: TRttiType;
      const selector: TFunc<TRttiType,TArray<T>>;
      enumerateBaseType: Boolean); overload;
    constructor Create(const parentType: TRttiType;
      const selector: TFunc<TRttiType,TArray<T>>;
      enumerateBaseType: Boolean;
      const predicate: TPredicate<T>); overload;

    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersNamed<T>'}

  TFiltersNamed<T: TRttiNamedObject> = class
  public
    class function IsNamed(const name: string): TSpecification<T>;
    class function HasAttribute(attributeClass: TAttributeClass): TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersBase<T>'}

  TMethodKinds = set of TMethodKind;

  ///	<summary>
  ///	  Provides static methods to create specifications to filter TRttiMember
  ///	  objects.
  ///	</summary>
  TFiltersBase<T: TRttiMember> = class(TFiltersNamed<T>)
  public
    class function ContainsParameterType(typeInfo: PTypeInfo): TSpecification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): TSpecification<T>;
    class function HasParameterFlags(const flags: TParamFlags): TSpecification<T>;
    class function IsTypeOf<TType>: TSpecification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>; overload;
    class function IsConstructor: TSpecification<T>;
    class function IsInstanceMethod: TSpecification<T>;
    class function IsClassMethod: TSpecification<T>;
    class function IsMethodKind(const kinds : TMethodKinds): TSpecification<T>;
    class function IsInvokable: TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'Filters'}

  TPackageFilters = class(TFiltersNamed<TRttiPackage>);
  TMemberFilters = class(TFiltersBase<TRttiMember>);
  TMethodFilters = class(TFiltersBase<TRttiMethod>);
  TPropertyFilters = class(TFiltersBase<TRttiProperty>);
  TFieldFilters = class(TFiltersBase<TRttiField>);
  TTypeFilters = class(TFiltersNamed<TRttiType>)
  public
    class function IsClass : TSpecification<TRttiType>;
    class function IsInterface : TSpecification<TRttiType>;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T>'}

  TNameFilter<T: TRttiNamedObject> = class(TSpecificationBase<T>)
  private
    fName: string;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T>'}

  TInvokableFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T>'}

  THasAttributeFilter<T: TRttiObject> = class(TSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  {$ENDREGION}


  {$REGION 'TTypeFilter<T>'}

  TTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'TContainsParameterTypeFilter<T>'}

  TContainsParameterTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  TRttiMemberClass = class of TRttiMember;


  {$REGION 'TMemberTypeFilter<T>'}

  TMemberTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T>'}

  TConstructorFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TClassMethodFilter<T>'}

  TClassMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'TMethodKindFilter<T>'}

  TMethodKindFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TMethodKinds;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TMethodKinds);
  end;

  {$ENDREGION}


  {$REGION 'TIsClassFilter>'}

  TIsClassFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TIsInterfaceFilter>'}

  TIsInterfaceFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'Internal Class Helpers'}

  /// <summary>
  ///   This helper was copied from Spring.Helpers.pas because URW1111 internal
  ///   error will occur when Spring.Helpers.pas is used by this unit.
  /// </summary>
  _InternalRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsConstructor: Boolean; inline;
    function GetIsProperty: Boolean; inline;
    function GetIsMethod: Boolean; inline;
    function GetIsField: Boolean; inline;
  public
    function AsProperty: TRttiProperty; inline;
    function AsMethod: TRttiMethod; inline;
    function AsField: TRttiField; inline;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv;
  IsConst: Boolean = False): Boolean;
procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);

  {$ENDREGION}


implementation

uses
  RTLConsts,
  StrUtils,
  Spring.ResourceStrings;


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
    tkVariant:
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
{$IFNDEF NEXTGEN}
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
{$ENDIF}
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


{$REGION 'TType'}

class constructor TType.Create;
begin
  fContext := TRttiContext.Create;
  fSection := TCriticalSection.Create;
end;

class destructor TType.Destroy;
begin
  fSection.Free;
  fContext.Free;
end;

class function TType.GetType<T>: TRttiType;
begin
  Result := GetType(TypeInfo(T));
end;

class function TType.GetType(typeInfo: PTypeInfo): TRttiType;
begin
  Result := fContext.GetType(typeInfo);
end;

class function TType.GetType(classType: TClass): TRttiType;
begin
  Result := fContext.GetType(classType);
end;

class function TType.GetType(const value: TValue): TRttiType;
begin
  Result := GetType(value.TypeInfo);
end;

class function TType.GetFullName(typeInfo: PTypeInfo): string;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  Result := fContext.GetType(typeInfo).QualifiedName;
end;

class function TType.GetFullName<T>: string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  Result := TType.GetFullName(typeInfo);
end;

const
  LazyPrefixStrings: array[lkFunc..High(TLazyKind)] of string = (
    'TFunc<', 'Lazy<', 'ILazy<');

class function TType.GetLazyKind(typeInfo: PTypeInfo): TLazyKind;
var
  name: string;
begin
  if Assigned(typeInfo) then
  begin
    name := GetTypeName(typeInfo);
    for Result := lkFunc to High(TLazyKind) do
      if StartsText(LazyPrefixStrings[Result], name) then
        Exit;
  end;
  Result := lkNone;
end;

class function TType.GetLazyTypeName(typeInfo: PTypeInfo): string;
var
  lazyKind: TLazyKind;
  name: string;
  i: Integer;
begin
  lazyKind := GetLazyKind(typeInfo);
  name := GetTypeName(typeInfo);
  if lazyKind > lkNone then
  begin
    i := Length(LazyPrefixStrings[lazyKind]) + 1;
    Result := Copy(name, i, Length(name) - i )
  end;
end;

class function TType.FindType(const qualifiedName: string): TRttiType;
var
  item: TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
  if not Assigned(Result) then
    for item in fContext.GetTypes do
      if SameText(item.Name, qualifiedName) then
        Exit(item);
end;

class function TType.IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean;
begin
  Result := IsAssignableFrom(typeTo, typeFrom);
end;

class function TType.IsDelegate(typeInfo: PTypeInfo): Boolean;
const
  DelegatePrefixStrings: array[0..2] of string = (
    'TFunc<', 'TProc<', 'TPredicate<');
var
  name: string;
  prefix: string;
  typeData: PTypeData;
begin
  while Assigned(typeInfo) and (typeInfo.Kind = tkInterface) do
  begin
    name := GetTypeName(typeInfo);
    for prefix in DelegatePrefixStrings do
      if StartsText(prefix, name) then
        Exit(True);
    typeData := GetTypeData(typeInfo);
    if Assigned(typeData) and Assigned(typeData.IntfParent) then
      typeInfo := typeData.IntfParent^
    else
      typeInfo := nil;
  end;
  Result := False;
end;

class function TType.IsLazy(typeInfo: PTypeInfo): Boolean;
begin
  Result := GetLazyKind(typeInfo) <> lkNone;
end;

class function TType.TryGetInterfaceType(const guid: TGUID;
  out aType: TRttiInterfaceType): Boolean;
var
  item: TRttiType;
begin
  if fInterfaceTypes = nil then
  begin
    fSection.Enter;
    try
      MemoryBarrier;
      if fInterfaceTypes = nil then
      begin
        fInterfaceTypes := TCollections.CreateDictionary<TGuid, TRttiInterfaceType>;
        for item in fContext.GetTypes do
        begin
          if (item is TRttiInterfaceType) and (ifHasGuid in TRttiInterfaceType(item).IntfFlags)
            and not fInterfaceTypes.ContainsKey(TRttiInterfaceType(item).GUID) then
          begin
            fInterfaceTypes.Add(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
          end;
        end;
      end;
    finally
      fSection.Leave;
    end;
  end;
  Result := fInterfaceTypes.TryGetValue(guid, aType);
end;

{$ENDREGION}


{$REGION 'Internal Class Helpers'}

function _InternalRttiMemberHelper.AsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function _InternalRttiMemberHelper.AsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function _InternalRttiMemberHelper.AsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

function _InternalRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function _InternalRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function _InternalRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function _InternalRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

{$ENDREGION}


{$REGION 'TRttiMemberIterator<T>'}

constructor TRttiMemberIterator<T>.Create(const parentType: TRttiType;
  const selector: TFunc<TRttiType,TArray<T>>; enumerateBaseType: Boolean);
begin
  Create(parentType, selector, enumerateBaseType, nil);
end;

constructor TRttiMemberIterator<T>.Create(const parentType: TRttiType;
  const selector: TFunc<TRttiType, TArray<T>>; enumerateBaseType: Boolean;
  const predicate: TPredicate<T>);
begin
  inherited Create;
  fParentType := parentType;
  fSelector := selector;
  fEnumerateBaseType := enumerateBaseType;
  fPredicate := predicate;
end;

function TRttiMemberIterator<T>.Clone: TIterator<T>;
begin
  Result := TRttiMemberIterator<T>.Create(
    fParentType, fSelector, fEnumerateBaseType, fPredicate);
end;

procedure TRttiMemberIterator<T>.Initialize(const targetType: TRttiType);
begin
  fIndex := -1;
  fTargetType := targetType;
  if Assigned(fTargetType) then
    fMembers := fSelector(fTargetType)
  else
    SetLength(fMembers, 0);
end;

function TRttiMemberIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    Initialize(fParentType);
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    repeat
      while fIndex < High(fMembers) do
      begin
        Inc(fIndex);
        if Assigned(fPredicate) and not fPredicate(fMembers[fIndex]) then
          Continue;
        fCurrent := fMembers[fIndex];
        Exit(True);
      end;
      if fEnumerateBaseType then
        Initialize(fTargetType.BaseType)
      else
        Initialize(nil);
    until not Assigned(fTargetType);
    fCurrent := Default(T);
    fState := STATE_FINISHED;
  end;
end;

{$ENDREGION}


{$REGION 'TFiltersNamed<T>'}

class function TFiltersNamed<T>.IsNamed(const name: string): TSpecification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersNamed<T>.HasAttribute(
  attributeClass: TAttributeClass): TSpecification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass);
end;

{$ENDREGION}


{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.ContainsParameterType(
  typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TContainsParameterTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.HasParameterTypes(
  const types: array of PTypeInfo): TSpecification<T>;
begin
  Result := THasParameterTypesFilter<T>.Create(types);
end;

class function TFiltersBase<T>.HasParameterFlags(
  const flags: TParamFlags): TSpecification<T>;
begin
  Result := THasParameterFlagsFilter<T>.Create(flags);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: TSpecification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
end;

class function TFiltersBase<T>.IsClassMethod: TSpecification<T>;
begin
  Result := TClassMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsConstructor: TSpecification<T>;
begin
  Result := TConstructorFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInstanceMethod: TSpecification<T>;
begin
  Result := TInstanceMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInvokable: TSpecification<T>;
begin
  Result := TInvokableFilter<T>.Create;
end;

class function TFiltersBase<T>.IsMethodKind(
  const kinds: TMethodKinds): TSpecification<T>;
begin
  Result := TMethodKindFilter<T>.Create(kinds);
end;

{$ENDREGION}


{$REGION 'TTypeFilters'}

class function TTypeFilters.IsClass: TSpecification<TRttiType>;
begin
  Result := TIsClassFilter.Create;
end;

class function TTypeFilters.IsInterface: TSpecification<TRttiType>;
begin
  Result := TIsInterfaceFilter.Create;
end;

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter<T> }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass);
begin
  inherited Create;
  fAttributeClass := attributeClass;
end;

function THasAttributeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  attribute: TCustomAttribute;
begin
  for attribute in member.GetAttributes do
    if attribute.InheritsFrom(fAttributeClass) then
      Exit(True);
  Result := False;
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := SameText(TRttiNamedObject(member).Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo
  else if member.IsField then
    Result := member.AsField.FieldType.Handle = fTypeInfo
  else
    Result := False;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fTypes, Length(types));
  for i := Low(types) to High(types) do
    fTypes[i] := types[i];
end;

function THasParameterTypesFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
    for i := Low(parameters) to High(parameters) do
      if parameters[i].ParamType.Handle <> fTypes[i] then  // IsAssignableFrom
        Exit(False);
end;

{ TContainsParameterTypeFilter<T> }

constructor TContainsParameterTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TContainsParameterTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.ParamType.Handle = fTypeInfo then
        Exit(True);
  end;
end;

{ THasParameterFlagsFilter<T> }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.Flags * fFlags <> [] then
        Exit(True);
  end;
end;

{ TMethodKindFilter<T> }

constructor TMethodKindFilter<T>.Create(const flags: TMethodKinds);
begin
  inherited Create;
  fFlags:=flags;
end;

function TMethodKindFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and (member.AsMethod.MethodKind in fFlags);
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.IsWritable
  else if member.IsMethod then
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor])
  else
    Result := True;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter<T> }

function TConstructorFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter<T> }

function TInstanceMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{ TClassMethodFilter<T> }

function TClassMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and member.AsMethod.IsClassMethod;
end;

{ TIsClassFilter }

function TIsClassFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member.IsInstance;
end;

{ TIsInterfaceFilter }

function TIsInterfaceFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member is TRttiInterfaceType;
end;

{$ENDREGION}


{$REGION 'TReflection'}

class constructor TReflection.Create;
begin
  fContext := TRttiContext.Create;
end;

class destructor TReflection.Destroy;
begin
  fContext.Free;
end;

function TReflection.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
end;

function TReflection.GetClasses: IEnumerable<TRttiInstanceType>;
begin
  Result := TRttiTypeIterator<TRttiInstanceType>.Create;
end;

function TReflection.GetFullName(const typeInfo: PTypeInfo): string;
var
  t: TRttiType;
begin
  t := fContext.GetType(typeInfo);
  if t = nil then
    Exit('');
  if t.IsPublicType then
    Result := t.QualifiedName
  else
    Result := t.Name;
end;

function TReflection.GetInterfaces: IEnumerable<TRttiInterfaceType>;
begin
  Result := TRttiTypeIterator<TRttiInterfaceType>.Create;
end;

function TReflection.GetType(const typeInfo: PTypeInfo): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  Result := fContext.GetType(typeInfo);
end;

function TReflection.GetType(const classType: TClass): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

  Result := fContext.GetType(classType.ClassInfo);
end;

function TReflection.GetType(const instance: TObject): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

  Result := fContext.GetType(instance.ClassInfo);
end;

function TReflection.GetType(const instance: TValue): TRttiType;
begin
  Result := fContext.GetType(instance.TypeInfo);
end;

function TReflection.GetTypes: IEnumerable<TRttiType>;
begin
  Result := TRttiTypeIterator<TRttiType>.Create;
end;

{$ENDREGION}


{$REGION 'TRttiTypeIterator<T>'}

function TRttiTypeIterator<T>.Clone: TIterator<T>;
begin
  Result := TRttiTypeIterator<T>.Create;
end;

function TRttiTypeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fTypes := fContext.GetTypes;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fIndex < High(fTypes) do
    begin
      Inc(fIndex);
      if not (fTypes[fIndex].InheritsFrom(T)) then
        Continue;
      fCurrent := T(fTypes[fIndex]);
      Exit(True);
    end;
  end;
end;

{$ENDREGION}


end.
