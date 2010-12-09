{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Reflection;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  TypInfo,
  SyncObjs,
  Rtti,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns;

type

  {$REGION 'TType'}

  {$REGION 'Documentation'}
  ///	<summary>Provides static methods to get RTTI information of a type.</summary>
  ///	<remarks>
  ///	  <note type="caller">When using this class, a shared instance of the
  ///	  TRttiContext class will be kept, which will make all instances of RTTI
  ///	  types live during the lifetime.</note>
  ///	</remarks>
  {$ENDREGION}
  TType = class
  strict private
    class var fContext: TRttiContext;
    class var fSection: TCriticalSection;
    class var fInterfaceTypes: TDictionary<TGuid, TRttiInterfaceType>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    class function GetType<T>: TRttiType; overload;
    class function GetType(typeInfo: PTypeInfo): TRttiType; overload;
    class function GetType(classType: TClass): TRttiType; overload;
    class function GetType(const value: TValue): TRttiType; overload;
//    class function GetTypes: IEnumerableEx<TRttiType>;
    class function GetFullName(typeInfo: PTypeInfo): string; overload;
    class function GetFullName<T>: string; overload;
    class function FindType(const qualifiedName: string): TRttiType;
    /// <summary>
    /// Returns true if the typeFrom is assignable to the typeTo.
    /// </summary>
    class function IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean; overload;
    class function TryGetInterfaceType(const guid: TGUID; out aType: TRttiInterfaceType): Boolean;
    class property Context: TRttiContext read fContext;
  end;

  IRttiPackage = interface
    ['{7365872F-36E1-424F-96F4-522357F0A9A4}']
    {$REGION 'Property Getters & Setters'}
      function GetHandle: HINST;
      function GetTypes: IEnumerableEx<TRttiType>;
    {$ENDREGION}

    property Handle: HINST read GetHandle;
    function FindType(const qualifiedName: string): TRttiType;
    property Types: IEnumerableEx<TRttiType> read GetTypes;
  end;

  IReflection = interface
    ['{E3B66C0B-4827-44C4-BDD9-27F1A856FDDD}']
    {$REGION 'Property Getters & Setters'}
      function GetTypes: IEnumerableEx<TRttiType>;
//      function GetPackages: IEnumerableEx<TRttiPackage>;
    {$ENDREGION}

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
//    function GetType(const instance: IInterface): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;
    property Types: IEnumerableEx<TRttiType> read GetTypes;
//    property Packages: IEnumerableEx<TRttiPackage> read GetPackages;
  end;

  TReflection = class(TInterfacedObject, IReflection)
  private
    fContext: TRttiContext;
    fTypes: IEnumerableEx<TRttiType>;
    function GetTypes: IEnumerableEx<TRttiType>;
//    function GetPackages: IEnumerableEx<TRttiPackage>;
  public
    constructor Create;

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;
    property Types: IEnumerableEx<TRttiType> read GetTypes;
//    property Packages: IEnumerableEx<TRttiPackage> read GetPackages;
  end;

  TRttiTypeEnumerable = class(TEnumerableEx<TRttiType>)
  protected
    function DoGetEnumerator: IEnumerator<TRttiType>; override;
  end;

  TRttiTypeEnumerator = class(TEnumeratorBase<TRttiType>)
  private
    fContext: TRttiContext;
    fTypes: TArray<TRttiType>;
    fIndex: Integer;
  protected
    function DoGetCurrent: TRttiType; override;
  public
    constructor Create;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'Activator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TObject;
  end;

  TActivator = record
  public
    class function CreateInstance(instanceType: TRttiInstanceType): TObject; overload; static;
    class function CreateInstance(const typeName: string): TObject; overload; static;
    class function CreateInstance(instanceType: TRttiInstanceType;
      constructorMethod: TRttiMethod; const arguments: array of TValue): TObject; overload; static;
  end;

  {$ENDREGION}


  TGetRttiMembersFunc<T> = reference to function(targetType: TRttiType): TArray<T>;


  {$REGION 'TRttiMemberEnumerable<T: TRttiMember>'}

  TRttiMemberEnumerable<T: TRttiMember> = class(TEnumerableEx<T>,
    IEnumerableEx<T>, IEnumerable<T>, IEnumerable, IInterface)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fCollection: TRttiMemberEnumerable<T>;
        fTargetType: TRttiType;
        fMembers: TArray<T>;
        fIndex: Integer;
      protected
        procedure Initialize(targetType: TRttiType);
        function DoGetCurrent: T; override;
      public
        constructor Create(collection: TRttiMemberEnumerable<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fParentType: TRttiType;
    fGetMembersFunc: TGetRttiMembersFunc<T>;
    fEnumerateBaseType: Boolean;
    fPredicate: TPredicate<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean); overload;
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean; const predicate: TPredicate<T>); overload;
    function Where(const predicate: TPredicate<T>): IEnumerableEx<T>; override;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersBase<T: TRttiMember>'}

  /// <summary>
  /// Provides static methods to create specifications to filter TRttiMember objects.
  /// </summary>
  TFiltersBase<T: TRttiMember> = class
  public
    class function HasAttribute(attributeClass: TAttributeClass): TSpecification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): TSpecification<T>;
    class function HasParameterFlags(const flags: TParamFlags): TSpecification<T>;
    class function IsNamed(const name: string): TSpecification<T>;
    class function IsTypeOf<TType>: TSpecification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>; overload;
    class function IsConstructor: TSpecification<T>;
    class function IsInstanceMethod: TSpecification<T>;
    class function IsInvokable: TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'Filters'}

  TMemberFilters = class(TFiltersBase<TRttiMember>);
  TMethodFilters = class(TFiltersBase<TRttiMethod>);
  TPropertyFilters = class(TFiltersBase<TRttiProperty>);
  TFieldFilters = class(TFiltersBase<TRttiField>);

  {$ENDREGION}


  {$REGION 'TMemberSpecificationBase<T: TRttiMember>'}

  TMemberSpecificationBase<T: TRttiMember> = class abstract(TSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; virtual; abstract;
  public
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T: TRttiMember>'}

  TNameFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fName: string;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T: TRttiMember>'}

  TInvokableFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T: TRttiMember>'}

  THasAttributeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  {$ENDREGION}


  {$REGION 'TTypeFilter<T: TRttiMember>'}

  TTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T: TRttiMember>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  TRttiMemberClass = class of TRttiMember;


  {$REGION 'TMemberTypeFilter<T: TRttiMember>'}

  TMemberTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T: TRttiMember>'}

  TConstructorFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T: TRttiMember>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T: TRttiMember>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


type

  {$REGION 'Internal Class Helpers'}

  /// <summary>
  /// The _InternalRttiMemberHelper class was copied from Spring.Helpers, as
  /// An URW1111 internal error will occured when the Spring.Helpers namespace
  /// was used by this unit.
  /// </summary>
  _InternalRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
  public
    function AsProperty: TRttiProperty;
    function AsMethod: TRttiMethod;
    function AsField: TRttiField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  {$ENDREGION}


implementation

uses
  //Spring.Helpers,  // Internal Error
  StrUtils,
  Character,
  Spring.ResourceStrings;


{$REGION 'TType'}

class constructor TType.Create;
begin
  fContext := TRttiContext.Create;
  fSection := TCriticalSection.Create;
end;

class destructor TType.Destroy;
begin
  fInterfaceTypes.Free;
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
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  Result := fContext.GetType(typeInfo).QualifiedName;
end;

class function TType.GetFullName<T>: string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  Result := TType.GetFullName(typeInfo);
end;

class function TType.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
end;

class function TType.IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean;
var
  dataFrom, dataTo: PTypeData;
begin
  TArgument.CheckNotNull(typeFrom, 'typeFrom');
  TArgument.CheckNotNull(typeTo, 'typeTo');
  if typeFrom = typeTo then
  begin
    Exit(True);
  end;
  dataFrom := TypInfo.GetTypeData(typeFrom);
  dataTo := TypInfo.GetTypeData(typeTo);
  if (typeFrom.Kind = tkClass) and (typeTo.Kind = tkClass) then
  begin
    Result := dataFrom.ClassType.InheritsFrom(dataTo.ClassType);
  end
  else if (typeFrom.Kind = tkClass) and (typeTo.Kind = tkInterface) then
  begin
    Result := (ifHasGuid in dataTo.IntfFlags) and
      Supports(dataFrom.ClassType, dataTo.Guid);
  end
  else if (typeFrom.Kind = tkInterface) and (typeTo.Kind = tkInterface) then
  begin
    Result := Assigned(dataFrom.IntfParent) and (dataFrom.IntfParent^ = typeTo);
    while not Result and Assigned(dataFrom.IntfParent) do
    begin
      Result := dataFrom.IntfParent^ = typeTo;
      dataFrom := TypInfo.GetTypeData(dataFrom.IntfParent^);
    end;
  end
  else
  begin
    Result := False;
  end;
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
        fInterfaceTypes := TDictionary<TGuid, TRttiInterfaceType>.Create;
        for item in fContext.GetTypes do
        begin
          if (item is TRttiInterfaceType) and (ifHasGuid in TRttiInterfaceType(item).IntfFlags) then
          begin
            if not fInterfaceTypes.ContainsKey(TRttiInterfaceType(item).GUID) then  // TEMP
            begin
              fInterfaceTypes.Add(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
            end;
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

{ TInternalRttiMemberHelper }

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

function _InternalRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function _InternalRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function _InternalRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function _InternalRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;


{$ENDREGION}


{$REGION 'TActivator'}

class function TActivator.CreateInstance(instanceType: TRttiInstanceType;
  constructorMethod: TRttiMethod; const arguments: array of TValue): TObject;
begin
  TArgument.CheckNotNull(instanceType, 'instanceType');
  TArgument.CheckNotNull(constructorMethod, 'constructorMethod');
  Result := constructorMethod.Invoke(instanceType.MetaclassType, arguments).AsObject;
end;

class function TActivator.CreateInstance(const typeName: string): TObject;
var
  context: TRttiContext;
  typeObj: TRttiType;
begin
  Result := nil;
  context := TRttiContext.Create;
  try
    typeObj := context.FindType(typeName);
    if not (typeObj is TRttiInstanceType) then
    begin
      Exit;
    end;
    Result := TActivator.CreateInstance(TRttiInstanceType(typeObj));
  finally
    context.Free;
  end;
end;

class function TActivator.CreateInstance(
  instanceType: TRttiInstanceType): TObject;
var
  method: TRttiMethod;
begin
  TArgument.CheckNotNull(instanceType, 'instanceType');
  Result := nil;
  for method in instanceType.GetMethods do
  begin
    if method.IsConstructor and (Length(method.GetParameters) = 0) then
    begin
      Result := method.Invoke(instanceType.MetaclassType, []).AsObject;
      Break;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TRttiMemberEnumerable<T>'}

constructor TRttiMemberEnumerable<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean);
begin
  Create(parentType, func, enumerateBaseType, nil);
end;

constructor TRttiMemberEnumerable<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean;
  const predicate: TPredicate<T>);
begin
  inherited Create;
  fParentType := parentType;
  fGetMembersFunc := func;
  fEnumerateBaseType := enumerateBaseType;
  fPredicate := predicate;
end;

function TRttiMemberEnumerable<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TRttiMemberEnumerable<T>.Where(
  const predicate: TPredicate<T>): IEnumerableEx<T>;
var
  finalPredicate: TPredicate<T>;
begin
  if not Assigned(fPredicate) then
  begin
    finalPredicate := predicate;
  end
  else
  begin
    finalPredicate :=
      function(const value: T): Boolean
      begin
        Result := fPredicate(value) and predicate(value);
      end;
  end;
  Result := TRttiMemberEnumerable<T>.Create(fParentType, fGetMembersFunc,
    fEnumerateBaseType, finalPredicate);
end;

{ TRttiMemberEnumerableEx<T>.TEnumerator }

constructor TRttiMemberEnumerable<T>.TEnumerator.Create(
  collection: TRttiMemberEnumerable<T>);
begin
  inherited Create;
  fCollection := collection;
  Initialize(fCollection.fParentType);
end;

procedure TRttiMemberEnumerable<T>.TEnumerator.Initialize(
  targetType: TRttiType);
begin
  fTargetType := targetType;
  if Assigned(fTargetType) then
  begin
    fMembers := fCollection.fGetMembersFunc(fTargetType);
  end
  else
  begin
    SetLength(fMembers, 0);
  end;
  fIndex := -1;
end;

function TRttiMemberEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < Length(fMembers) - 1;
  if Result then
  begin
    Inc(fIndex);
    if Assigned(fCollection.fPredicate) and not fCollection.fPredicate(Current) then
    begin
      Result := MoveNext;
    end;
  end
  else if fCollection.fEnumerateBaseType and (fTargetType <> nil) then
  begin
    Initialize(fTargetType.BaseType);
    Exit(MoveNext);
  end;
end;

function TRttiMemberEnumerable<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := fMembers[fIndex];
end;

{$ENDREGION}


{$REGION 'TMemberSpecificationBase<T>'}

function TMemberSpecificationBase<T>.IsSatisfiedBy(
  const member: T): Boolean;
begin
//  TArgument.CheckNotNull<T>(member, 'member');
  Result := Accept(member);
end;

{$ENDREGION}


{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.HasAttribute(
  attributeClass: TAttributeClass): TSpecification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass);
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

class function TFiltersBase<T>.IsNamed(const name: string): TSpecification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: TSpecification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
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

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass);
begin
  inherited Create;
  fAttributeClass := attributeClass;
end;

function THasAttributeFilter<T>.Accept(const member: T): Boolean;
var
  attribute: TCustomAttribute;
begin
  Result := False;
  for attribute in member.GetAttributes do
  begin
    if attribute.InheritsFrom(fAttributeClass) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.Accept(const member: T): Boolean;
begin
  Result := SameText(member.Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo;
  end
  else if member.IsField then
  begin
    Result := member.AsField.FieldType.Handle = fTypeInfo;
  end
  else
  begin
    Result := False;
  end;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fTypes, Length(types));
  for i := 0 to High(types) do
  begin
    fTypes[i] := types[i];
  end;
end;

function THasParameterTypesFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
  for i := 0 to Length(parameters) - 1 do
  begin
    if parameters[i].ParamType.Handle <> fTypes[i] then  // IsAssignableFrom
    begin
      Result := False;
      Break;
    end;
  end;
end;

{ THasParameterFlagsFilter }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
    begin
      if parameter.Flags * fFlags <> [] then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.IsWritable;
  end
  else if member.IsMethod then
  begin
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor]);
  end
  else
  begin
    Result := True;
  end;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter }

function TConstructorFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter }

function TInstanceMethodFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{$ENDREGION}


{$REGION 'TReflection'}

constructor TReflection.Create;
begin
  fContext := TRttiContext.Create;
  fTypes := TRttiTypeEnumerable.Create;
end;

function TReflection.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
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

function TReflection.GetType(const typeInfo: PTypeInfo): TRttiType;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  Result := fContext.GetType(typeInfo);
end;

function TReflection.GetType(const classType: TClass): TRttiType;
begin
  TArgument.CheckNotNull(classType, 'classType');
  Result := fContext.GetType(classType.ClassInfo);
end;

function TReflection.GetType(const instance: TObject): TRttiType;
begin
  TArgument.CheckNotNull(instance, 'instance');
  Result := fContext.GetType(instance.ClassInfo);
end;

function TReflection.GetType(const instance: TValue): TRttiType;
begin
  Result := fContext.GetType(instance.TypeInfo);
end;

function TReflection.GetTypes: IEnumerableEx<TRttiType>;
begin
  Result := fTypes;
end;

{$ENDREGION}


{ TRttiTypeEnumerable }

function TRttiTypeEnumerable.DoGetEnumerator: IEnumerator<TRttiType>;
begin
  Result := TRttiTypeEnumerator.Create;
end;


{ TRttiTypeEnumerator<T> }

constructor TRttiTypeEnumerator.Create;
begin
  fContext := TRttiContext.Create;
  fTypes := fContext.GetTypes;
  fIndex := -1;
end;

function TRttiTypeEnumerator.DoGetCurrent: TRttiType;
begin
  Result := fTypes[fIndex];
end;

function TRttiTypeEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < Length(fTypes) - 1;
  if Result then
    Inc(fIndex);
end;

end.
