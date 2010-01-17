{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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
  Rtti,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TObject;
  end;

  TActivator = record
  public
    class function CreateInstance(instanceType: TRttiInstanceType;
      constructorMethod: TRttiMethod; const arguments: array of TValue): TObject; static;
  end;

  TInterfaceTypeRegistry = record
  strict private
    class var fContext: TRttiContext;
    class var fTypes: IDictionary<TGuid, TRttiInterfaceType>;
    class constructor Create;
  public
    class function TryGetType(const guid: TGUID; out aType: TRttiInterfaceType): Boolean; static;
  end;

  TGetRttiMembersFunc<T> = reference to function(targetType: TRttiType): TArray<T>;

  /// <summary>
  /// TRttiMemberEnumerable<T>
  /// </summary>
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

  TMemberFilters = class(TFiltersBase<TRttiMember>);
  TMethodFilters = class(TFiltersBase<TRttiMethod>);
  TPropertyFilters = class(TFiltersBase<TRttiProperty>);
  TFieldFilters = class(TFiltersBase<TRttiField>);

  TMemberSpecificationBase<T: TRttiMember> = class abstract(TSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; virtual; abstract;
  public
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  TNameFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fName: string;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  TInvokableFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  THasAttributeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  TTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  THasParameterTypesFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  TRttiMemberClass = class of TRttiMember;

  TMemberTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  TConstructorFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  TInstanceMethodFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  THasParameterFlagsFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

type
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


implementation

uses
//  Spring.Helpers,  // Internal Error
  Spring.ResourceStrings;


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

type
  TInterfacedObjectHack = class(TInterfacedObject);

class function TActivator.CreateInstance(instanceType: TRttiInstanceType;
  constructorMethod: TRttiMethod; const arguments: array of TValue): TObject;
var
  classType: TClass;
begin
  TArgument.CheckNotNull(instanceType, 'instanceType');
  TArgument.CheckNotNull(constructorMethod, 'constructorMethod');
  classType := instanceType.MetaclassType;
  Result := classType.NewInstance;
  try
    constructorMethod.Invoke(Result, arguments);
  except
    on Exception do
    begin
      if Result is TInterfacedObject then
      begin
        Dec(TInterfacedObjectHack(Result).FRefCount);
      end;
      Result.Free;
      raise;
    end;
  end;
  try
    Result.AfterConstruction;
  except
    on Exception do
    begin
      Result.Free;
      raise;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TInterfaceTypeRegistry'}

class constructor TInterfaceTypeRegistry.Create;
var
  types: TArray<TRttiType>;
  item: TRttiType;
begin
  fContext := TRttiContext.Create;
  types := fContext.GetTypes;
  fTypes := TCollections.CreateDictionary<TGuid, TRttiInterfaceType>;
  for item in types do
  begin
    if (item is TRttiInterfaceType) and (ifHasGuid in TRttiInterfaceType(item).IntfFlags) then
    begin
      if not fTypes.ContainsKey(TRttiInterfaceType(item).GUID) then  // TEMP
      begin
        fTypes.Add(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
      end;
    end;
  end;
end;

class function TInterfaceTypeRegistry.TryGetType(const guid: TGUID;
  out aType: TRttiInterfaceType): Boolean;
begin
  Result := fTypes.TryGetValue(guid, aType);
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

end.


