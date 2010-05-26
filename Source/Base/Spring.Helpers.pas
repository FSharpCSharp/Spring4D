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

unit Spring.Helpers;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Types,
  TypInfo,
  Rtti,
  ComObj,
  DB,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Reflection;

type
  /// <summary>
  /// Record helper for TGuid
  /// </summary>
  /// <remarks>
  /// Note: We have reported an enhancement request to QC. (See QC#78515)
  /// </remarks>
  TGuidHelper = record helper for TGuid
  private
    class function GetEmpty: TGuid; static;
    function GetIsEmpty: Boolean;
  public
    class function Create(const guidString: string): TGuid; static;
    class function NewGuid: TGuid; static;
    function Equals(const guid: TGuid): Boolean;
    function ToString: string;
    function ToQuotedString: string;
    property IsEmpty: Boolean read GetIsEmpty;
    class property Empty: TGuid read GetEmpty;
//    class operator Equal(const left, right: TGuid) : Boolean;
//    class operator NotEqual(const left, right: TGuid) : Boolean;
  end;

  TMethodHelper = record helper for TMethod
  public
    class function Create(const objectAddress, methodAddress: Pointer): TMethod; static;
  end;

  TArrayHelper = class helper for TArray
  public
    class function CreateArray<T>(const values: array of T): TArray<T>;
  end; // deprecated;

  TStreamHelper = class helper for TStream
  public
    procedure ReadBuffer<T: record>(var value: T); overload;
    procedure WriteBuffer<T: record>(const value: T); overload;
  end;

  TStringsHelper = class helper for TStrings
  private
    function GetIsEmpty: Boolean;
  public
    procedure AddStrings(const strings: array of string); overload;
    procedure AddOrUpdate(const name, value: string);
//    procedure Remove(const s: string);
    procedure ExecuteUpdate(proc: TProc);
    procedure ExtractNames(strings: TStrings);
    procedure ExtractValues(strings: TStrings);
    function GetNames: TStringDynArray;
    function GetValues: TStringDynArray;
    function ContainsName(const name: string): Boolean;
    function ContainsValue(const value: string): Boolean;
    function ContainsObject(obj: TObject): Boolean;
    function GetValueOrDefault<T>(const name: string; const default: T): T;
    function ToArray: TStringDynArray;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TDataSetHelper = class helper for TDataSet
  public
    function GetValueOrDefault<T>(const fieldName: string; const default: T): T;
    procedure CopyRecordFrom(source: TDataSet);
//    procedure EnumerateRows(proc: TProc<TDataSet>);
//    procedure Clear;
//    property IsModified: Boolean;
  end;

  TFieldHelper = class helper for TField
  public
    function GetValueOrDefault<T>(const default: T): T;
//    property IsNullOrWhiteSpace: Boolean;
//    property IsModified: Boolean;
  end;

  // TPointHelper, TSizeHelper, TRectHelper


  {$REGION 'Class helpers for Enhanced Rtti (Reflection)'}

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetCustomAttributes<T: TCustomAttribute>: TArray<T>;
    function GetCustomAttribute<T: TCustomAttribute>: T;
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T): Boolean;
    function HasCustomAttribute<T: TCustomAttribute>: Boolean;
  end;

  TRttiClassType = TRttiInstanceType;

  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
    function GetAsClass: TRttiInstanceType;
    function GetIsGenericType: Boolean;
  protected
    function InternalGetConstructors(enumerateBaseType: Boolean = True): IEnumerableEx<TRttiMethod>;
    function InternalGetMethods(enumerateBaseType: Boolean = True): IEnumerableEx<TRttiMethod>;
    function InternalGetProperties(enumerateBaseType: Boolean = True): IEnumerableEx<TRttiProperty>;
    function InternalGetFields(enumerateBaseType: Boolean = True): IEnumerableEx<TRttiField>;
    function GetConstructors: IEnumerableEx<TRttiMethod>;
    function GetMethods: IEnumerableEx<TRttiMethod>;
    function GetProperties: IEnumerableEx<TRttiProperty>;
    function GetFields: IEnumerableEx<TRttiField>;
  public
    // function GetMembers: IEnumerableEx<TRttiMember>;
    function GetInterfaces: IEnumerableEx<TRttiInterfaceType>;
    function GetGenericArguments: TArray<TRttiType>;

    property Constructors: IEnumerableEx<TRttiMethod> read GetConstructors;
    property Methods: IEnumerableEx<TRttiMethod> read GetMethods;
    property Properties: IEnumerableEx<TRttiProperty> read GetProperties;
    property Fields: IEnumerableEx<TRttiField> read GetFields;

    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property AsClass: TRttiInstanceType read GetAsClass;
    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property IsGenericType: Boolean read GetIsGenericType;
  end;

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
    function GetAsMethod: TRttiMethod;
    function GetAsProperty: TRttiProperty;
    function GetAsField: TRttiField;
  public
//    procedure InvokeMember(instance: TValue; const arguments: array of TValue);
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
    property AsMethod: TRttiMethod read GetAsMethod;
    property AsProperty: TRttiProperty read GetAsProperty;
    property AsField: TRttiField read GetAsField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    property HasGuid: Boolean read GetHasGuid;
  end;

  {$ENDREGION}

implementation

uses
  Spring.ResourceStrings;

{$REGION 'TGuidHelper'}

class function TGuidHelper.Create(const guidString: string): TGuid;
begin
  Result := StringToGUID(guidString);
end;

class function TGuidHelper.NewGuid: TGuid;
begin
  ComObj.OleCheck(SysUtils.CreateGUID(Result));
end;

function TGuidHelper.Equals(const guid: TGuid): Boolean;
begin
  Result := SysUtils.IsEqualGUID(Self, guid);
end;

function TGuidHelper.GetIsEmpty: Boolean;
begin
  {$WARNINGS OFF}
  Result := Self.Equals(TGuid.Empty);
  {$WARNINGS ON}
end;

function TGuidHelper.ToString: string;
begin
  Result := SysUtils.GUIDToString(Self);
end;

function TGuidHelper.ToQuotedString: string;
begin
  {$WARNINGS OFF}
  Result := QuotedStr(Self.ToString);
  {$WARNINGS ON}
end;

class function TGuidHelper.GetEmpty: TGuid;
const
  EmptyGuid: TGUID = (
    D1: 0;
    D2: 0;
    D3: 0;
    D4: (0, 0, 0, 0, 0, 0, 0, 0);
  );
begin
  Result := EmptyGuid;
end;

//class operator TGuidHelper.Equal(const left, right: TGuid) : Boolean;
//begin
//  Result := left.Equals(right);
//end;

//class operator TGuidHelper.NotEqual(const left, right: TGuid) : Boolean;
//begin
//  Result := not left.Equals(right);
//end;

{$ENDREGION}


{$REGION 'TMethodHelper'}

class function TMethodHelper.Create(const objectAddress,
  methodAddress: Pointer): TMethod;
begin
  Result.Code := methodAddress;
  Result.Data := objectAddress;
end;

{$ENDREGION}


{$REGION 'TArrayHelper'}

class function TArrayHelper.CreateArray<T>(const values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := 0 to High(values) do
  begin
    Result[i] := values[i];
  end;
end;

{$ENDREGION}


{$REGION 'Classes'}

{ TStreamHelper }

procedure TStreamHelper.ReadBuffer<T>(var value: T);
begin
  ReadBuffer(value, SizeOf(T));
end;

procedure TStreamHelper.WriteBuffer<T>(const value: T);
begin
  WriteBuffer(value, SizeOf(T));
end;

{ TStringsHelper }

procedure TStringsHelper.AddStrings(const strings: array of string);
var
  s: string;
begin
  BeginUpdate;
  try
    for s in strings do
    begin
      Add(s);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.AddOrUpdate(const name, value: string);
var
  index: Integer;
begin
  index := IndexOfName(name);
  if index <> -1 then
  begin
    Strings[index] := name + NameValueSeparator + value;
  end
  else
  begin
    Add(name + NameValueSeparator + value);
  end;
end;

procedure TStringsHelper.ExecuteUpdate(proc: TProc);
begin
  UpdateStrings(Self, proc);
end;

procedure TStringsHelper.ExtractNames(strings: TStrings);
var
  i: Integer;
begin
  TArgument.CheckNotNull(strings, 'strings');
  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
    begin
      strings.Add(Self.Names[i]);
    end;
  finally
    strings.EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractValues(strings: TStrings);
var
  i: Integer;
begin
  TArgument.CheckNotNull(strings, 'strings');
  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
    begin
      strings.Add(Self.ValueFromIndex[i]);
    end;
  finally
    strings.EndUpdate;
  end;
end;

function TStringsHelper.ContainsName(const name: string): Boolean;
begin
  Result := IndexOfName(name) <> -1;
end;

function TStringsHelper.ContainsValue(const value: string): Boolean;
var
  v: string;
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    v := ValueFromIndex[i];
    if SameText(v, value) then
    begin
      Exit(True);
    end;
  end;
end;

function TStringsHelper.ContainsObject(obj: TObject): Boolean;
begin
  Result := IndexOfObject(obj) <> -1;
end;

function TStringsHelper.GetValueOrDefault<T>(const name: string;
  const default: T): T;
var
  index: Integer;
  value: string;
begin
  index := IndexOfName(name);
  if index <> -1 then
  begin
    value := ValueFromIndex[index];
  end;
  if value <> '' then
  begin
    Result := TValue.From<string>(value).AsType<T>;  // TODO: Fix this ASAP because TValue.AsType<T> sucks...
  end
  else
  begin
    Result := default;
  end;
end;

function TStringsHelper.GetNames: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := Names[i];
  end;
end;

function TStringsHelper.GetValues: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := ValueFromIndex[i];
  end;
end;

function TStringsHelper.ToArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := Strings[i];
  end;
end;

function TStringsHelper.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

{$ENDREGION}


{$REGION 'DB'}

{ TDataSetHelper }

procedure TDataSetHelper.CopyRecordFrom(source: TDataSet);
var
  field: TField;
  sourceField: TField;
begin
  TArgument.CheckNotNull(source, 'source');
  for field in Fields do
  begin
    if not field.ReadOnly and (field.FieldKind = fkData) then
    begin
      sourceField := source.FindField(field.FieldName);
      if sourceField <> nil then
      begin
        field.Value := sourceField.Value;
      end;
    end;
  end;
end;

function TDataSetHelper.GetValueOrDefault<T>(const fieldName: string;
  const default: T): T;
var
  field: TField;
begin
  field := FieldByName(fieldName);
  Result := field.GetValueOrDefault<T>(default);
end;

{ TFieldHelper }

function TFieldHelper.GetValueOrDefault<T>(const default: T): T;
var
  v: TValue;
begin
  if not IsNull then
  begin
    v := TValue.FromVariant(Value);
    Result := v.AsType<T>;
  end
  else
  begin
    Result := default;
  end;
end;

{$ENDREGION}


{$REGION 'Rtti Class Helpers'}

{ TRttiObjectHelper }

function TRttiObjectHelper.TryGetCustomAttribute<T>(out attribute: T): Boolean;
begin
  attribute := GetCustomAttribute<T>;
  Result := attribute <> nil;
end;

function TRttiObjectHelper.GetCustomAttribute<T>: T;
var
  attribute: TCustomAttribute;
begin
  Result := Default(T);
  for attribute in GetAttributes do
  begin
    if attribute.InheritsFrom(T) then
    begin
      Result := T(attribute);
      Break;
    end;
  end;
end;

function TRttiObjectHelper.GetCustomAttributes<T>: TArray<T>;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
  begin
    if attribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := T(attribute);
    end;
  end;
end;

function TRttiObjectHelper.HasCustomAttribute<T>: Boolean;
var
  attribute: T;
begin
  Result := TryGetCustomAttribute<T>(attribute);
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.InternalGetConstructors(
  enumerateBaseType: Boolean): IEnumerableEx<TRttiMethod>;
var
  func: TGetRttiMembersFunc<TRttiMethod>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end;
  Result := TRttiMemberEnumerable<TRttiMethod>.Create(
    Self,
    func,
    enumerateBaseType,
    TMethodFilters.IsConstructor()
  );
end;

function TRttiTypeHelper.InternalGetMethods(
  enumerateBaseType: Boolean): IEnumerableEx<TRttiMethod>;
var
  func: TGetRttiMembersFunc<TRttiMethod>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end;
  Result := TRttiMemberEnumerable<TRttiMethod>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.InternalGetProperties(
  enumerateBaseType: Boolean): IEnumerableEx<TRttiProperty>;
var
  func: TGetRttiMembersFunc<TRttiProperty>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiProperty>
    begin
      Result := targetType.GetDeclaredProperties;
    end;
  Result := TRttiMemberEnumerable<TRttiProperty>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.InternalGetFields(
  enumerateBaseType: Boolean): IEnumerableEx<TRttiField>;
var
  func: TGetRttiMembersFunc<TRttiField>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiField>
    begin
      Result := targetType.GetDeclaredFields;
    end;
  Result := TRttiMemberEnumerable<TRttiField>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.GetConstructors: IEnumerableEx<TRttiMethod>;
begin
  Result := InternalGetConstructors;
end;

function TRttiTypeHelper.GetMethods: IEnumerableEx<TRttiMethod>;
begin
  Result := InternalGetMethods;
end;

function TRttiTypeHelper.GetProperties: IEnumerableEx<TRttiProperty>;
begin
  Result := InternalGetProperties;
end;

function TRttiTypeHelper.GetFields: IEnumerableEx<TRttiField>;
begin
  Result := InternalGetFields;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  p1, p2: Integer;
  args: string;
  elements: TStringDynArray;
  i: Integer;
begin
  p1 := Pos('<', Name);
  p2 := Pos('>', Name);
  if (p1 = 0) or (p2 = 0) or (p1 > p2) then
  begin
    Exit(nil);
  end;
  args := MidStr(Name, p1+1, p2-p1-1);
  elements := SplitString(args, [','], True);
  SetLength(Result, Length(elements));
  for i := 0 to High(elements) do
  begin
    Result[i] := TType.FindType(elements[i]);
  end;
end;

function TRttiTypeHelper.GetAsClass: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetInterfaces: IEnumerableEx<TRttiInterfaceType>;
var
  list: IDictionary<TGUID, TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  entry: TInterfaceEntry;
  aType: TRttiInterfaceType;
  i: Integer;
begin
  if Self.IsClass then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    classType := Self.AsInstance.MetaclassType;
    while classType <> nil do
    begin
      table := classType.GetInterfaceTable;
      if table <> nil then
      begin
        for i := 0 to table.EntryCount - 1 do
        begin
          entry := table.Entries[i];
          if not list.ContainsKey(entry.IID) and
        {$WARNINGS OFF}
            not entry.IID.IsEmpty and
        {$WARNINGS ON}
            TType.TryGetInterfaceType(entry.IID, aType) then
          begin
            list[entry.IID] := aType;
          end;
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.Values;
  end;
end;

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := Self.IsClass or Self.IsInterface;
end;

function TRttiTypeHelper.GetIsGenericType: Boolean;
begin
  Result := (Pos('<', Name) > 0) and (Pos('>', Name) > 0);
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

{ TRttiInterfaceTypeHelper }

function TRttiInterfaceTypeHelper.GetHasGuid: Boolean;
begin
  Result := ifHasGuid in Self.IntfFlags;
end;

{ TRttiMemberHelper }

function TRttiMemberHelper.GetValue(const instance: TValue): TValue;
begin
  if IsProperty then
  begin
    Result := AsProperty.GetValue(instance);
  end
  else if IsField then
  begin
    Result := AsField.GetValue(instance);
  end
  else
  begin
    raise EInvalidOperation.CreateRes(@SInvalidOperation_GetValue);
  end;
end;

procedure TRttiMemberHelper.SetValue(const instance, value: TValue);
begin
  if IsProperty then
  begin
    AsProperty.SetValue(instance, value);
  end
  else if IsField then
  begin
    AsField.SetValue(instance, value);
  end
  else
  begin
    raise EInvalidOperation.CreateRes(@SInvalidOperation_SetValue);
  end;
end;

function TRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function TRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function TRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function TRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;

function TRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function TRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function TRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function TRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function TRttiMemberHelper.GetAsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.GetAsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.GetAsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := GetValue(instance.AsObject)
  else
    Result := GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiPropertyHelper.SetValue(const instance, value: TValue);
begin
  if instance.IsObject then
    SetValue(instance.AsObject, value)
  else
    SetValue(instance.GetReferenceToRawData, value);
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := AsField.GetValue(instance.AsObject)
  else
    Result := AsField.GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiFieldHelper.SetValue(const instance, value: TValue);
begin
  if instance.IsObject then
    SetValue(instance.AsObject, value)
  else
    SetValue(instance.GetReferenceToRawData, value);
end;

{$ENDREGION}

end.
