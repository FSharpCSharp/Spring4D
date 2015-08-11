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

unit Spring.Persistence.Mapping.RttiExplorer;

interface

uses
  Rtti,
  SyncObjs,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;

type
  /// <summary>
  ///   Provides methods to access certain RTTI related information from entity
  ///   classes - internal use only.
  /// </summary>
  TRttiExplorer = record
  private
    class var fLock: TCriticalSection;
    class constructor Create;
    class destructor Destroy;

    class function GetEntityRttiType(typeInfo: PTypeInfo): TRttiType; static;
    class function GetSubEntityFromMemberDeep(const entity: TObject;
      const rttiMember: TRttiMember): IList<TObject>; static;
  public
    class function GetClassMembers<T: TORMAttribute>(classType: TClass): IList<T>; static;
    class function GetColumns(classType: TClass): IList<ColumnAttribute>; static;
    class function GetEntityClass(classInfo: PTypeInfo): TClass; static;

    class function GetMemberValueDeep(const entity: TObject;
      const member: TRttiMember): TValue; overload; static;
    class function GetMemberValueDeep(const initialValue: TValue): TValue; overload; static;
    class function GetRelationsOf(const entity: TObject;
      const relationAttributeClass: TAttributeClass): IList<TObject>; static;
    class function GetTable(classType: TClass): TableAttribute; overload; static;
    class function GetPrimaryKeyColumn(classType: TClass): ColumnAttribute; static;
    class function GetSequence(classType: TClass): SequenceAttribute; static;
    class function GetUniqueConstraints(classType: TClass): IList<UniqueConstraintAttribute>; static;
  end;

implementation

uses
  Classes,
  TypInfo,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TRttiExplorer'}

class constructor TRttiExplorer.Create;
begin
  fLock := TCriticalSection.Create;
end;

class destructor TRttiExplorer.Destroy;
begin
  fLock.Free;
end;

class function TRttiExplorer.GetClassMembers<T>(classType: TClass): IList<T>;
var
  rttiType: TRttiType;
  rttiField: TRttiField;
  rttiProperty: TRttiProperty;
  attribute: TORMAttribute;
  attribute2: T;
begin
  Result := TCollections.CreateList<T>;
  rttiType := TType.GetType(classType);

  fLock.Enter;
  try
    for attribute in rttiType.GetCustomAttributes<TORMAttribute>(True) do
    begin
      attribute.EntityClass := classType;
      attribute.MemberKind := mkClass;
    end;

    for rttiField in rttiType.GetFields do
    begin
      for attribute2 in rttiField.GetCustomAttributes<T>(True) do
      begin
        attribute2.EntityClass := classType;
        attribute2.Member := rttiField;
        attribute2.MemberKind := mkField;
        Result.Add(attribute2);
      end;
    end;

    for rttiProperty in rttiType.GetProperties do
    begin
      for attribute2 in rttiProperty.GetCustomAttributes<T>(True) do
      begin
        attribute2.EntityClass := classType;
        attribute2.Member := rttiProperty;
        attribute2.MemberKind := mkProperty;
        Result.Add(attribute2);
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

class function TRttiExplorer.GetColumns(classType: TClass): IList<ColumnAttribute>;
begin
  Result := GetClassMembers<ColumnAttribute>(classType);
end;

class function TRttiExplorer.GetEntityClass(classInfo: PTypeInfo): TClass;
var
  rttiType: TRttiType;
begin
  rttiType := GetEntityRttiType(classInfo);
  if not Assigned(rttiType) then
    raise EORMUnsupportedType.CreateFmt('Unsupported type %s', [classInfo.TypeName]);

  Result := rttiType.AsInstance.MetaclassType;
end;

class function TRttiExplorer.GetEntityRttiType(typeInfo: PTypeInfo): TRttiType;
var
  rttiType: TRttiType;
begin
  rttiType := TType.GetType(typeInfo);
  if rttiType = nil then
    raise EORMUnsupportedType.CreateFmt('Type %s must contain RTTI', [typeInfo.TypeName]);

  if not rttiType.IsInstance then
    raise EORMUnsupportedType.CreateFmt('Type %s must be a class type', [typeInfo.TypeName]);

  if not rttiType.HasCustomAttribute<TableAttribute>(True) then
    raise EORMUnsupportedType.CreateFmt('Type %s requires [Table] attribute', [typeInfo.TypeName]);

  if GetPrimaryKeyColumn(rttiType.AsInstance.MetaclassType) = nil then
    raise EORMUnsupportedType.CreateFmt('Type %s requires primary key [Column]', [typeInfo.TypeName]);

  Result := rttiType;
end;

class function TRttiExplorer.GetPrimaryKeyColumn(classType: TClass): ColumnAttribute;
var
  column: ColumnAttribute;
begin
  for column in GetColumns(classType) do
    if cpPrimaryKey in column.Properties then
      Exit(column);
  Result := nil;
end;

class function TRttiExplorer.GetRelationsOf(const entity: TObject;
  const relationAttributeClass: TAttributeClass): IList<TObject>;
var
  rttiType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
begin
  Result := TCollections.CreateList<TObject>;

  rttiType := TType.GetType(entity.ClassType);
  for field in rttiType.GetFields do
    if field.HasCustomAttribute(relationAttributeClass)
      and not field.HasCustomAttribute(TransientAttribute) then
      Result.AddRange(GetSubEntityFromMemberDeep(entity, field));

  for prop in rttiType.GetProperties do
    if prop.HasCustomAttribute(relationAttributeClass)
      and not prop.HasCustomAttribute(TransientAttribute) then
      Result.AddRange(GetSubEntityFromMemberDeep(entity, prop));
end;

class function TRttiExplorer.GetMemberValueDeep(
  const initialValue: TValue): TValue;
begin
  Result := initialValue;
  if IsNullable(Result.TypeInfo) then
  begin
    if not initialValue.TryGetNullableValue(Result) then
      Result := TValue.Empty;
  end
  else if IsLazyType(Result.TypeInfo) then
    if not initialValue.TryGetLazyValue(Result) then
      Result := TValue.Empty;
end;

class function TRttiExplorer.GetMemberValueDeep(const entity: TObject;
  const member: TRttiMember): TValue;
begin
  Result := member.GetValue(entity);
  if not Result.IsEmpty then
    Result := GetMemberValueDeep(Result);
end;

class function TRttiExplorer.GetSequence(classType: TClass): SequenceAttribute;
begin
  Result := TType.GetType(classType).GetCustomAttribute<SequenceAttribute>(True);
end;

class function TRttiExplorer.GetSubEntityFromMemberDeep(const entity: TObject;
  const rttiMember: TRttiMember): IList<TObject>;
var
  memberValue: TValue;
  value: TValue;
  objects: IObjectList;
  current: TObject;
begin
  Result := TCollections.CreateList<TObject>;

  memberValue := rttiMember.GetValue(entity);
  if memberValue.IsEmpty then
    Exit;
    
  value := GetMemberValueDeep(memberValue);
  if value.IsEmpty then
    Exit;
  
  if value.IsInterface and Supports(value.AsInterface, IObjectList, objects) then
  begin
    for current in objects do
      Result.Add(current);
    value := TValue.Empty;
  end;

  if value.IsObject and (value.AsObject <> nil) then
    Result.Add(value.AsObject);
end;

class function TRttiExplorer.GetTable(classType: TClass): TableAttribute;
begin
  Result := TType.GetType(classType).GetCustomAttribute<TableAttribute>(True);
end;

class function TRttiExplorer.GetUniqueConstraints(classType: TClass): IList<UniqueConstraintAttribute>;
begin
  Result := GetClassMembers<UniqueConstraintAttribute>(classType);
end;

{$ENDREGION}


end.
