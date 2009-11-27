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
  Generics.Defaults,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Collections.Extensions,
  Spring.Reflection.Filters,
  Spring.DesignPatterns;

type
  {$REGION 'TRtti'}

  /// <summary>
  /// Provides static methods to get RTTI information of parameterized type.
  /// </summary>
  TRtti = record
  private
    class var
      fContext: TRttiContext;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    class procedure CheckTypeKind<T>(const typeKind: TypInfo.TTypeKind); overload; static;
    class procedure CheckTypeKind<T>(const typeKinds: TypInfo.TTypeKinds); overload; static;
    class function IsManagedType<T>: Boolean; static;
    class function IsNullReference<T>(const value: T): Boolean; overload; static;
    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; overload; static;
    class function IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean; overload; static;
    class function GetTypeInfo<T>: PTypeInfo; static;
    class function GetTypeData<T>: PTypeData; static;
    class function GetTypeKind<T>: TypInfo.TTypeKind; static;
    class function GetTypeName<T>: string; static;
    class function GetFullName(typeInfo: PTypeInfo): string; overload; static;
    class function GetFullName<T>: string; overload; static;
//    class function GetType<T>(const obj: T): IType<T>;
  end;

  {$ENDREGION}

  TMemberFilters = Spring.Reflection.Filters.TMemberFilters;
  TMethodFilters = Spring.Reflection.Filters.TMethodFilters;
  TPropertyFilters = Spring.Reflection.Filters.TPropertyFilters;
  TFieldFilters = Spring.Reflection.Filters.TFieldFilters;

  TGetRttiMembersFunc<T> = reference to function(targetType: TRttiType): TArray<T>;

  /// <summary>
  /// TRttiMemberEnumerable<T>
  /// </summary>
  TRttiMemberEnumerableEx<T: TRttiMember> = class(TEnumerableBase<T>,
    IEnumerableEx<T>, IEnumerable_<T>, IInterface)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fCollection: TRttiMemberEnumerableEx<T>;
        fTargetType: TRttiType;
        fMembers: TArray<T>;
        fIndex: Integer;
      protected
        procedure Initialize(targetType: TRttiType);
        function GetCurrent: T; override;
      public
        constructor Create(collection: TRttiMemberEnumerableEx<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fParentType: TRttiType;
    fGetMembersFunc: TGetRttiMembersFunc<T>;
    fEnumerateBaseType: Boolean;
    fPredicate: TPredicate<T>;
  public
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean); overload;
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean; const predicate: TPredicate<T>); overload;
    function GetEnumerator: IEnumeratorEx<T>; override;
    function Where(const predicate: TPredicate<T>): IEnumerableEx<T>; override;
  end;

implementation

uses
  Spring.ResourceStrings;


{$REGION 'TRtti'}

class constructor TRtti.Create;
begin
  fContext := TRttiContext.Create;
end;

class destructor TRtti.Destroy;
begin
  fContext.Free;
end;

class procedure TRtti.CheckTypeKind<T>(const typeKind: TypInfo.TTypeKind);
begin
  TRtti.CheckTypeKind<T>([typeKind]);
end;

class procedure TRtti.CheckTypeKind<T>(const typeKinds: TypInfo.TTypeKinds);
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TRtti.GetTypeInfo<T>;
  if not (typeInfo.Kind in typeKinds) then
    raise ERttiException.CreateResFmt(@SUnexpectedTypeKind, [TRtti.GetTypeName<T>]);
end;

class function TRtti.IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean;
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

class function TRtti.IsManagedType<T>: Boolean;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TRtti.GetTypeInfo<T>;
  Result := Rtti.IsManaged(typeInfo);
end;

class function TRtti.IsNullReference<T>(const value: T): Boolean;
var
  localTypeInfo: PTypeInfo;
begin
  localTypeInfo := TypeInfo(T);
  Result := TRtti.IsNullReference(value, localTypeInfo);
end;

//  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
//    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
//    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
//    tkClassRef, tkPointer, tkProcedure);
class function TRtti.IsNullReference(const value; typeInfo: PTypeInfo): Boolean;
begin
  Result := (typeInfo <> nil) and
    (typeInfo.Kind in [tkPointer, tkClass, tkClassRef, tkInterface, tkProcedure, tkMethod]);
  Result := Result and not Assigned(@value);
end;

class function TRtti.GetTypeName<T>: string;
begin
  Result := TypInfo.GetTypeName(TRtti.GetTypeInfo<T>);
end;

class function TRtti.GetTypeKind<T>: TypInfo.TTypeKind;
begin
  Result := TRtti.GetTypeInfo<T>.Kind;
end;

class function TRtti.GetTypeInfo<T>: PTypeInfo;
begin
  Result := System.TypeInfo(T);
  if Result = nil then
    raise ERttiException.CreateRes(@SNoTypeInfo);
end;

class function TRtti.GetFullName(typeInfo: PTypeInfo): string;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  Result := fContext.GetType(typeInfo).QualifiedName;
end;

class function TRtti.GetFullName<T>: string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TRtti.GetTypeInfo<T>;
  Result := TRtti.GetFullName(typeInfo);
end;

class function TRtti.GetTypeData<T>: PTypeData;
var
  info: PTypeInfo;
begin
  info := TRtti.GetTypeInfo<T>;
  Result := TypInfo.GetTypeData(info);
end;

{$ENDREGION}


{$REGION 'TRttiMemberEnumerable<T>'}

constructor TRttiMemberEnumerableEx<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean);
begin
  Create(parentType, func, enumerateBaseType, nil);
end;

constructor TRttiMemberEnumerableEx<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean;
  const predicate: TPredicate<T>);
begin
  inherited Create;
  fParentType := parentType;
  fGetMembersFunc := func;
  fEnumerateBaseType := enumerateBaseType;
  fPredicate := predicate;
end;

function TRttiMemberEnumerableEx<T>.GetEnumerator: IEnumeratorEx<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TRttiMemberEnumerableEx<T>.Where(
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
      function(value: T): Boolean
      begin
        Result := fPredicate(value) and predicate(value);
      end;
  end;
  Result := TRttiMemberEnumerableEx<T>.Create(fParentType, fGetMembersFunc,
    fEnumerateBaseType, finalPredicate);
end;

{ TRttiMemberEnumerableEx<T>.TEnumerator }

constructor TRttiMemberEnumerableEx<T>.TEnumerator.Create(
  collection: TRttiMemberEnumerableEx<T>);
begin
  inherited Create;
  fCollection := collection;
  Initialize(fCollection.fParentType);
end;

procedure TRttiMemberEnumerableEx<T>.TEnumerator.Initialize(
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

function TRttiMemberEnumerableEx<T>.TEnumerator.MoveNext: Boolean;
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

function TRttiMemberEnumerableEx<T>.TEnumerator.GetCurrent: T;
begin
  Result := fMembers[fIndex];
end;

{$ENDREGION}

end.
