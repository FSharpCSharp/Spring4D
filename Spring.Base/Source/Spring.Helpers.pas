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

unit Spring.Helpers;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  TypInfo,
  Rtti,
  ComObj,
  Spring.DesignPatterns;

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
  end experimental;

  TMethodHelper = record helper for TMethod
  public
    class function Create(const objectAddress, methodAddress: Pointer): TMethod; static;
  end;

  {$REGION 'Class helpers for Enhanced Rtti (Reflection)'}

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetCustomAttributes<T: TCustomAttribute>: TArray<T>;
    function GetCustomAttribute<T: TCustomAttribute>: T;
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T): Boolean;
    function HasCustomAttribute<T: TCustomAttribute>: Boolean;
  end;

  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
  public
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
  end;

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
  public
//    procedure InvokeMember(instance: TValue; const args: TArray<TValue>);
//    procedure InvokeMember(instance: TObject; const args: TArray<TValue>);
    function AsProperty: TRttiProperty;
    function AsMethod: TRttiMethod;
    function AsField: TRttiField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    property HasGuid: Boolean read GetHasGuid;
  end;

  {$ENDREGION}

implementation


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
//  attributes: TArray<TCustomAttribute>;
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

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := Self.IsClass or Self.IsInterface;
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

function TRttiMemberHelper.AsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.AsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.AsField: TRttiField;
begin
  Result := Self as TRttiField;
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

{$ENDREGION}


end.
