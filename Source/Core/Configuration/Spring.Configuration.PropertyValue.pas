{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.spring4d.org                                           }
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

/// NOT READY
unit Spring.Configuration.PropertyValue experimental;
{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Spring,
  Spring.Collections,
  TypInfo,
  Generics.Collections;

type
  TPropertyValue = record
  strict private
    fValue: TValue;
    function GetDataSize: Integer;
    function GetIsEmpty: Boolean;
    function GetTypeDataProp: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;
  public
    // Easy in
    class operator Implicit(const Value: string): TPropertyValue;
    class operator Implicit(Value: Integer): TPropertyValue;
    class operator Implicit(Value: Extended): TPropertyValue;
    class operator Implicit(Value: Int64): TPropertyValue;
    class operator Implicit(Value: TObject): TPropertyValue;
    class operator Implicit(Value: TClass): TPropertyValue;
    class operator Implicit(Value: Boolean): TPropertyValue;
    class function FromVariant(const Value: Variant): TPropertyValue; static;
    class function From<T>(const Value: T): TPropertyValue; static;
    class function FromOrdinal(ATypeInfo: PTypeInfo; AValue: Int64): TPropertyValue; static;
    class function FromArray(ArrayTypeInfo: PTypeInfo; const Values: array of TValue): TPropertyValue; static;
    // Easy out
    property Kind: TTypeKind read GetTypeKind;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeData: PTypeData read GetTypeDataProp;
    // Empty converts to all-zero for the corresponding type, but IsEmpty
    // will only return True for reference types (where 'nil' is logical)
    // and method pointers (where TMethod.Code equivalent is nil).
    // A typeless empty (like 'nil' literal) will also have 0 for DataSize.
    property IsEmpty: Boolean read GetIsEmpty;
    function IsObject: Boolean;
    function AsObject: TObject;
    function IsInstanceOf(AClass: TClass): Boolean;
    function IsClass: Boolean;
    function AsClass: TClass;
    function IsOrdinal: Boolean;
    function AsOrdinal: Int64;
    function TryAsOrdinal(out AResult: Int64): Boolean;
    // TPropertyValue -> concrete type
    // IsType returns true if AsType or Cast would succeed
    // AsType / Cast are only for what would normally be implicit conversions in Delphi.
    function IsType<T>: Boolean; overload;
    function IsType(ATypeInfo: PTypeInfo): Boolean; overload;
    function AsType<T>: T;
    function TryAsType<T>(out AResult: T): Boolean;
    // TPropertyValue -> TPropertyValue conversions
    function Cast<T>: TPropertyValue; overload;
    function Cast(ATypeInfo: PTypeInfo): TPropertyValue; overload;
    function TryCast(ATypeInfo: PTypeInfo; out AResult: TPropertyValue): Boolean;
    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsExtended: Extended;
    function AsInt64: Int64;
    function AsInterface: IInterface;
    function AsString: string;
    function AsVariant: Variant;
    function AsCurrency: Currency;
    function IsArray: Boolean;
    function GetArrayLength: Integer;
    function GetArrayElement(Index: Integer): TPropertyValue;
    procedure SetArrayElement(Index: Integer; const AValue: TPropertyValue);
    // Low-level in
    class procedure Make(ABuffer: Pointer; ATypeInfo: PTypeInfo; out Result: TPropertyValue); overload; static;
    class procedure Make(AValue: NativeInt; ATypeInfo: PTypeInfo; out Result: TPropertyValue); overload; static;
    class procedure MakeWithoutCopy(ABuffer: Pointer; ATypeInfo: PTypeInfo; out Result: TPropertyValue); overload; static;
    // Low-level out
    property DataSize: Integer read GetDataSize;
    procedure ExtractRawData(ABuffer: Pointer);
    // If internal data is something with lifetime management, this copies a
    // reference out *without* updating the reference count.
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    function GetReferenceToRawData: Pointer;
    function GetReferenceToRawArrayElement(Index: Integer): Pointer;
    function ToString: string;


    procedure Clear;
  end;

implementation

{ TPropertyValue }

function TPropertyValue.AsBoolean: Boolean;
begin
  Result := fValue.AsBoolean;
end;

function TPropertyValue.AsClass: TClass;
begin
  Result := fValue.AsClass;
end;

function TPropertyValue.AsCurrency: Currency;
begin
  Result := fValue.AsCurrency;
end;

function TPropertyValue.AsExtended: Extended;
begin
  Result := fValue.AsExtended;
end;

function TPropertyValue.AsInt64: Int64;
begin
  Result := fValue.AsInt64;
end;

function TPropertyValue.AsInteger: Integer;
begin
  Result := fValue.AsInteger;
end;

function TPropertyValue.AsInterface: IInterface;
begin
  Result := fValue.AsInterface;
end;

function TPropertyValue.AsObject: TObject;
begin
  Result := fValue.AsObject;
end;

function TPropertyValue.AsOrdinal: Int64;
begin
  Result := fValue.AsOrdinal;
end;

function TPropertyValue.AsString: string;
begin
  Result := fValue.AsString;
end;

function TPropertyValue.AsType<T>: T;
begin
  Result := fValue.AsType<T>;
end;

function TPropertyValue.AsVariant: Variant;
begin
  Result := fValue.AsVariant;
end;

function TPropertyValue.Cast(ATypeInfo: PTypeInfo): TPropertyValue;
begin
  Result.fValue := fValue.Cast(ATypeInfo);
end;

function TPropertyValue.Cast<T>: TPropertyValue;
begin
  Result.fValue := fValue.Cast<T>;
end;

procedure TPropertyValue.Clear;
begin
  fValue := TValue.Empty;
end;

procedure TPropertyValue.ExtractRawData(ABuffer: Pointer);
begin
  fValue.ExtractRawData(ABuffer);
end;

procedure TPropertyValue.ExtractRawDataNoCopy(ABuffer: Pointer);
begin
  fValue.ExtractRawDataNoCopy(ABuffer);
end;

class function TPropertyValue.From<T>(const Value: T): TPropertyValue;
begin
  Result.fValue := TValue.From<T>(Value);
end;

class function TPropertyValue.FromArray(ArrayTypeInfo: PTypeInfo;
  const Values: array of TValue): TPropertyValue;
begin
  Result.fValue := TValue.FromArray(ArrayTypeInfo, Values);
end;

class function TPropertyValue.FromOrdinal(ATypeInfo: PTypeInfo;
  AValue: Int64): TPropertyValue;
begin
  Result.fValue := TValue.FromOrdinal(ATypeInfo, AValue);
end;

class function TPropertyValue.FromVariant(const Value: Variant): TPropertyValue;
begin
  Result.fValue := TValue.FromVariant(Value);
end;

function TPropertyValue.GetArrayElement(Index: Integer): TPropertyValue;
begin
  Result.fValue := fValue.GetArrayElement(Index);
end;

function TPropertyValue.GetArrayLength: Integer;
begin
  Result := fValue.GetArrayLength;
end;

function TPropertyValue.GetDataSize: Integer;
begin
  Result := fValue.DataSize;
end;

function TPropertyValue.GetIsEmpty: Boolean;
begin
  Result := fValue.IsEmpty;
end;

function TPropertyValue.GetReferenceToRawArrayElement(Index: Integer): Pointer;
begin
  Result := fValue.GetReferenceToRawArrayElement(Index);
end;

function TPropertyValue.GetReferenceToRawData: Pointer;
begin
  Result := fValue.GetReferenceToRawData;
end;

function TPropertyValue.GetTypeDataProp: PTypeData;
begin
  Result := fValue.TypeData;
end;

function TPropertyValue.GetTypeInfo: PTypeInfo;
begin
  Result := fValue.TypeInfo;
end;

function TPropertyValue.GetTypeKind: TTypeKind;
begin
  Result := fValue.Kind;
end;

class operator TPropertyValue.Implicit(const Value: string): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: TObject): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: Int64): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: Boolean): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: TClass): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: Integer): TPropertyValue;
begin
  Result.fValue := Value;
end;

class operator TPropertyValue.Implicit(Value: Extended): TPropertyValue;
begin
  Result.fValue := Value;
end;

function TPropertyValue.IsArray: Boolean;
begin
  Result := fValue.IsArray;
end;

function TPropertyValue.IsClass: Boolean;
begin
  Result := fValue.IsClass;
end;

function TPropertyValue.IsInstanceOf(AClass: TClass): Boolean;
begin
  Result := fValue.IsInstanceOf(AClass);
end;

function TPropertyValue.IsObject: Boolean;
begin
  Result := fValue.IsObject;
end;

function TPropertyValue.IsOrdinal: Boolean;
begin
  Result := fValue.IsOrdinal;
end;

function TPropertyValue.IsType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := fValue.IsType(ATypeInfo);
end;

function TPropertyValue.IsType<T>: Boolean;
begin
  Result := fValue.IsType<T>;
end;

class procedure TPropertyValue.Make(AValue: NativeInt; ATypeInfo: PTypeInfo;
  out Result: TPropertyValue);
begin
  TValue.Make(AValue, ATypeInfo, Result.fValue);
end;

class procedure TPropertyValue.Make(ABuffer: Pointer; ATypeInfo: PTypeInfo;
  out Result: TPropertyValue);
begin
  TValue.Make(ABuffer, ATypeInfo, Result.fValue);
end;

class procedure TPropertyValue.MakeWithoutCopy(ABuffer: Pointer;
  ATypeInfo: PTypeInfo; out Result: TPropertyValue);
begin
  TValue.MakeWithoutCopy(ABuffer, ATypeInfo, Result.fValue);
end;

procedure TPropertyValue.SetArrayElement(Index: Integer; const AValue: TPropertyValue);
begin
  fValue.SetArrayElement(Index, AValue.fValue);
end;

function TPropertyValue.ToString: string;
begin
  Result := fValue.ToString;
end;

function TPropertyValue.TryAsOrdinal(out AResult: Int64): Boolean;
begin
  Result := fValue.TryAsOrdinal(AResult);
end;

function TPropertyValue.TryAsType<T>(out AResult: T): Boolean;
begin
  Result := fValue.TryAsType<T>(AResult);
end;

function TPropertyValue.TryCast(ATypeInfo: PTypeInfo; out AResult: TPropertyValue): Boolean;
begin
  Result := fValue.TryCast(ATypeInfo, AResult.fValue);
end;

end.
