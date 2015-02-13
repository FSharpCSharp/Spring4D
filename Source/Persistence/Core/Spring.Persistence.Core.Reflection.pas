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

unit Spring.Persistence.Core.Reflection;

interface

uses
  Rtti,
  Types,
  TypInfo;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiType">TRttiType</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiTypeHelper = class helper for TRttiType
  private
    function ExtractGenericArguments: string;
    function GetAsInterface: TRttiInterfaceType;
    function GetIsInterface: Boolean;
    function GetMethodCount: Integer;
    function InheritsFrom(OtherType: PTypeInfo): Boolean;
  public
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;
    function GetGenericArguments: TArray<TRttiType>;
    function GetGenericTypeDefinition(const AIncludeUnitName: Boolean = True): string;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method at the given code address; <b>nil</b> if nothing
    ///	  is found.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    {$ENDREGION}
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;
    function GetStandardConstructor: TRttiMethod;

    function IsCovariantTo(OtherClass: TClass): Boolean; overload;
    function IsCovariantTo(OtherType: PTypeInfo): Boolean; overload;
    function IsGenericTypeDefinition: Boolean;
    function IsGenericTypeOf(const BaseTypeName: string): Boolean;
    function IsInheritedFrom(OtherType: TRttiType): Boolean; overload;
    function IsInheritedFrom(const OtherTypeName: string): Boolean; overload;
    function MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    ///	<param name="AField">
    ///	  Field that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetField(const AName: string; out AField: TRttiField): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the property with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    ///	<param name="AProperty">
    ///	  Property that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    function TryGetStandardConstructor(out AMethod: TRttiMethod): Boolean;

    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property IsInterface: Boolean read GetIsInterface;
    property MethodCount: Integer read GetMethodCount;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TValue">TValue</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TValueHelper = record helper for TValue
  private
    function GetRttiType: TRttiType;
    class function FromFloat(ATypeInfo: PTypeInfo; AValue: Extended): TValue; static;
  public
    function GetType: TRttiType;

    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsPointer: Boolean;
    function IsString: Boolean;

    function IsInstance: Boolean;
    function IsInterface: Boolean;

    // conversion for almost all standard types
    function TryConvert(ATypeInfo: PTypeInfo; out AResult: TValue; out AFreeAfter: Boolean): Boolean;

    function AsByte: Byte;
    function AsCardinal: Cardinal;
    function AsCurrency: Currency;
    function AsDate: TDate;
    function AsDateTime: TDateTime;
    function AsDouble: Double;
    function AsFloat: Extended;
    function AsPointer: Pointer;
    function AsShortInt: ShortInt;
    function AsSingle: Single;
    function AsSmallInt: SmallInt;
    function AsTime: TTime;
    function AsUInt64: UInt64;
    function AsWord: Word;

    function ToObject: TObject;

    class function ToString(const Value: TValue): string; overload; static;
    class function ToString(const Values: TArray<TValue>): string; overload; static;

    class function From(ABuffer: Pointer; ATypeInfo: PTypeInfo): TValue; overload; static;
    class function From(AObject: TObject; AClass: TClass): TValue; overload; static;
    class function FromBoolean(const Value: Boolean): TValue; static;
    class function FromString(const Value: string): TValue; static;

    function IsBoolean: Boolean;
    function IsByte: Boolean;
    function IsCardinal: Boolean;
    function IsCurrency: Boolean;
    function IsDate: Boolean;
    function IsDateTime: Boolean;
    function IsDouble: Boolean;
    function IsInteger: Boolean;
    function IsInt64: Boolean;
    function IsShortInt: Boolean;
    function IsSingle: Boolean;
    function IsSmallInt: Boolean;
    function IsTime: Boolean;
    function IsUInt64: Boolean;
    function IsVariant: Boolean;
    function IsRecord: Boolean;
    function IsWord: Boolean;

    property RttiType: TRttiType read GetRttiType;
  end;

procedure FreeValueObject(const AValue: TValue);

implementation

uses
  Classes,
  Spring.Collections,
  Math,
  StrUtils,
  SysUtils
  ,Graphics
  ,Spring.Reflection
  ,Spring.Persistence.Core.Utils
  ,Variants
  ;

var
  Context: TRttiContext;
  Enumerations: IDictionary<PTypeInfo, TStrings>;

function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisType);
  Result := Assigned(LType) and LType.IsCovariantTo(OtherType);
end;

function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(AClass);
  Result := Assigned(AType);
end;

function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(ATypeInfo);
  Result := Assigned(AType);
end;

{$REGION 'Conversion functions'}
type
  TConvertFunc = function(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;

function ConvFail(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := False;
end;

function ConvAny2Nullable(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LValue: TValue;
  LBuffer: array of Byte;
  LFree: Boolean;
begin
  Result := TryGetRttiType(ATarget, LType) and LType.IsGenericTypeOf('Nullable')
    and ASource.TryConvert(LType.GetGenericArguments[0].Handle, LValue, LFree);
  if Result then
  begin
    SetLength(LBuffer, LType.TypeSize);
    Move(LValue.GetReferenceToRawData^, LBuffer[0], LType.TypeSize - SizeOf(string));
    PString(@LBuffer[LType.TypeSize - SizeOf(string)])^ := DefaultTrueBoolStr;
    TValue.Make(LBuffer, LType.Handle, AResult);
    PString(@LBuffer[LType.TypeSize - SizeOf(string)])^ := '';
  end
end;

function ConvClass2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ASource.TryCast(ATarget, AResult);
  if not Result and IsTypeCovariantTo(ASource.TypeInfo, ATarget) then
  begin
    AResult := TValue.From(ASource.AsObject, GetTypeData(ATarget).ClassType);
    Result := True;
  end;
end;

function ConvClass2Enum(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ATarget = TypeInfo(Boolean);
  if Result then
    AResult := ASource.AsObject <> nil;
end;

function ConvEnum2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LStrings: TStrings;
  i: Integer;
begin
  Result := TryGetRttiType(ATarget, LType)
    and LType.AsInstance.MetaclassType.InheritsFrom(TStrings);
  if Result then
  begin
    if not Enumerations.TryGetValue(ASource.TypeInfo, LStrings) then
    begin
      LStrings := TStringList.Create;
      with TRttiEnumerationType(ASource.RttiType) do
      begin
        for i := MinValue to MaxValue do
        begin
          LStrings.Add(GetEnumName(Handle, i));
        end;
      end;
      Enumerations.Add(ASource.TypeInfo, LStrings);
    end;
    AResult := TValue.From(LStrings, TStrings);
    Result := True;
  end;
end;

function ConvFloat2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := Frac(ASource.AsExtended) = 0;
  if Result then
    AResult := TValue.FromOrdinal(ATarget, Trunc(ASource.AsExtended));
end;

function ConvFloat2Str(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LValue: TValue;
begin
  if ASource.TypeInfo = TypeInfo(TDate) then
    LValue := DateToStr(ASource.AsExtended)
  else if ASource.TypeInfo = TypeInfo(TDateTime) then
    LValue := DateTimeToStr(ASource.AsExtended)
  else if ASource.TypeInfo = TypeInfo(TTime) then
    LValue := TimeToStr(ASource.AsExtended)
  else
    LValue := FloatToStr(ASource.AsExtended);
  Result := LValue.TryCast(ATarget, AResult);
end;

function ConvIntf2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ConvClass2Class(ASource.AsInterface as TObject, ATarget, AResult);
end;

function ConvIntf2Intf(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LSourceType, LTargetType: TRttiType;
  LMethod: TRttiMethod;
  LInterface: IInterface;
begin
  Result := ASource.TryCast(ATarget, AResult);
  if not Result then
  begin
    if IsTypeCovariantTo(ASource.TypeInfo, ATarget) then
    begin
      AResult := TValue.From(ASource.GetReferenceToRawData, ATarget);
      Result := True;
    end else
    if TryGetRttiType(ASource.TypeInfo, LSourceType) and LSourceType.IsGenericTypeOf('IList') then
    begin
      if (ATarget.Name = 'IList') and LSourceType.TryGetMethod('AsList', LMethod) then
      begin
        LInterface := LMethod.Invoke(ASource, []).AsInterface;
        AResult := TValue.From(@LInterface, ATarget);
        Result := True;
      end else
      // assume that the two lists are contravariant
      // TODO: check type parameters for compatibility
      if TryGetRttiType(ATarget, LTargetType) and LTargetType.IsGenericTypeOf('IList') then
      begin
        LInterface := ASource.AsInterface;
        AResult := TValue.From(@LInterface, ATarget);
        Result := True;
      end;
    end;
  end;
end;

function ConvNullable2Any(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LValue: TValue;
  LFree: Boolean;
begin
  Result := TryGetRttiType(ASource.TypeInfo, LType)
    and LType.IsGenericTypeOf('Nullable');
  if Result then
  begin
    LValue := TValue.From(ASource.GetReferenceToRawData, LType.GetGenericArguments[0].Handle);
    Result := LValue.TryConvert(ATarget, AResult, LFree);
  end
end;

function ConvOrd2Float(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromFloat(ATarget, ASource.AsOrdinal);
  Result := True;
end;

function ConvOrd2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, ASource.AsOrdinal);
  Result := True;
end;

function ConvOrd2Str(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LValue: TValue;
begin
  LValue := ASource.ToString;
  Result := LValue.TryCast(ATarget, AResult);
end;

function ConvRec2Meth(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  if ASource.TypeInfo = TypeInfo(TMethod) then
  begin
    AResult := TValue.From(ASource.GetReferenceToRawData, ATarget);
    Result := True;
  end
  else
  begin
    Result := ConvNullable2Any(ASource, ATarget, AResult);
  end;
end;

function ConvSet2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LTypeData: PTypeData;
  LStrings: TStrings;
  i: Integer;
begin
  Result := TryGetRttiType(ATarget, LType)
    and LType.AsInstance.MetaclassType.InheritsFrom(TStrings);
  if Result then
  begin
    LTypeData := GetTypeData(ASource.TypeInfo);
    if not Enumerations.TryGetValue(LTypeData.CompType^, LStrings) then
    begin
      LStrings := TStringList.Create;
      with TRttiEnumerationType(TRttiSetType(ASource.RttiType).ElementType) do
      begin
        for i := MinValue to MaxValue do
        begin
          LStrings.Add(GetEnumName(Handle, i));
        end;
      end;
      Enumerations.Add(LTypeData.CompType^, LStrings);
    end;
    AResult := TValue.From(LStrings, TStrings);
  end
end;

function ConvStr2Enum(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, GetEnumValue(ATarget, ASource.AsString));
  Result := True;
end;

function ConvStr2Float(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  if ATarget = TypeInfo(TDate) then
    AResult := TValue.From<TDate>(StrToDateDef(ASource.AsString, 0))
  else if ATarget = TypeInfo(TDateTime) then
    AResult := TValue.From<TDateTime>(StrToDateTimeDef(ASource.AsString, 0))
  else if ATarget = TypeInfo(TTime) then
    AResult := TValue.From<TTime>(StrToTimeDef(ASource.AsString, 0))
  else
    AResult := TValue.FromFloat(ATarget, StrToFloatDef(ASource.AsString, 0));
  Result := True;
end;

function ConvStr2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, StrToInt64Def(ASource.AsString, 0));
  Result := True;
end;

{$ENDREGION}

{$REGION 'Conversions'}
const
  Conversions: array[TTypeKind, TTypeKind] of TConvertFunc = (
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvOrd2Str,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvEnum2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFloat2Ord, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFloat2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFloat2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvSet2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvClass2Enum, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvClass2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvRec2Meth, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInterface
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvIntf2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvIntf2Intf, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvStr2Ord, ConvFail, ConvStr2Enum, ConvStr2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvStr2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    )
  );
{$ENDREGION}

function FindType(const AName: string; out AType: TRttiType): Boolean; overload
var
  LType: TRttiType;
begin
  AType := Context.FindType(AName);
  if not Assigned(AType) then
  begin
    for LType in Context.GetTypes do
    begin
      if SameText(LType.Name, AName) then
      begin
        AType := LType;
        Break;
      end;
    end;
  end;
  Result := Assigned(AType);
end;

function FindType(const AGuid: TGUID; out AType: TRttiType): Boolean; overload;
var
  LType: TRttiType;
begin
  AType := nil;
  for LType in Context.GetTypes do
  begin
    if (LType is TRttiInterfaceType)
      and IsEqualGUID(TRttiInterfaceType(LType).GUID, AGuid) then
    begin
      AType := LType;
      Break;
    end;
  end;
  Result := Assigned(AType);
end;

function MergeStrings(Values: TStringDynArray; const Delimiter: string): string;
var
  i: Integer;
begin
  for i := Low(Values) to High(Values) do
  begin
    if i = 0 then
    begin
      Result := Values[i];
    end
    else
    begin
      Result := Result + Delimiter + Values[i];
    end;
  end;
end;

function StripUnitName(const s: string): string;
begin
  Result := ReplaceText(s, 'System.', '');
end;

{$IFDEF VER210}
function SplitString(const S: string; const Delimiter: Char): TStringDynArray;
var
  list: TStrings;
  i: Integer;
begin
  list := TStringList.Create;
  try
    list.StrictDelimiter := True;
    list.Delimiter := Delimiter;
    list.DelimitedText := s;
    SetLength(Result, list.Count);
    for i := Low(Result) to High(Result) do
    begin
      Result[i] := list[i];
    end;
  finally
    list.Free;
  end;
end;
{$ENDIF}

function CompareValue(const Left, Right: TValue): Integer;
var
  LLeft, LRight: TValue;
  LeftNull, RightNull: Boolean;
  LRel: TVariantRelationship;
begin
  if Left.IsEmpty or Right.IsEmpty then
  begin
    Result := 0;
    LeftNull := Left.IsEmpty;
    RightNull := Right.IsEmpty;
    if LeftNull then
    begin
      if RightNull then
        Result := 0
      else
        Result := 1;
    end
    else if RightNull then
    begin
      if LeftNull then
        Result := 0
      else
        Result := -1;
    end;
  end
  else
  if Left.IsOrdinal and Right.IsOrdinal then
  begin
    Result := Math.CompareValue(Left.AsOrdinal, Right.AsOrdinal);
  end else
  if Left.IsFloat and Right.IsFloat then
  begin
    Result := Math.CompareValue(Left.AsFloat, Right.AsFloat);
  end else
  if Left.IsString and Right.IsString then
  begin
    Result := SysUtils.AnsiCompareStr(Left.AsString, Right.AsString);
  end
  else if Left.IsObject and Right.IsObject then
  begin
    Result := NativeInt(Left.AsObject) - NativeInt(Right.AsObject);
  end
  else if (Left.TypeInfo = TypeInfo(Variant)) and (Right.TypeInfo = TypeInfo(Variant)) then
  begin
    Result := 0;
    LRel := VarCompareValue(Left.AsVariant, Right.AsVariant);
    case LRel of
      vrEqual: Result := 0;
      vrLessThan: Result := -1;
      vrGreaterThan: Result := 1;
      vrNotEqual: Result := -1;
    end;
  end
  else if TType.IsNullableType(Left.TypeInfo) and TType.IsNullableType(Right.TypeInfo) then
  begin
    LeftNull := not TType.TryGetNullableValue(Left, LLeft);
    RightNull := not TType.TryGetNullableValue(Right, LRight);
    if LeftNull then
    begin
      if RightNull then
        Result := 0
      else
        Result := 1;
    end
    else if RightNull then
    begin
      if LeftNull then
        Result := 0
      else
        Result := -1;
    end
    else
    begin
      Result := CompareValue(LLeft, LRight);
    end;
  end else
  begin
    Result := 0;
  end;
end;

procedure FreeValueObject(const AValue: TValue);
var
  LObj: TObject;
begin
  if AValue.IsObject then
  begin
    LObj := AValue.AsObject;
    if Assigned(LObj) then
      LObj.Free;
  end;
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.ExtractGenericArguments: string;
var
  i: Integer;
  s: string;
begin
  s := Name;
  i := Pos('<', s);
  if i > 0 then
  begin
    Result := Copy(s, Succ(i), Length(s) - Succ(i));
  end
  else
  begin
    Result := ''
  end;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetAttributesOfType<T>: TArray<T>;
var
  LAttribute: TCustomAttribute;
  LAttributes: TArray<T>;
  i: Integer;
begin
  SetLength(Result, 0);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := T(LAttribute);
    end;
  end;

  if Assigned(BaseType) then
  begin
    for LAttribute in BaseType.GetAttributesOfType<T> do
    begin
      if LAttribute.InheritsFrom(T) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := T(LAttribute);
      end;
    end;
  end;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  i: Integer;
  args: TStringDynArray;
begin
  args := SplitString(ExtractGenericArguments, ',');
  SetLength(Result, Length(args));
  for i := 0 to Pred(Length(args)) do
  begin
    FindType(args[i], Result[i]);
  end;
end;

function TRttiTypeHelper.GetGenericTypeDefinition(
  const AIncludeUnitName: Boolean = True): string;
var
  i: Integer;
  args: TStringDynArray;
  s: string;
begin
  args := SplitString(ExtractGenericArguments, ',');
  for i := Low(args) to High(args) do
  begin
    // naive implementation - but will work in most cases
    if (i = 0) and (Length(args) = 1) then
    begin
      args[i] := 'T';
    end
    else
    begin
      args[i] := 'T' + IntToStr(Succ(i));
    end;
  end;
  if IsPublicType and AIncludeUnitName then
  begin
    s := QualifiedName;
    Result := Copy(s, 1, Pos('<', s)) + MergeStrings(args, ',') + '>';
  end
  else
  begin
    s := Name;
    Result := Copy(s, 1, Pos('<', s)) + MergeStrings(args, ',') + '>';
  end;
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetMethod(ACodeAddress: Pointer): TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods do
  begin
    if LMethod.CodeAddress = ACodeAddress then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

function TRttiTypeHelper.GetMethodCount: Integer;
begin
  Result := Length(GetMethods);
end;

function TRttiTypeHelper.GetStandardConstructor: TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods do
  begin
    if LMethod.IsConstructor and (Length(LMethod.GetParameters) = 0) then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

function TRttiTypeHelper.InheritsFrom(OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  Result := Handle = OtherType;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType;
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherType: PTypeInfo): Boolean;
var
  t: TRttiType;
  args, otherArgs: TArray<TRttiType>;
  i: Integer;
begin
  Result := False;
  t := Context.GetType(OtherType);
  if Assigned(t) and IsGenericTypeDefinition then
  begin
    if SameText(GetGenericTypeDefinition, t.GetGenericTypeDefinition)
      or SameText(GetGenericTypeDefinition(False), t.GetGenericTypeDefinition(False)) then
    begin
      Result := True;
      args := GetGenericArguments;
      otherArgs := t.GetGenericArguments;
      for i := Low(args) to High(args) do
      begin
        if args[i].IsInterface and args[i].IsInterface
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        if args[i].IsInstance and otherArgs[i].IsInstance
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        Result := False;
        Break;
      end;
    end
    else
    begin
      if Assigned(BaseType) then
      begin
        Result := BaseType.IsCovariantTo(OtherType);
      end;
    end;
  end
  else
  begin
    Result := InheritsFrom(OtherType);
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherClass: TClass): Boolean;
begin
  Result := Assigned(OtherClass) and IsCovariantTo(OtherClass.ClassInfo);
end;

function TRttiTypeHelper.IsGenericTypeDefinition: Boolean;
begin
  Result := Length(GetGenericArguments) > 0;
  if not Result and Assigned(BaseType) then
  begin
    Result := BaseType.IsGenericTypeDefinition;
  end;
end;

function TRttiTypeHelper.IsGenericTypeOf(const BaseTypeName: string): Boolean;
var
  s: string;
begin
  s := Name;
  Result := (Copy(s, 1, Succ(Length(BaseTypeName))) = (BaseTypeName + '<'))
    and (Copy(s, Length(s), 1) = '>');
end;

function TRttiTypeHelper.IsInheritedFrom(const OtherTypeName: string): Boolean;
var
  LType: TRttiType;
begin
  Result := SameText(Name, OtherTypeName)
    or (IsPublicType and SameText(QualifiedName, OtherTypeName));

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := SameText(LType.Name, OtherTypeName)
        or (LType.IsPublicType and SameText(LType.QualifiedName, OtherTypeName));
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsInheritedFrom(OtherType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  Result := Self.Handle = OtherType.Handle;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType.Handle;
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;
var
  i: Integer;
  args: TStringDynArray;
  s: string;
begin
  if IsPublicType then
  begin
    args := SplitString(ExtractGenericArguments, ',');
    for i := Low(args) to High(args) do
    begin
      args[i] := Context.GetType(TypeArguments[i]).QualifiedName;
    end;
    s := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
    Result := Context.FindType(s);
  end
  else
  begin
    Result := nil;
  end;
end;

function TRttiTypeHelper.TryGetField(const AName: string;
  out AField: TRttiField): Boolean;
begin
  AField := GetField(AName);
  Result := Assigned(AField);
end;

function TRttiTypeHelper.TryGetMethod(ACodeAddress: Pointer;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(ACodeAddress);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetMethod(const AName: string;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(AName);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetProperty(const AName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GetProperty(AName);
  Result := Assigned(AProperty);
end;

function TRttiTypeHelper.TryGetStandardConstructor(
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetStandardConstructor;
  Result := Assigned(AMethod);
end;

{ TValueHelper }

function TValueHelper.AsByte: Byte;
begin
  Result := AsType<Byte>;
end;

function TValueHelper.AsCardinal: Cardinal;
begin
  Result := AsType<Cardinal>;
end;

function TValueHelper.AsCurrency: Currency;
begin
  Result := AsType<Currency>;
end;

function TValueHelper.AsDate: TDate;
begin
  Result := AsType<TDate>;
end;

function TValueHelper.AsDateTime: TDateTime;
begin
  Result := AsType<TDateTime>;
end;

function TValueHelper.AsDouble: Double;
begin
  Result := AsType<Double>;
end;

function TValueHelper.AsFloat: Extended;
begin
  Result := AsType<Extended>;
end;

function TValueHelper.AsPointer: Pointer;
begin
  ExtractRawDataNoCopy(@Result);
end;

function TValueHelper.AsShortInt: ShortInt;
begin
  Result := AsType<ShortInt>;
end;

function TValueHelper.AsSingle: Single;
begin
  Result := AsType<Single>;
end;

function TValueHelper.AsSmallInt: SmallInt;
begin
  Result := AsType<SmallInt>;
end;

function TValueHelper.AsTime: TTime;
begin
  Result := AsType<TTime>;
end;

function TValueHelper.AsUInt64: UInt64;
begin
  Result := AsType<UInt64>;
end;

function TValueHelper.AsWord: Word;
begin
  Result := AsType<Word>;
end;

class function TValueHelper.From(ABuffer: Pointer;
  ATypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(ABuffer, ATypeInfo, Result);
end;

class function TValueHelper.From(AObject: TObject; AClass: TClass): TValue;
begin
  TValue.Make(NativeInt(AObject), AClass.ClassInfo, Result);
end;

class function TValueHelper.FromBoolean(const Value: Boolean): TValue;
begin
  Result := TValue.From<Boolean>(Value);
end;

class function TValueHelper.FromFloat(ATypeInfo: PTypeInfo;
  AValue: Extended): TValue;
begin
  case GetTypeData(ATypeInfo).FloatType of
    ftSingle: Result := TValue.From<Single>(AValue);
    ftDouble: Result := TValue.From<Double>(AValue);
    ftExtended: Result := TValue.From<Extended>(AValue);
    ftComp: Result := TValue.From<Comp>(AValue);
    ftCurr: Result := TValue.From<Currency>(AValue);
  end;
end;

class function TValueHelper.FromString(const Value: string): TValue;
begin
  Result := TValue.From<string>(Value);
end;

function TValueHelper.GetRttiType: TRttiType;
begin
  Result := Context.GetType(TypeInfo);
end;

function TValueHelper.GetType: TRttiType;
begin
  Result := Context.GetType(TypeInfo);
end;

function TValueHelper.IsBoolean: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Boolean);
end;

function TValueHelper.IsByte: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Byte);
end;

function TValueHelper.IsCardinal: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Cardinal);
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeUInt));
{$ENDIF}
end;

function TValueHelper.IsCurrency: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Currency);
end;

function TValueHelper.IsDate: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDateTime);
end;

function TValueHelper.IsDouble: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Double);
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.IsInstance: Boolean;
begin
  Result := Kind in [tkClass, tkInterface];
end;

function TValueHelper.IsInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Int64);
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInteger: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Integer);
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := Assigned(TypeInfo) and (TypeInfo.Kind = tkInterface);
end;

function TValueHelper.IsNumeric: Boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
end;

function TValueHelper.IsPointer: Boolean;
begin
  Result := Kind = tkPointer;
end;

function TValueHelper.IsRecord: Boolean;
begin
  Result := Kind = tkRecord;
end;

function TValueHelper.IsShortInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(ShortInt);
end;

function TValueHelper.IsSingle: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Single);
end;

function TValueHelper.IsSmallInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(SmallInt);
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString];
end;

function TValueHelper.IsTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TTime);
end;

function TValueHelper.IsUInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(UInt64);
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsVariant: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Variant);
end;

function TValueHelper.IsWord: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Word);
end;

function TValueHelper.ToObject: TObject;
begin
  if IsInterface then
    Result := AsInterface as TObject
  else
    Result := AsObject;
end;

class function TValueHelper.ToString(const Values: TArray<TValue>): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Values) to High(Values) do
  begin
    if i > Low(Values) then
    begin
      Result := Result + ', ';
    end;
    if Values[i].IsString then
    begin
      Result := Result + '''' + TValue.ToString(Values[i]) + '''';
    end
    else
    begin
      Result := Result + TValue.ToString(Values[i]);
    end;
  end;
end;

class function TValueHelper.ToString(const Value: TValue): string;
var
  LInterface: IInterface;
  LObject: TObject;
begin
  case Value.Kind of
    tkFloat:
    begin
      if Value.IsDate then
      begin
        Result := DateToStr(Value.AsDate);
      end else
      if Value.IsDateTime then
      begin
        Result := DateTimeToStr(Value.AsDateTime);
      end else
      if Value.IsTime then
      begin
        Result := TimeToStr(Value.AsTime);
      end else
      begin
        Result := Value.ToString;
      end;
    end;
    tkClass:
    begin
      LObject := Value.AsObject;
      Result := Format('%s($%x)', [StripUnitName(LObject.ClassName),
        NativeInt(LObject)]);
    end;
    tkInterface:
    begin
      LInterface := Value.AsInterface;
      LObject := LInterface as TObject;
      Result := Format('%s($%x) as %s', [StripUnitName(LObject.ClassName),
        NativeInt(LInterface), StripUnitName(string(Value.TypeInfo.Name))]);
    end
  else
    Result := Value.ToString;
  end;
end;

function TValueHelper.TryConvert(ATypeInfo: PTypeInfo;
  out AResult: TValue; out AFreeAfter: Boolean): Boolean;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LInterface: IInterface;
  LStream: TStream;
begin
  Result := False;
  AFreeAfter := False;
  if (Self.TypeInfo = nil) then
  begin
    Exit;
  end;

  if Assigned(ATypeInfo) then
  begin
    if (ATypeInfo = Self.TypeInfo) then
    begin
      AResult := Self;
      Exit(True);
    end;

    Result := Conversions[Kind, ATypeInfo.Kind](Self, ATypeInfo, AResult);

    if not Result then
    begin
      case Kind of
        tkClass:
        begin
          case ATypeInfo.Kind of
            tkClass:
            begin
              {TODO -oLinas -cGeneral : refactor into separate method or class}
              if (IsObject) and (AsObject <> nil) and (AsObject.InheritsFrom(TStream)) then
              begin
                if (ATypeInfo = System.TypeInfo(TPicture)) then
                begin
                  //load from TStream into TPicture
                  if TUtils.TryLoadFromStreamToPictureValue(Self.AsObject as TStream, AResult) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end;
              end
              else if TypeInfo = System.TypeInfo(TPicture) then
              begin
                LStream := nil;
                //convert from picture to stream to be able to add it as a parameter
                if (IsObject) and (AsObject <> nil) then
                begin
                  if (TPicture(AsObject).Graphic <> nil) then
                  begin
                    LStream := TMemoryStream.Create;
                    AFreeAfter := True;
                    TPicture(AsObject).Graphic.SaveToStream(LStream);
                    LStream.Position := 0;
                  end;
                end;
                AResult := LStream;
                Result := True;
              end
              else
              begin
                if IsTypeCovariantTo(TypeInfo, ATypeInfo) then
                begin
                  AResult := TValue.From(GetReferenceToRawData, ATypeInfo);
                  Result := True;
                end;
              end;
            end;
          end;
        end;

        tkInterface:
        begin
          case ATypeInfo.Kind of
            tkInterface:
            begin
              if IsTypeCovariantTo(TypeInfo, ATypeInfo) then
              begin
                AResult := TValue.From(GetReferenceToRawData, ATypeInfo);
                Result := True;
              end else if TryGetRttiType(TypeInfo, LType) and (ATypeInfo.Name = 'IList')
                and LType.IsGenericTypeOf('IList') and LType.TryGetMethod('AsList', LMethod) then
              begin
                LInterface := LMethod.Invoke(Self, []).AsInterface;
                TValue.Make(@LInterface, ATypeInfo, AResult);
                Result := True;
              end;
            end;
            tkClass:
            begin
              Result := TValue.From<TObject>(Self.AsInterface as TObject).TryConvert(ATypeInfo, AResult, AFreeAfter);
            end;
          end;
        end;

        {$IFDEF VER210}
        // workaround for bug in RTTI.pas (fixed in XE)
        tkUnknown:
        begin
          case ATypeInfo.Kind of
            tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
            begin
              AResult := TValue.FromOrdinal(ATypeInfo, 0);
              Result := True;
            end;
            tkFloat:
            begin
              AResult := TValue.From<Extended>(0);
              Result := True;
            end;
            tkUString:
            begin
              AResult := TValue.FromString('');
              Result := True;
            end;
          end;
        end;
        {$ENDIF}
      end;
    end;

    if not Result then
    begin
      Result := TryCast(ATypeInfo, AResult);
    end;
  end;
end;

initialization
  Enumerations := TCollections.CreateDictionary<PTypeInfo, TStrings>([doOwnsValues]);

finalization
  Enumerations := nil;

end.
