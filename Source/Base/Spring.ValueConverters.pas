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

unit Spring.ValueConverters;

interface

uses
  TypInfo,
  Rtti,
  Generics.Collections;

type

  IValueConverter = interface
  ['{048EF3F0-41B5-4019-9BD6-00B88CAA7275}']
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
  end;

  /// <summary>
  /// Base abstract class provides DefaultConverter
  ///  as an entry point to the user side
  /// </summary>
  TValueConverter = class abstract(TInterfacedObject, IValueConverter)
  private
    class var fDefaultConverter: IValueConverter;

    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
    class function GetDefault: IValueConverter; static;
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
  public
    class constructor Create;
    constructor Create; virtual;

    class property Default: IValueConverter read GetDefault;
  end;
  TConverterClass = class of TValueConverter;

  /// <summary>
  /// Provides default converter shared instance,
  ///  TDefaultValueConverter is the master in the process of conversion
  /// </summary>
  /// <remarks>
  /// There is three steps of doing so
  ///  1. find/lock "global" registry
  ///  2. use TValue.TryCast
  ///  3. use RTTI exploring and select apropriate converter
  ///  There are four different internall converter types
  ///  that can be selected to convert
  ///   * TNullable<T> and T
  ///   * Enumeration and Integer/string
  ///   * TColor and Integer/string
  ///   * Integer and string
  ///   * Enumeration and Integer/string
  /// </remarks>
  TDefaultValueConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  public
    class constructor Create;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and string
  /// </summary>
  TIntegerToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Integer
  /// </summary>
  TStringToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and Boolean
  /// </summary>
  TIntegerToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and Integer
  /// </summary>
  TBooleanToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and string
  /// </summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Boolean
  /// </summary>
  TStringToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TNullable<T> and T
  /// </summary>
  /// <remarks>
  /// Internally it use another Converter to delegate
  ///  conversion routine if necessary
  /// </remarks>
  TNullableToTypeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen T and TNullable<T>
  /// </summary>
  /// <remarks>
  /// Internally it use another Converter to delegate
  ///  conversion routine if necessary
  /// </remarks>
  TTypeToNullableConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and Integer
  /// </summary>
  TEnumToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen Integer and enumeration
  /// </summary>
  TIntegerToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and string
  /// </summary>
  TEnumToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and enumeration
  /// </summary>
  TStringToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen float and string
  /// </summary>
  TFloatToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen float and Integer
  /// </summary>
  TFloatToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and float
  /// </summary>
  TStringToFloatConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TColor and string
  /// </summary>
  TColorToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TColor and integer
  /// </summary>
  TColorToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and TColor
  /// </summary>
  TStringToColorConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen Currency and string
  /// </summary>
  TCurrencyToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and Currency
  /// </summary>
  TStringToCurrencyConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and TDateTime
  /// </summary>
  TStringToDateTimeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TDateTime and string
  /// </summary>
  TDateTimeToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Factory class that brings to live converter which are registered within global
  ///  converter registry
  /// </summary>
  TValueConverterFactory = class
  strict private
    type
      /// <summary>
      /// TypeInfo registry structure
      /// </summary>
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        TargetTypeInfo: PTypeInfo;
      end;

      /// <summary>
      /// TypeKind registry structure
      /// </summary>
      TConvertedTypeKind = record
        SourceTypeKinds: TTypeKinds;
        TargetTypeKinds: TTypeKinds;
      end;

      /// <summary>
      /// TypeKind/TypeInfo registry structure
      /// </summary>
      TConvertedTypeKindInfo = record
        SourceTypeKinds: TTypeKinds;
        TargetTypeInfo: PTypeInfo;
      end;

      TConverterPackage = record
        ConverterClass: TConverterClass;
        Converter: IValueConverter;
      end;
    class var fTypeInfoRegistry: TDictionary<TConvertedTypeInfo, TConverterPackage>;
    class var fTypeKindRegistry: TDictionary<TConvertedTypeKind, TConverterPackage>;
    class var fTypeKindInfoRegistry: TDictionary<TConvertedTypeKindInfo, TConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds, targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds: TTypeKinds;
      targetTypeInfo: PTypeInfo; converterClass: TConverterClass); overload;
    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter;
  end;

implementation

uses
  Windows,
  Graphics,
  StrUtils,
  SysUtils,
  Math,
  Spring,
  Spring.ResourceStrings;


{$REGION 'TValueConverter'}

class constructor TValueConverter.Create;
begin
  inherited;
  fDefaultConverter := TDefaultValueConverter.Create;
end;

constructor TValueConverter.Create;
begin
  inherited;
end;

class function TValueConverter.GetDefault: IValueConverter;
begin
  Exit(fDefaultConverter);
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo): TValue;
begin
  Exit(ConvertTo(value, targetTypeInfo, TValue.Empty));
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo;
  const parameter: TValue): TValue;
begin
  try
    Exit(DoConvertTo(value, targetTypeInfo, parameter));
  except
    /// <summary>
    /// In order to save nested exception, you need to raise new exceptions
    ///  via Exception.RaiseOuterException (Delphi’s style) or Exception.ThrowOuterException (C++ Builder’s style).
    /// </summary>
    Exception.RaiseOuterException(Exception.Create(Format(SCouldNotConvertValue,
      [value.TypeInfo.Name, targetTypeInfo.Name])));
  end;
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Exit(TryConvertTo(value, targetTypeInfo, targetValue, TValue.Empty));
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := DoConvertTo(value, targetTypeInfo, parameter);
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TDefaultValueConverter'}

class constructor TDefaultValueConverter.Create;
begin
  inherited;
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(string), TIntegerToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(Boolean), TIntegerToBooleanConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(Integer), TStringToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(Boolean), TStringToBooleanConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TColor), TStringToColorConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(Currency), TStringToCurrencyConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TDateTime), TStringToDateTimeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Currency>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.TDateTime>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(string), TColorToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(Integer), TColorToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(Currency), TypeInfo(string), TCurrencyToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Currency), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(TDateTime), TypeInfo(string), TDateTimeToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TDateTime), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(Integer), TFloatToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(string), TBooleanToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(Integer), TBooleanToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Integer), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Integer), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Extended), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(Extended), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(Boolean), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(Integer), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.TDateTime>), TypeInfo(string), TNullableToTypeConverter);

  TValueConverterFactory.RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter([tkEnumeration], [tkInteger], TEnumToIntegerConverter);;
  TValueConverterFactory.RegisterConverter([tkInteger], [tkEnumeration], TIntegerToEnumConverter);
  TValueConverterFactory.RegisterConverter([tkEnumeration], [tkString, tkUString, tkLString], TEnumToStringConverter);
  TValueConverterFactory.RegisterConverter([tkString, tkUString, tkLString], [tkEnumeration], TStringToEnumConverter);
  TValueConverterFactory.RegisterConverter([tkFloat], [tkString, tkUString, tkLString], TFloatToStringConverter);
  TValueConverterFactory.RegisterConverter([tkString, tkUString, tkLString], [tkFloat], TStringToFloatConverter);
end;

function TDefaultValueConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.CreateConverter(value.TypeInfo, targetTypeInfo);
  if Assigned(converter) then
    Result := converter.ConvertTo(value, targetTypeInfo, parameter)
  else
    value.TryCast(targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<string>(IntToStr(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Integer>(StrToInt(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<string>(BoolToStr(value.AsBoolean, True));
end;

{$ENDREGION}


{$REGION 'TStringToBooleanConverter'}

function TStringToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(StrToBool(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToIntegerConverter'}

function TBooleanToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Integer>(Integer(value.AsBoolean));
end;

{$ENDREGION}


{$REGION 'TIntegerToBooleanConverter'}

function TIntegerToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(Boolean(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TNullableToTypeConverter'}

function TNullableToTypeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  underlyingValue: TValue;
begin
  if Spring.TryGetUnderlyingValue(value, underlyingValue) and
      (underlyingValue.TypeInfo.Name <> targetTypeInfo.Name) then
  begin
    Result := TValueConverter.Default.ConvertTo(underlyingValue,
      targetTypeInfo, parameter)
  end
  else Result := underlyingValue;
end;

{$ENDREGION}


{$REGION 'TTypeToNullableConverter'}

function TTypeToNullableConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  underlyingTypeInfo: PTypeInfo;
  underlyingValue: TValue;
  valueBuffer: TBytes;
  hasValueFlag: string;
  p: Pointer;
  us: UnicodeString;
begin
  if TryGetUnderlyingTypeInfo(targetTypeInfo, underlyingTypeInfo) then
  begin
    underlyingValue := value;
    if underlyingTypeInfo.Name <> value.TypeInfo.Name then
      Result := TValueConverter.Default.TryConvertTo(value, underlyingTypeInfo,
        underlyingValue, parameter);

    p := underlyingValue.GetReferenceToRawData;
    SetLength(valueBuffer, GetTypeData(targetTypeInfo).RecSize);
    ZeroMemory(PByte(valueBuffer), Length(valueBuffer));
    if not IsManaged(underlyingTypeInfo) then
    begin
      Move(PByte(p)^, PByte(valueBuffer)^, Length(valueBuffer) - SizeOf(string));
    end
    else if underlyingTypeInfo.Kind in [tkWString, tkUString] then
    begin
      us := PString(p)^;
      PPointer(@valueBuffer[0])^ := Pointer(us);
    end;
    if value.GetReferenceToRawData <> nil then
    begin
      hasValueFlag := '@';
      PPointer(PByte(valueBuffer) + Length(valueBuffer) - SizeOf(string))^ := Pointer(hasValueFlag);
    end;
    p := PByte(valueBuffer);
    TValue.Make(p, targetTypeInfo, Result);
  end;
end;

{$ENDREGION}


{$REGION 'TEnumToStringConverter'}

function TEnumToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
  enumName: string;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  enumName := GetEnumName(value.TypeInfo, enumValue);
  Result := TValue.From<string>(enumName);
end;

{$ENDREGION}


{$REGION 'TStringToEnumConverter'}

function TStringToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := GetEnumValue(targetTypeInfo, value.AsString);
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TEnumToIntegerConverter'}

function TEnumToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TIntegerToEnumConverter'}

function TIntegerToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  TValue.Make(value.AsInteger, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TFloatToStringConverter'}

function TFloatToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    Result := TValue.From<string>(FormatFloat(format, value.AsExtended));
  end
  else
    Result := TValue.From<string>(FloatToStr(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TFloatToIntegerConverter'}

function TFloatToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Integer>(Floor(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TStringToFloatConverter'}

function TStringToFloatConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Extended>(StrToFloat(value.AsString));
end;

{$ENDREGION}


{$REGION 'TColorToStringConverter'}

function TColorToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<string>(ColorToString(value.AsType<TColor>));
end;

{$ENDREGION}


{$REGION 'TColorToIntegerConverter'}

function TColorToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Integer>(ColorToRGB(value.AsType<TColor>));
end;

{$ENDREGION}


{$REGION 'TStringToColorConverter'}

function TStringToColorConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TColor>(StringToColor(value.AsString));
end;

{$ENDREGION}


{$REGION 'TCurrencyToStringConverter'}

function TCurrencyToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<string>(CurrToStr(value.AsType<Currency>));
end;

{$ENDREGION}


{$REGION 'TStringToCurrencyConverter'}

function TStringToCurrencyConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Currency>(StrToCurr(value.AsString));
end;

{$ENDREGION}


{$REGION 'TDateTimeToStringConverter'}

function TDateTimeToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<string>(DateTimeToStr(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TStringToDateTimeConverter'}

function TStringToDateTimeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TDateTime>(StrToDateTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fTypeInfoRegistry := TDictionary<TConvertedTypeInfo, TConverterPackage>.Create;
  fTypeKindRegistry := TDictionary<TConvertedTypeKind, TConverterPackage>.Create;
  fTypeKindInfoRegistry := TDictionary<TConvertedTypeKindInfo, TConverterPackage>.Create;
end;

class destructor TValueConverterFactory.Destroy;
begin
  fTypeInfoRegistry.Free;
  fTypeKindRegistry.Free;
  fTypeKindInfoRegistry.Free;
end;

class function TValueConverterFactory.CreateConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
var
  typeInfoPair: TPair<TConvertedTypeInfo, TConverterPackage>;
  typeKindPair: TPair<TConvertedTypeKind, TConverterPackage>;
  typeKindInfoPair: TPair<TConvertedTypeKindInfo, TConverterPackage>;
  value: TConverterPackage;
begin
  System.MonitorEnter(fTypeInfoRegistry);
  try
    for typeInfoPair in fTypeInfoRegistry do
    begin
      if (typeInfoPair.Key.SourceTypeInfo = sourceTypeInfo) and
        (typeInfoPair.Key.TargetTypeInfo = targetTypeInfo) then
      begin
        value := typeInfoPair.Value;
        if not Assigned(typeInfoPair.Value.Converter) then
        begin
          value.Converter := typeInfoPair.Value.ConverterClass.Create;
          value.ConverterClass := typeInfoPair.Value.ConverterClass;
          fTypeInfoRegistry.AddOrSetValue(typeInfoPair.Key, value);
        end;
        Exit(value.Converter);
      end;
    end;
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
  System.MonitorEnter(fTypeKindRegistry);
  try
    if not Assigned(value.Converter) then
      for typeKindPair in fTypeKindRegistry do
      begin
        if (sourceTypeInfo.Kind in typeKindPair.Key.SourceTypeKinds) and
          (targetTypeInfo.Kind in typeKindPair.Key.TargetTypeKinds) then
        begin
          value := typeKindPair.Value;
          if not Assigned(typeKindPair.Value.Converter) then
          begin
            value.Converter := typeKindPair.Value.ConverterClass.Create;
            value.ConverterClass := typeKindPair.Value.ConverterClass;
            fTypeKindRegistry.AddOrSetValue(typeKindPair.Key, value);
          end;
          Exit(value.Converter);
        end;
      end;
  finally
    System.MonitorExit(fTypeKindRegistry);
  end;
  System.MonitorEnter(fTypeKindInfoRegistry);
  try
    if not Assigned(value.Converter) then
      for typeKindInfoPair in fTypeKindInfoRegistry do
      begin
        if (sourceTypeInfo.Kind in typeKindInfoPair.Key.SourceTypeKinds) and
          (targetTypeInfo = typeKindInfoPair.Key.TargetTypeInfo) then
        begin
          value := typeKindInfoPair.Value;
          if not Assigned(typeKindInfoPair.Value.Converter) then
          begin
            value.Converter := typeKindInfoPair.Value.ConverterClass.Create;
            value.ConverterClass := typeKindInfoPair.Value.ConverterClass;
            fTypeKindInfoRegistry.AddOrSetValue(typeKindInfoPair.Key, value);
          end;
          Exit(value.Converter);
        end;
      end;
  finally
    System.MonitorExit(fTypeKindInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo; converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  System.MonitorEnter(fTypeInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeInfo := targetTypeInfo;
    fTypeInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeKinds,
  targetTypeKinds: TTypeKinds; converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeKind;
begin
  System.MonitorEnter(fTypeKindRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeKinds := sourceTypeKinds;
    key.TargetTypeKinds := targetTypeKinds;
    fTypeKindRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeKindRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  const sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
  converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeKindInfo;
begin
  System.MonitorEnter(fTypeKindInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeKinds := sourceTypeKinds;
    key.TargetTypeInfo := targetTypeInfo;
    fTypeKindInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeKindInfoRegistry);
  end;
end;

{$ENDREGION}

end.
