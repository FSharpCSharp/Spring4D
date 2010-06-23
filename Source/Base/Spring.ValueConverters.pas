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
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; virtual; abstract;
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
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  public
    class constructor Create;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and string
  /// </summary>
  TIntegerToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Integer
  /// </summary>
  TStringToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and Boolean
  /// </summary>
  TIntegerToBooleanConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and Integer
  /// </summary>
  TBooleanToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and string
  /// </summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Boolean
  /// </summary>
  TStringToBooleanConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
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
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
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
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and Integer
  /// </summary>
  TEnumToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen Integer and enumeration
  /// </summary>
  TIntegerToEnumConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and string
  /// </summary>
  TEnumToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and enumeration
  /// </summary>
  TStringToEnumConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen float and string
  /// </summary>
  TFloatToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and float
  /// </summary>
  TStringToFloatConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TColor and string
  /// </summary>
  TColorToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and TColor
  /// </summary>
  TStringToColorConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen Currency and string
  /// </summary>
  TCurrencyToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and Currency
  /// </summary>
  TStringToCurrencyConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and TDateTime
  /// </summary>
  TStringToDateTimeConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TDateTime and string
  /// </summary>
  TDateTimeToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; override;
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

      TConverterPackage = record
        ConverterClass: TConverterClass;
        Converter: IValueConverter;
      end;
    class var fTypeInfoRegistry: TDictionary<TConvertedTypeInfo, TConverterPackage>;
    class var fTypeKindRegistry: TDictionary<TConvertedTypeKind, TConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeKinds, targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload;
    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter;
  end;

implementation

uses
  Windows,
  Graphics,
  StrUtils,
  SysUtils,
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
  if not TryConvertTo(value, targetTypeInfo, Result, parameter) then
    raise Exception.Create(Format(SCouldNotConvertValue,
        [value.TypeInfo.Name, targetTypeInfo.Name]));
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Exit(DoTryConvertTo(value, targetTypeInfo, targetValue, TValue.Empty));
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  try
    Exit(DoTryConvertTo(value, targetTypeInfo, targetValue, parameter));
  except
    Result := False;

    /// <summary>
    /// In order to save nested exception, you need to raise new exceptions
    ///  via Exception.RaiseOuterException (Delphi’s style) or Exception.ThrowOuterException (C++ Builder’s style).
    /// </summary>
    Exception.RaiseOuterException(Exception.Create(Format(SCouldNotConvertValue,
        [value.TypeInfo.Name, targetTypeInfo.Name])));
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
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.TDateTime>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(string), TColorToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(Currency), TypeInfo(string), TCurrencyToStringConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(TDateTime), TypeInfo(string), TDateTimeToStringConverter);

  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);

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

  TValueConverterFactory.RegisterConverter([tkEnumeration], [tkInteger], TEnumToIntegerConverter);
  TValueConverterFactory.RegisterConverter([tkInteger], [tkEnumeration], TIntegerToEnumConverter);
  TValueConverterFactory.RegisterConverter([tkEnumeration], [tkString, tkUString, tkLString], TEnumToStringConverter);
  TValueConverterFactory.RegisterConverter([tkString, tkUString, tkLString], [tkEnumeration], TStringToEnumConverter);
  TValueConverterFactory.RegisterConverter([tkFloat], [tkString, tkUString, tkLString], TFloatToStringConverter);
  TValueConverterFactory.RegisterConverter([tkString, tkUString, tkLString], [tkFloat], TStringToFloatConverter);
end;

function TDefaultValueConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.CreateConverter(value.TypeInfo, targetTypeInfo);
  if Assigned(converter) then
    Exit(converter.TryConvertTo(value, targetTypeInfo, targetValue, parameter))
  else
    Result := value.TryCast(targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<string>(IntToStr(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Integer>(StrToInt(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<string>(BoolToStr(value.AsBoolean, True));
end;

{$ENDREGION}


{$REGION 'TStringToBooleanConverter'}

function TStringToBooleanConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Boolean>(StrToBool(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToIntegerConverter'}

function TBooleanToIntegerConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Integer>(Integer(value.AsBoolean));
end;

{$ENDREGION}


{$REGION 'TIntegerToBooleanConverter'}

function TIntegerToBooleanConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Boolean>(Boolean(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TNullableToTypeConverter'}

function TNullableToTypeConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  underlyingValue: TValue;
begin
  Result := Spring.TryGetUnderlyingValue(value, underlyingValue);
  if Result and (underlyingValue.TypeInfo.Name <> targetTypeInfo.Name) then
    Result := TValueConverter.Default.TryConvertTo(underlyingValue, targetTypeInfo,
      targetValue, parameter)
  else targetValue := underlyingValue;
end;

{$ENDREGION}


{$REGION 'TTypeToNullableConverter'}

function TTypeToNullableConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  underlyingTypeInfo: PTypeInfo;
  underlyingValue: TValue;
  valueBuffer: TBytes;
  hasValueFlag: string;
  p: Pointer;
  us: UnicodeString;
begin
  Result := TryGetUnderlyingTypeInfo(targetTypeInfo, underlyingTypeInfo);
  if Result then
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
    TValue.Make(p, targetTypeInfo, targetValue);
  end;
end;

{$ENDREGION}


{$REGION 'TEnumToStringConverter'}

function TEnumToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  enumValue: Integer;
  enumName: string;
begin
  Result := True;
  enumValue := PInteger(value.GetReferenceToRawData)^;
  enumName := GetEnumName(value.TypeInfo, enumValue);
  targetValue := TValue.From<string>(enumName);
end;

{$ENDREGION}


{$REGION 'TStringToEnumConverter'}

function TStringToEnumConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  enumValue: Integer;
begin
  Result := True;
  enumValue := GetEnumValue(targetTypeInfo, value.AsString);
  TValue.Make(enumValue, targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TEnumToIntegerConverter'}

function TEnumToIntegerConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  enumValue: Integer;
begin
  Result := True;
  enumValue := PInteger(value.GetReferenceToRawData)^;
  TValue.Make(enumValue, targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TIntegerToEnumConverter'}

function TIntegerToEnumConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  TValue.Make(value.AsInteger, targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TFloatToStringConverter'}

function TFloatToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  format: string;
begin
  Result := True;
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    targetValue := TValue.From<string>(FormatFloat(format, value.AsExtended));
  end
  else
    targetValue := TValue.From<string>(FloatToStr(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TStringToFloatConverter'}

function TStringToFloatConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Extended>(StrToFloat(value.AsString));
end;

{$ENDREGION}


{$REGION 'TColorToStringConverter'}

function TColorToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<string>(ColorToString(value.AsType<TColor>));
end;

{$ENDREGION}


{$REGION 'TStringToColorConverter'}

function TStringToColorConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<TColor>(StringToColor(value.AsString));
end;

{$ENDREGION}


{$REGION 'TCurrencyToStringConverter'}

function TCurrencyToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<string>(CurrToStr(value.AsType<Currency>));
end;

{$ENDREGION}


{$REGION 'TStringToCurrencyConverter'}

function TStringToCurrencyConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<Currency>(StrToCurr(value.AsString));
end;

{$ENDREGION}


{$REGION 'TDateTimeToStringConverter'}

function TDateTimeToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<string>(DateTimeToStr(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TStringToDateTimeConverter'}

function TStringToDateTimeConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  targetValue := TValue.From<TDateTime>(StrToDateTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fTypeInfoRegistry := TDictionary<TConvertedTypeInfo, TConverterPackage>.Create;
  fTypeKindRegistry := TDictionary<TConvertedTypeKind, TConverterPackage>.Create;
end;

class destructor TValueConverterFactory.Destroy;
begin
  fTypeInfoRegistry.Free;
  fTypeKindRegistry.Free;
end;

class function TValueConverterFactory.CreateConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
var
  typeInfoPair: TPair<TConvertedTypeInfo, TConverterPackage>;
  typeKindPair: TPair<TConvertedTypeKind, TConverterPackage>;
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

{$ENDREGION}

end.
