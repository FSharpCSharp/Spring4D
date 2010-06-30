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

  /// <summary>
  /// Base value converter interface
  /// </summary>
  IValueConverter = interface
  ['{048EF3F0-41B5-4019-9BD6-00B88CAA7275}']

    /// <param name="value">Rtti.TValue to convert</param>
    /// <param name="targetTypeInfo">Target Rtti.PTypeInfo structure</param>
    /// <returns>Returns <param name="value">converted</param> to type pointing by <param name="targetTypeInfo">parameter</param></returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;

    /// <param name="value">Rtti.TValue to convert</param>
    /// <param name="targetTypeInfo">Target Rtti.PTypeInfo structure</param>
    /// <param name="parameter">Additional Rtti.TValue formatting parameter, use when possible</param>
    /// <returns>Returns <param name="value">converted</param> to type pointing by <param name="targetTypeInfo">parameter</param></returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;

    /// <param name="value">Rtti.TValue to convert</param>
    /// <param name="targetTypeInfo">Target Rtti.PTypeInfo structure</param>
    /// <param name="targetValue">Target Rtti.TValue out parameter</param>
    /// <returns>Returns System.Boolean, True if converting with success</returns>
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;

    /// <param name="value">Rtti.TValue to convert</param>
    /// <param name="targetTypeInfo">Target Rtti.PTypeInfo structure</param>
    /// <param name="targetValue">Target Rtti.TValue out parameter</param>
    /// <param name="parameter">Additional Rtti.TValue formatting parameter, use when possible</param>
    /// <returns>Returns System.Boolean, True if converting with success</returns>
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
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and string/UnicodeString
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
  /// Provides conversion routine beetwen TObject and string
  /// </summary>
  TObjectToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TObject and IInterface
  /// </summary>
  /// <remarks>
  /// acc. to #82433 TValue.TryAsType<T> raised an AV because ConvClass2Intf is wrong
  /// </remarks>
  TObjectToInterfaceConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen TObject and TClass
  /// </summary>
  TObjectToClassConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen UnicodeString and WideString
  /// </summary>
  /// <remarks>
  /// acc. to #82487 Rtti.ConvStr2Str is wrong (when cast a unicode string to WideString)
  /// </remarks>
  TStringToWStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen UnicodeString and WideString
  /// </summary>
  TWStringToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Factory class that brings to live converter which are registered within global
  ///  converter registry scope
  /// </summary>
  TValueConverterFactory = class
  strict private
    type
      /// <summary>
      /// TypeInfo registry structure
      /// </summary>
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        SourceTypeKinds: TTypeKinds;
        TargetTypeInfo: PTypeInfo;
        TargetTypeKinds: TTypeKinds;
      end;

      TConverterPackage = record
        ConverterClass: TConverterClass;
        Converter: IValueConverter;
      end;
    class var fTypeInfoRegistry: TDictionary<TConvertedTypeInfo, TConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds, targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds: TTypeKinds;
      targetTypeInfo: PTypeInfo; converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeInfo: PTypeInfo;
      targetTypeKinds: TTypeKinds; converterClass: TConverterClass); overload;

    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter; deprecated 'Use GetConverter instead.';
    class function GetConverter(const sourceTypeInfo,
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
  Spring.Reflection,
  Spring.Helpers,
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
  TArgument.CheckNotNull(value.TypeInfo, 'value.TypeInfo');
  TArgument.CheckNotNull(targetTypeInfo, 'targetTypeInfo');
  try
    Result := DoConvertTo(value, targetTypeInfo, parameter);
  except
    /// <summary>
    /// In order to save nested exception, you need to raise new exceptions
    ///  via Exception.RaiseOuterException (Delphi’s style) or Exception.ThrowOuterException (C++ Builder’s style).
    /// </summary>
    Exception.RaiseOuterException(Exception.CreateResFmt(@SCouldNotConvertValue,
      [value.TypeInfo.Name, targetTypeInfo.Name]));
  end;
  if Result.IsEmpty then
    raise Exception.CreateResFmt(@SCouldNotConvertValue,
      [value.TypeInfo.Name, targetTypeInfo.Name]);
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

function TDefaultValueConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.GetConverter(value.TypeInfo, targetTypeInfo);
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
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(IntToStr(value.AsInteger));
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(IntToStr(value.AsInteger)));
    tkWString:
      Result := TValue.From<WideString>(IntToStr(value.AsInteger));
  end;
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo.Name = 'Integer' then
    Result := TValue.From<Integer>(StrToInt(value.AsString))
  else if targetTypeInfo.Name = 'SmallInt' then
    Result := TValue.From<SmallInt>(StrToInt(value.AsString))
  else if targetTypeInfo.Name = 'ShortInt' then
    Result := TValue.From<ShortInt>(StrToInt(value.AsString))
  else if targetTypeInfo.Name = 'LongInt' then
    Result := TValue.From<ShortInt>(StrToInt(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(BoolToStr(value.AsBoolean, True));
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(BoolToStr(value.AsBoolean, True)));
    tkWString:
      Result := TValue.From<WideString>(BoolToStr(value.AsBoolean, True));
  end;
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
  if targetTypeInfo.Name = 'Integer' then
    Result := TValue.From<Integer>(Integer(value.AsBoolean))
  else if targetTypeInfo.Name = 'SmallInt' then
    Result := TValue.From<SmallInt>(Integer(value.AsBoolean))
  else if targetTypeInfo.Name = 'ShortInt' then
    Result := TValue.From<ShortInt>(Integer(value.AsBoolean))
  else if targetTypeInfo.Name = 'LongInt' then
    Result := TValue.From<ShortInt>(Integer(value.AsBoolean));
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
  ws: WideString;
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
    else
      case underlyingTypeInfo.Kind of
        tkUString, tkLString:
        begin
          us := PString(p)^;
          PPointer(@valueBuffer[0])^ := Pointer(us);
        end;
        tkWString:
        begin
          ws := PWideString(p)^;
          PPointer(@valueBuffer[0])^ := Pointer(ws);
        end;
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
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(enumName);
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(enumName));
    tkWString:
      Result := TValue.From<WideString>(enumName);
  end;
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
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatFloat(format, value.AsExtended));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatFloat(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatFloat(format, value.AsExtended));
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FloatToStr(value.AsExtended));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FloatToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FloatToStr(value.AsExtended));
    end;
end;

{$ENDREGION}


{$REGION 'TFloatToIntegerConverter'}

function TFloatToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo.Name = 'Integer' then
    Result := TValue.From<Integer>(Floor(value.AsExtended))
  else if targetTypeInfo.Name = 'Integer' then
    Result := TValue.From<SmallInt>(Floor(value.AsExtended))
  else if targetTypeInfo.Name = 'SmallInt' then
    Result := TValue.From<SmallInt>(Floor(value.AsExtended))
  else if targetTypeInfo.Name = 'ShortInt' then
    Result := TValue.From<ShortInt>(Floor(value.AsExtended))
  else if targetTypeInfo.Name = 'LongInt' then
    Result := TValue.From<LongInt>(Floor(value.AsExtended))
end;

{$ENDREGION}


{$REGION 'TStringToFloatConverter'}

function TStringToFloatConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo.Name = 'Extended' then
    Result := TValue.From<Extended>(StrToFloat(value.AsString))
  else if targetTypeInfo.Name = 'Double' then
    Result := TValue.From<Double>(StrToFloat(value.AsString))
  else if targetTypeInfo.Name = 'Single' then
    Result := TValue.From<Single>(StrToFloat(value.AsString));
end;

{$ENDREGION}


{$REGION 'TColorToStringConverter'}

function TColorToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(ColorToString(value.AsType<TColor>));
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(ColorToString(value.AsType<TColor>)));
    tkWString:
      Result := TValue.From<WideString>(ColorToString(value.AsType<TColor>));
  end;
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
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatCurr(format, value.AsType<Currency>));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatCurr(format, value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(FormatCurr(format, value.AsType<Currency>));
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(CurrToStr(value.AsType<Currency>));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(CurrToStr(value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(CurrToStr(value.AsType<Currency>));
    end;
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
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatDateTime(format, value.AsExtended));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatDateTime(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatDateTime(format, value.AsExtended));
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(DateTimeToStr(value.AsExtended));
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(DateTimeToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(DateTimeToStr(value.AsExtended));
    end;
end;

{$ENDREGION}


{$REGION 'TStringToDateTimeConverter'}

function TStringToDateTimeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    Result := TValue.From<TDateTime>(ConvertStrToDateTime(value.AsString, format));
  end
  else
    Result := TValue.From<TDateTime>(StrToDateTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TObjectToStringConverter'}

function TObjectToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsObject.ToString);
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsObject.ToString));
    tkWString:
      Result := TValue.From<WideString>(value.AsObject.ToString);
  end;
end;

{$ENDREGION}


{$REGION 'TObjectToInterfaceConverter'}

function TObjectToInterfaceConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  guid: TGUID;
  p: Pointer;
begin
  guid := GetTypeData(targetTypeInfo)^.Guid;
  if value.AsObject.GetInterface(guid, p) then
    TValue.MakeWithoutCopy(@p, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TObjectToClassConverter'}

function TObjectToClassConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TClass>(value.AsObject.ClassType);
end;

{$ENDREGION}


{$REGION 'TStringToWStringConverter'}

function TStringToWStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<WideString>(value.AsString);
end;

{$ENDREGION}


{$REGION 'TWStringToStringConverter'}

function TWStringToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsString);
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsString));
    tkWString:
      Result := TValue.From<WideString>(value.AsString);
  end;
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fTypeInfoRegistry := TDictionary<TConvertedTypeInfo, TConverterPackage>.Create;
  RegisterConverter(TypeInfo(Integer), TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Integer), [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);
  RegisterConverter(TypeInfo(Integer), [tkEnumeration], TIntegerToEnumConverter);

  RegisterConverter(TypeInfo(SmallInt), TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), TypeInfo(TNullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(SmallInt), [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);
  RegisterConverter(TypeInfo(SmallInt), [tkEnumeration], TIntegerToEnumConverter);

  RegisterConverter(TypeInfo(ShortInt), TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), TypeInfo(TNullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(ShortInt), [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);
  RegisterConverter(TypeInfo(ShortInt), [tkEnumeration], TIntegerToEnumConverter);

  RegisterConverter(TypeInfo(LongInt), TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), TypeInfo(TNullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(LongInt), [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);
  RegisterConverter(TypeInfo(LongInt), [tkEnumeration], TIntegerToEnumConverter);

  RegisterConverter(TypeInfo(Extended), TypeInfo(Integer), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(SmallInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(ShortInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(LongInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Extended), [tkString, tkUString, tkLString, tkWString], TFloatToStringConverter);

  RegisterConverter(TypeInfo(Double), TypeInfo(Integer), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(SmallInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(ShortInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(LongInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Double), [tkString, tkUString, tkLString, tkWString], TFloatToStringConverter);

  RegisterConverter(TypeInfo(Single), TypeInfo(Integer), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(SmallInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(ShortInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(LongInt), TFloatToIntegerConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Single), [tkString, tkUString, tkLString, tkWString], TFloatToStringConverter);

  RegisterConverter(TypeInfo(TColor), [tkString, tkUString, tkLString, tkWString], TColorToStringConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);

  RegisterConverter(TypeInfo(Currency), [tkString, tkUString, tkLString, tkWString], TCurrencyToStringConverter);
  RegisterConverter(TypeInfo(Currency), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Currency), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Currency), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);

  RegisterConverter(TypeInfo(TDateTime), [tkString, tkUString, tkLString, tkWString], TDateTimeToStringConverter);
  RegisterConverter(TypeInfo(TDateTime), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TDateTime), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TDateTime), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);

  RegisterConverter(TypeInfo(Boolean), [tkString, tkUString, tkLString, tkWString], TBooleanToStringConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(Integer), TBooleanToIntegerConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(SmallInt), TBooleanToIntegerConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(ShortInt), TBooleanToIntegerConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(LongInt), TBooleanToIntegerConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);

  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Single), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(Single), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(Single), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(Single), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(string), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(AnsiString), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(WideString), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(string), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(AnsiString), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.SmallInt>), TypeInfo(WideString), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(string), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(AnsiString), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.ShortInt>), TypeInfo(WideString), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(string), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(AnsiString), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.LongInt>), TypeInfo(WideString), TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Single), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.string>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), TypeInfo(Single), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.AnsiString>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), TypeInfo(Single), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.WideString>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(Extended), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(Double), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), TypeInfo(Single), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Extended>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(Boolean), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Boolean>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<System.Boolean>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), TypeInfo(Integer), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), TypeInfo(SmallInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), TypeInfo(ShortInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), TypeInfo(LongInt), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(TNullable<Graphics.TColor>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(TNullable<System.TDateTime>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter([tkEnumeration], TypeInfo(Integer), TEnumToIntegerConverter);
  RegisterConverter([tkEnumeration], TypeInfo(SmallInt), TEnumToIntegerConverter);
  RegisterConverter([tkEnumeration], TypeInfo(ShortInt), TEnumToIntegerConverter);
  RegisterConverter([tkEnumeration], TypeInfo(LongInt), TEnumToIntegerConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter([tkEnumeration], [tkString, tkUString, tkLString, tkWString], TEnumToStringConverter);

  RegisterConverter([tkClass], TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter([tkClass], TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter([tkClass], TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter([tkClass], [tkString, tkUString, tkLString, tkWString], TObjectToStringConverter);
  RegisterConverter([tkClass], [tkInterface], TObjectToInterfaceConverter);
  RegisterConverter([tkClass], [tkClassRef], TObjectToClassConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Integer), TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(SmallInt), TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(ShortInt), TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(LongInt), TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Boolean), TStringToBooleanConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TColor), TStringToColorConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Currency), TStringToCurrencyConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TDateTime), TStringToDateTimeConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.WideString>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.Currency>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<Graphics.TColor>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TNullable<System.TDateTime>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Extended), TStringToFloatConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Double), TStringToFloatConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Single), TStringToFloatConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkEnumeration], TStringToEnumConverter);

  RegisterConverter([tkString, tkUString, tkLString], [tkWString], TStringToWStringConverter);
  RegisterConverter([tkWString], [tkString, tkUString, tkLString], TWStringToStringConverter);
end;

class destructor TValueConverterFactory.Destroy;
begin
  fTypeInfoRegistry.Free;
end;

class function TValueConverterFactory.GetConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
var
  typeInfoPair: TPair<TConvertedTypeInfo, TConverterPackage>;
  value: TConverterPackage;
begin
  System.MonitorEnter(fTypeInfoRegistry);
  try
    for typeInfoPair in fTypeInfoRegistry do
    begin
      if ((typeInfoPair.Key.SourceTypeInfo = sourceTypeInfo) and
        (typeInfoPair.Key.TargetTypeInfo = targetTypeInfo)) or
        ((typeInfoPair.Key.SourceTypeInfo = sourceTypeInfo) and
        (targetTypeInfo.Kind in typeInfoPair.Key.TargetTypeKinds)) or
        ((sourceTypeInfo.Kind in typeInfoPair.Key.SourceTypeKinds) and
        (targetTypeInfo.Kind in typeInfoPair.Key.TargetTypeKinds)) or
        ((sourceTypeInfo.Kind in typeInfoPair.Key.SourceTypeKinds) and
        (typeInfoPair.Key.TargetTypeInfo = targetTypeInfo)) then
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
end;

class function TValueConverterFactory.CreateConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
begin
  Result := GetConverter(sourceTypeInfo, targetTypeInfo);
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo; converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  TArgument.CheckNotNull(sourceTypeInfo, 'sourceTypeInfo');
  TArgument.CheckNotNull(targetTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeInfo := targetTypeInfo;
    key.SourceTypeKinds := [];
    key.TargetTypeKinds := [];
    fTypeInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeKinds,
  targetTypeKinds: TTypeKinds; converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  TArgument.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  TArgument.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptyTargetTypeKind);

  System.MonitorEnter(fTypeInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeInfo := nil;
    key.SourceTypeKinds := sourceTypeKinds;
    key.TargetTypeKinds := targetTypeKinds;
    key.TargetTypeInfo := nil;
    fTypeInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  const sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
  converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  TArgument.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  TArgument.CheckNotNull(targetTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeKinds := sourceTypeKinds;
    key.SourceTypeInfo := nil;
    key.TargetTypeInfo := targetTypeInfo;
    key.TargetTypeKinds := [];
    fTypeInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  const sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
  converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  TArgument.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptySourceTypeKind);
  TArgument.CheckNotNull(sourceTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeInfoRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeKinds := [];
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeInfo := nil;
    key.TargetTypeKinds := targetTypeKinds;
    fTypeInfoRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fTypeInfoRegistry);
  end;
end;

{$ENDREGION}

end.
