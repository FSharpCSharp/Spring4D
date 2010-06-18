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
      const targetTypeInfo: PTypeInfo): TValue;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean;
  end;

  /// <summary>
  /// Base abstract class provides DefaultConverter
  ///  as an entry point to the user side
  /// </summary>
  TValueConverter = class abstract(TInterfacedObject, IValueConverter)
  private
    class var fDefaultConverter: IValueConverter;

    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean;
    class function GetDefault: IValueConverter; static;
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; virtual; abstract;
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; virtual; abstract;
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
      out targetValue: TValue): Boolean; override;
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
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Integer
  /// </summary>
  TStringToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Integer and Boolean
  /// </summary>
  TIntegerToBooleanConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and Integer
  /// </summary>
  TBooleanToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen Boolean and string
  /// </summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Simply provides conversion routine beetwen string and Boolean
  /// </summary>
  TStringToBooleanConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
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
      out targetValue: TValue): Boolean; override;
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
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and Integer
  /// </summary>
  TEnumToIntegerConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen Integer and enumeration
  /// </summary>
  TIntegerToEnumConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen enumeration and string
  /// </summary>
  TEnumToStringConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Provides conversion routine beetwen string and enumeration
  /// </summary>
  TStringToEnumConverter = class(TValueConverter)
  protected
    function DoTryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; override;
  end;

  /// <summary>
  /// Factory class that brings to live converter which are registered within global
  ///  converter registry
  /// </summary>
  TValueConverterFactory = class
  strict private
    type
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        TargetTypeInfo: PTypeInfo;
      end;

      TConverterPackage = record
        ConverterClass: TConverterClass;
        Converter: IValueConverter;
      end;
    class var fRegistry: TDictionary<TConvertedTypeInfo, TConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass);
    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter;
  end;

implementation

uses
  Windows,
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
  if not TryConvertTo(value, targetTypeInfo, Result) then
    raise Exception.Create(Format(SCouldNotConvertValue,
      [value.TypeInfo.Name, targetTypeInfo.Name]));
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Exit(DoTryConvertTo(value, targetTypeInfo, targetValue));
end;

{$ENDREGION}


{$REGION 'TDefaultValueConverter'}

class constructor TDefaultValueConverter.Create;
begin
  inherited;
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(string), TIntegerToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(Integer), TStringToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(string), TBooleanToStringConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(Boolean), TStringToBooleanConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Boolean), TypeInfo(Integer), TBooleanToIntegerConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(Boolean), TIntegerToBooleanConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(Integer), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.Integer>), TypeInfo(string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(System.string), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TNullable<System.string>), TypeInfo(Integer), TNullableToTypeConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(string), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.Integer>), TTypeToNullableConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Integer), TypeInfo(TNullable<System.string>), TTypeToNullableConverter);
end;

function TDefaultValueConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.CreateConverter(value.TypeInfo, targetTypeInfo);
  if Assigned(converter) then
    Exit(converter.TryConvertTo(value, targetTypeInfo, targetValue))
  else
    Result := value.TryCast(targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  try
    targetValue := TValue.From<string>(IntToStr(value.AsInteger));
    Exit(True);
  except
    Exit(False);
  end;
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  try
    targetValue := TValue.From<Integer>(StrToInt(value.AsString));
    Exit(True);
  except
    Exit(False);
  end;
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := TValue.From<string>(BoolToStr(value.AsBoolean, True));
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TStringToBooleanConverter'}

function TStringToBooleanConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := TValue.From<Boolean>(StrToBool(value.AsString));
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TBooleanToIntegerConverter'}

function TBooleanToIntegerConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := TValue.From<Integer>(Integer(value.AsBoolean));
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TIntegerToBooleanConverter'}

function TIntegerToBooleanConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := TValue.From<Boolean>(Boolean(value.AsInteger));
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TNullableToTypeConverter'}

function TNullableToTypeConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := Spring.TryGetUnderlyingValue(value, targetValue);
  if targetValue.TypeInfo.Name <> targetTypeInfo.Name then
    Result := TValueConverter.Default.TryConvertTo(targetValue, targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TTypeToNullableConverter'}

function TTypeToNullableConverter.DoTryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
var
  underlyingTypeInfo: PTypeInfo;
  underlyingValue: TValue;
  valueBuffer: TBytes;
  hasValueFlag: string;
  p: Pointer;
  us: UnicodeString;
begin
  Result := False;
  if TryGetUnderlyingTypeInfo(targetTypeInfo, underlyingTypeInfo) then
  begin
    underlyingValue := value;
    if underlyingTypeInfo.Name <> value.TypeInfo.Name then
      Result := TValueConverter.Default.TryConvertTo(value, underlyingTypeInfo, underlyingValue);

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
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
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
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
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
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
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
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := True;
  TValue.Make(value.AsInteger, targetTypeInfo, targetValue);
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fRegistry := TDictionary<TConvertedTypeInfo, TConverterPackage>.Create;
end;

class destructor TValueConverterFactory.Destroy;
begin
  fRegistry.Free;
end;

class function TValueConverterFactory.CreateConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
var
  pair: TPair<TConvertedTypeInfo, TConverterPackage>;
  value: TConverterPackage;
begin
  System.MonitorEnter(fRegistry);
  try
    for pair in fRegistry do
    begin
      if (pair.Key.SourceTypeInfo = sourceTypeInfo) and
        (pair.Key.TargetTypeInfo = targetTypeInfo) then
      begin
        value := pair.Value;
        if pair.Value.Converter = nil then
        begin
          value.Converter := pair.Value.ConverterClass.Create;
          value.ConverterClass := pair.Value.ConverterClass;
          fRegistry.AddOrSetValue(pair.Key, value);
        end;
        Exit(value.Converter);
      end;
    end;
  finally
    System.MonitorExit(fRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo; converterClass: TConverterClass);
var
  value: TConverterPackage;
  key: TConvertedTypeInfo;
begin
  System.MonitorEnter(fRegistry);
  try
    value.ConverterClass := converterClass;
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeInfo := targetTypeInfo;
    fRegistry.AddOrSetValue(key, value);
  finally
    System.MonitorExit(fRegistry);
  end;
end;

{$ENDREGION}

end.
