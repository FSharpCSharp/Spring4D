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
  ['{4FCF1210-7B19-4B67-9B71-DE715D80D5EE}']
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
    function ConvertFrom(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
  end;

  /// <summary>
  /// Provides default converter shared instance
  /// </summary>
  TValueConverter = class abstract(TInterfacedObject, IValueConverter)
  private
    class var fDefaultConverter: IValueConverter;

    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
    function ConvertFrom(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
    class function GetDefault: IValueConverter; static;
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
  public
    class constructor Create;
    constructor Create; virtual;

    class property Default: IValueConverter read GetDefault;
  end;
  TValueConverterClass = class of TValueConverter;

  /// <summary>
  ///  Default value Converter makes the exact conversion
  /// </summary>
  /// <remarks>
  ///  There is three steps of doing so
  ///  1. find/lock "global" registry
  ///  2. use TValue.TryCast
  ///  3. use RTTI exploring and select apropriate converter
  ///  There are four different internall converter types
  ///  that can be selected to convert
  ///   * TNullable<T> and T
  ///   * Enumeration and Integer/string
  ///   * TColor and Integer/string
  ///   * Integer and string
  /// </remarks>
  TDefaultValueConverter = class(TValueConverter)
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  ///  Simply provides an conversion routine beetwen Integer and string
  /// </summary>
  TIntegerToStringConverter = class(TValueConverter)
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  ///  Simply provides an conversion routine beetwen Boolean and string
  /// </summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  ///  Provides an conversion routine beetwen TNullable<T> and T
  /// </summary>
  /// <remarks>
  ///  Internally it may use another Converter delegating
  ///  conversion routine
  /// </remarks>
  TNullableValueConverter = class(TValueConverter)
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  ///  Provides an conversion routine beetwen enum and Integer/string
  /// </summary>
  TEnumValueConverter = class(TValueConverter)
  protected
    function DoSourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function DoTargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  ///  Provides a registry to store information about available converters that can be use
  ///  to make a conversion beetwen each type
  /// </summary>
  TValueConverterFactory = class
  strict private
    type
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        TargetTypeInfo: PTypeInfo;
      end;

      TConverterPackage = record
        ConverterClass: TValueConverterClass;
        Converter: IValueConverter;
      end;
    class var fRegistry: TDictionary<TConvertedTypeInfo, TConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TValueConverterClass);
    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring;

{$REGION 'TValueConverter'}

constructor TValueConverter.Create;
begin
  inherited;
end;

class constructor TValueConverter.Create;
begin
  inherited;
  fDefaultConverter := TDefaultValueConverter.Create;
end;

class function TValueConverter.GetDefault: IValueConverter;
begin
  Exit(fDefaultConverter);
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Exit(DoSourceToTarget(value, targetTypeInfo, parameter));
end;

function TValueConverter.ConvertFrom(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Exit(DoTargetToSource(value, sourceTypeInfo, parameter));
end;

{$ENDREGION}


{$REGION 'TDefaultValueConverter'}

function TDefaultValueConverter.DoSourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  converter: IValueConverter;
  converterClass: TValueConverterClass;
begin
  ///  1. find/lock "global" registry
  converter := TValueConverterFactory.CreateConverter(value.TypeInfo, targetTypeInfo);
  converterClass := nil;
  if Assigned(converter) then
    Exit(converter.ConvertTo(value, targetTypeInfo, parameter))
  else ///  2. use TValue.TryCast
  if not value.TryCast(targetTypeInfo, Result) then
  begin
    ///  3. use RTTI exploring and select apropriate converter then
    ///     register it on global registry
    ///   * TNullable<T> to T
    if StrUtils.ContainsStr(GetTypeName(value.TypeInfo), 'TNullable') and
      (targetTypeInfo.Kind in [tkInteger, tkFloat, tkString, tkUString]) then
    begin
      converterClass := TNullableValueConverter;
    end
    else
    if (value.TypeInfo.Kind in [tkInteger, tkFloat, tkString, tkUString]) and
      StrUtils.ContainsStr(GetTypeName(targetTypeInfo), 'TNullable') then
    begin
      Exit(DoTargetToSource(value, targetTypeInfo, parameter));
    end
    else
    ///   * Enumeration to Integer/string
    if (value.TypeInfo.Kind = tkEnumeration) and
      (targetTypeInfo.Kind in [tkString, tkUString, tkInteger, tkFloat]) then
    begin
      converterClass := TEnumValueConverter;
    end
    else
    if (value.TypeInfo.Kind in [tkString, tkUString, tkInteger, tkFloat]) and
      (targetTypeInfo.Kind = tkEnumeration) then
    begin
      Exit(DoTargetToSource(value, targetTypeInfo, parameter));
    end
    else
    ///   * Integer to string
    if (value.TypeInfo.Kind = tkInteger) and
      (targetTypeInfo.Kind in [tkString, tkUString, tkWString]) then
    begin
      converterClass := TIntegerToStringConverter;
    end
    else
    if (value.TypeInfo.Kind in [tkString, tkUString, tkWString]) and
      (targetTypeInfo.Kind = tkInteger ) then
    begin
      Exit(DoTargetToSource(value, targetTypeInfo, parameter));
    end;
    ///   * ex. TColor to Integer/string, etc.
    if Assigned(converterClass) then
    begin
      TValueConverterFactory.RegisterConverter(value.TypeInfo,
        targetTypeInfo, converterClass);
      Exit(DoSourceToTarget(value, targetTypeInfo, parameter));
    end;
  end;
end;

function TDefaultValueConverter.DoTargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  converter: IValueConverter;
  converterClass: TValueConverterClass;
begin
  ///  1. find/lock "global" registry
  converter := TValueConverterFactory.CreateConverter(value.TypeInfo, sourceTypeInfo);
  converterClass := nil;
  if Assigned(converter) then
    Exit(converter.ConvertFrom(value, sourceTypeInfo, parameter))
  else ///  2. use TValue.TryCast
  if not value.TryCast(sourceTypeInfo, Result) then
  begin
    ///  3. use RTTI exploring and select apropriate converter then
    ///     register it on global registry
    ///   * TNullable<T> to T
    if (value.TypeInfo.Kind in [tkInteger, tkFloat, tkString, tkUString]) and
      StrUtils.ContainsStr(GetTypeName(sourceTypeInfo), 'TNullable') then
    begin
      converterClass := TNullableValueConverter;
    end
    else
    if StrUtils.ContainsStr(GetTypeName(value.TypeInfo), 'TNullable') and
      (sourceTypeInfo.Kind in [tkInteger, tkFloat, tkString, tkUString]) then
    begin
      Exit(DoSourceToTarget(value, sourceTypeInfo, parameter));
    end
    else
    ///   * Integer/string to Enumeration
    if (value.TypeInfo.Kind in [tkString, tkUString, tkInteger, tkFloat]) and
      (sourceTypeInfo.Kind = tkEnumeration) then
    begin
      converterClass := TEnumValueConverter;
    end
    else
    if (value.TypeInfo.Kind = tkEnumeration) and
      (sourceTypeInfo.Kind in [tkString, tkInteger, tkFloat]) then
    begin
      Exit(DoSourceToTarget(value, sourceTypeInfo, parameter));
    end
    else
    ///   * string to Integer
    if (value.TypeInfo.Kind in [tkString, tkUString, tkWString]) and
      (sourceTypeInfo.Kind = tkInteger ) then
    begin
      converterClass := TIntegerToStringConverter;
    end
    else
    if (value.TypeInfo.Kind = tkInteger) and
      (sourceTypeInfo.Kind in [tkString, tkUString, tkWString]) then
    begin
      Exit(DoSourceToTarget(value, sourceTypeInfo, parameter));
    end;
    ///   * ex. TColor to Integer/string, etc.
    if Assigned(converterClass) then
    begin
      TValueConverterFactory.RegisterConverter(value.TypeInfo,
        sourceTypeInfo, converterClass);
      Exit(DoTargetToSource(value, sourceTypeInfo, parameter));
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoSourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  try
    Exit(TValue.From<string>(IntToStr(value.AsInteger)));
  except
    Exit(TValue.Empty);
  end;
end;

function TIntegerToStringConverter.DoTargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  try
    Exit(TValue.From<Integer>(StrToInt(value.AsString)));
  except
    Exit(TValue.Empty);
  end;
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoSourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  token: string;
begin
  if not parameter.IsEmpty then
    token := parameter.AsString
  else
    token := 'False;True';
  if not Value.AsBoolean then
    token := Copy(token, 1, Pos(';', token) - 1)
  else
    token := Copy(token, Pos(';', token) + 1, Length(token));
  try
    Result := TValue.From<string>(token);
  except
  end;
end;

function TBooleanToStringConverter.DoTargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  token: string;
begin
  if not parameter.IsEmpty then
  begin
    token := Parameter.AsString;
    token := Copy(token, 1, Pos(';', token) - 1);
  end
  else
    token := 'False';
  try
    TValue.From<Boolean>(AnsiSameText(Value.AsString, token));
  except
  end;
end;

{$ENDREGION}


{$REGION 'TNullableValueConverter'}

function TNullableValueConverter.DoSourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  outValue: TValue;
begin
  if Spring.TryGetUnderlyingValue(value, outValue) then
    Exit(outValue);
end;

function TNullableValueConverter.DoTargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  outValue: TValue;
begin
  if sourceTypeInfo.Kind = tkInteger then
    TValue.Make(value.GetReferenceToRawData, TypeInfo(TNullable<Integer>), outValue)
  else
  if sourceTypeInfo.Kind = tkFloat then
    TValue.Make(value.GetReferenceToRawData, TypeInfo(TNullable<Double>), outValue)
  else
  if sourceTypeInfo.Kind in [tkString, tkUString] then
    TValue.Make(value.GetReferenceToRawData, TypeInfo(TNullable<string>), outValue);
end;

{$ENDREGION}


{$REGION 'TEnumValueConverter'}

function TEnumValueConverter.DoSourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin

end;

function TEnumValueConverter.DoTargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin

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
  targetTypeInfo: PTypeInfo; converterClass: TValueConverterClass);
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
