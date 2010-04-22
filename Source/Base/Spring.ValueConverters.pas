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
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
    function ConvertFrom(const value: TValue;
      const targetTypeInfo: PTypeInfo;
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
    function SourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
    function TargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
  public
    class constructor Create;
    constructor Create; virtual;

    class property Default: IValueConverter read GetDefault;
  end;
  TValueConverterClass = class of TValueConverter;

  /// <summary>
  /// Default value Converter makes the exact conversion
  /// </summary>
  /// <remarks>
  ///  There is three steps of doing so
  ///  1. find/lock "global" registry
  ///  2. use TValue.TryCast
  ///  3. use RTTI exploring
  /// </remarks>
  TDefaultValueConverter = class(TValueConverter)
  protected
    function SourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function TargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides a way to select appropriate Converter by using Rtti
  /// </summary>
  /// <remarks>
  ///  There is three different internall converter types
  ///  that can be selected to convert
  ///  * TNullable<T> and T
  ///  * Enumeration and Integer/string
  ///  * TColor and Integer/string
  /// </remarks>
  TRttiValueConverter = class(TValueConverter)
  protected
    function SourceToTarget(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
    function TargetToSource(const value: TValue;
      const sourceTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  /// <summary>
  /// Provides a registry to store information about available converters that can be use
  /// to make a conversion beetwen each type
  /// </summary>
  TValueConverterFactory = class
  private
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
      converterClass: TClass);
    class function CreateConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo): IValueConverter;
  end;

implementation

{$REGION 'TValueConverter'}

class constructor TValueConverter.Create;
begin
  fDefaultConverter := TDefaultValueConverter.Create;
end;

class function TValueConverter.GetDefault: IValueConverter;
begin
  Exit(fDefaultConverter);
end;

constructor TValueConverter.Create;
begin

end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  SourceToTarget(value, targetTypeInfo, parameter);
end;

function TValueConverter.ConvertFrom(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  TargetToSource(value, sourceTypeInfo, parameter);
end;

{$ENDREGION}


{$REGION 'TDefaultValueConverter'}

function TDefaultValueConverter.SourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin

end;

function TDefaultValueConverter.TargetToSource(const value: TValue;
  const sourceTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin

end;

{$ENDREGION}


{$REGION 'TRttiValueConverter'}

function TRttiValueConverter.SourceToTarget(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin

end;

function TRttiValueConverter.TargetToSource(const value: TValue;
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
  try
    System.MonitorEnter(fRegistry);
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
        Exit(pair.Value.Converter);
      end;
    end;
  finally
    System.MonitorExit(fRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo; converterClass: TClass);
begin
  System.MonitorEnter(fRegistry);
  //
  // register the converter here
  //
  System.MonitorExit(fRegistry);
end;

{$ENDREGION}

end.
