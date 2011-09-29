{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
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

unit Spring.Configuration.ConfigurationProperty experimental;

{$I Spring.inc}

interface

uses
  Rtti;

type
  {$REGION 'Documentation'}
  ///	<summary>Is a TValue wrapper with some easy implicit
  ///	conversions.</summary>
  ///	<remarks>It may use ValueConverters also, if there is a information about
  ///	it in the configuration property.</remarks>
  {$ENDREGION}
  TConfigurationProperty = record
  private
    fValue: TValue;
    //fDefault: TValue;
    //fConverter: IValueConverter;
  {$REGION 'Property Accessors'}
    function GetValue: TValue;
    procedure SetValue(const value: TValue);
  {$ENDREGION}
  public
    {$REGION 'Easy input'}
      class operator Implicit(const operand: string): TConfigurationProperty;
      class operator Implicit(operand: Integer): TConfigurationProperty;
      class operator Implicit(operand: Extended): TConfigurationProperty;
      class operator Implicit(operand: Int64): TConfigurationProperty;
      class operator Implicit(operand: TObject): TConfigurationProperty;
      class operator Implicit(operand: TClass): TConfigurationProperty;
      class operator Implicit(operand: Boolean): TConfigurationProperty;
    {$ENDREGION}
    class function From<T>(const operand: T): TConfigurationProperty; static;
    {$REGION 'Easy output'}
      class operator Implicit(const operand: TConfigurationProperty): string;
      class operator Implicit(const operand: TConfigurationProperty): Integer;
      class operator Implicit(const operand: TConfigurationProperty): Extended;
      class operator Implicit(const operand: TConfigurationProperty): Int64;
      class operator Implicit(const operand: TConfigurationProperty): TObject;
      class operator Implicit(const operand: TConfigurationProperty): TClass;
      class operator Implicit(const operand: TConfigurationProperty): Boolean;
    {$ENDREGION}
    property Value: TValue read GetValue write SetValue;
  end;

implementation

{$REGION 'TConfigurationAttribute'}

function TConfigurationProperty.GetValue: TValue;
begin
  Result := fValue;
end;

procedure TConfigurationProperty.SetValue(const value: TValue);
begin
  fValue := value;
end;

class function TConfigurationProperty.From<T>(const operand: T): TConfigurationProperty;
begin
  Result.Value := TValue.From<T>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: TObject): TConfigurationProperty;
begin
  Result := From<TObject>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: TClass): TConfigurationProperty;
begin
  Result := From<TClass>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: Boolean): TConfigurationProperty;
begin
  Result := From<Boolean>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: Int64): TConfigurationProperty;
begin
  Result := From<Int64>(operand);
end;

class operator TConfigurationProperty.Implicit(
  const operand: string): TConfigurationProperty;
begin
  Result := From<string>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: Integer): TConfigurationProperty;
begin
  Result := From<Integer>(operand);
end;

class operator TConfigurationProperty.Implicit(
  operand: Extended): TConfigurationProperty;
begin
  Result := From<Extended>(operand);
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): TObject;
begin
  Result := operand.Value.AsObject;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): TClass;
begin
  Result := operand.Value.AsClass;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): Boolean;
begin
  Result := operand.Value.AsBoolean;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): Int64;
begin
  Result := operand.Value.AsInt64;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): string;
begin
  Result := operand.Value.AsString;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): Integer;
begin
  Result := operand.Value.AsInteger;
end;

class operator TConfigurationProperty.Implicit(
  const operand: TConfigurationProperty): Extended;
begin
  Result := operand.Value.AsExtended;
end;

{$ENDREGION}

end.
