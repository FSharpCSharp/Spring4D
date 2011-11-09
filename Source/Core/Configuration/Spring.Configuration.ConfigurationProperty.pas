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

(*

notes of dev, Virion:
- i think we should consider autocreation of values in collection. only then it
  can totally mimic hashtable implemented in many dynamic languages such as PHP,
  JavaScript, Perl and others.

- collection of properties should contain reference to parent node's collection
  of properties. i think it is best choice to implement value inheritancy on
  collection level.

- property record must contain references to two collections - source of value
  (it can be one of parents collections, used to reading value) and target of
  value (it always is current node's collection, used to writing value).

- let's consider situation when developer using configuration declares his own
  TConfigurationProperty instance and initializes in following manner:

  my_record_instance := configuration_node.Properties[key];

  my_record_instance will contain references to collections, source and target.
  changes made on that instance will be reflected on configuration tree. is it
  bug or feature? :-) i propose to treat this like a feature (maybe it could be
  useful to have reference to single configuration property, connected with
  whole tree?) then we can introduce record's new method - Detach - which will
  nil collections references and really detach that instance from tree.

- i was thinking about DefaultValue and Validation features. it is clear for me,
  that default value set will disable value inheritancy. Validation could be
  done by predicate assigned to property, simply.

*)

unit Spring.Configuration.ConfigurationProperty experimental;

{$I Spring.inc}

interface

uses
  Rtti,

  Spring.Collections,
  Spring.Collections.Dictionaries;

type
  TConfigurationPropertiesCollection = class;

  TConfigurationPropertyValidator = reference to procedure(value: TValue);

  {$REGION 'Documentation'}
  ///	<summary>Is a TValue wrapper with some easy implicit
  ///	conversions.</summary>
  ///	<remarks>It may use ValueConverters also, if there is a information about
  ///	it in the configuration property.</remarks>
  {$ENDREGION}
  TConfigurationProperty = record
  private
    fValue: TValue;
    fSourceCollection: TConfigurationPropertiesCollection;
    fTargetCollection: TConfigurationPropertiesCollection;
    fKey: string;
    fValidator: TConfigurationPropertyValidator;
    fDefaultValue: TValue;
    //fConverter: IValueConverter;
  {$REGION 'Property Accessors'}
    function GetValue: TValue;
    procedure SetValue(const value: TValue);
    class function GetEmpty: TConfigurationProperty; static;
    function GetValidator: TConfigurationPropertyValidator;
    procedure SetValidator(const Value: TConfigurationPropertyValidator);
    function GetDefaultValue: TValue;
    procedure SetDefaultValue(const value: TValue);
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
    class property Empty: TConfigurationProperty read GetEmpty;
    {$REGION 'Easy output'}
      class operator Implicit(const operand: TConfigurationProperty): string;
      class operator Implicit(const operand: TConfigurationProperty): Integer;
      class operator Implicit(const operand: TConfigurationProperty): Extended;
      class operator Implicit(const operand: TConfigurationProperty): Int64;
      class operator Implicit(const operand: TConfigurationProperty): TObject;
      class operator Implicit(const operand: TConfigurationProperty): TClass;
      class operator Implicit(const operand: TConfigurationProperty): Boolean;
    {$ENDREGION}

    function IsInherited: Boolean;
    property Value: TValue read GetValue write SetValue;
    procedure Clear;
    property Validator: TConfigurationPropertyValidator read GetValidator write SetValidator;
    property DefaultValue: TValue read GetDefaultValue write SetDefaultValue;
  end;

  TConfigurationPropertiesCollection = class(TDictionary<string, TConfigurationProperty>, IDictionary<string, TConfigurationProperty>)
  public
    function GetItem(const key: string): TConfigurationProperty; override;
  end;

implementation

{$REGION 'TConfigurationProperty'}

function TConfigurationProperty.GetDefaultValue: TValue;
begin
  if Assigned(fSourceCollection) then
    Result := fSourceCollection[fKey].fDefaultValue
  else
    Result := fDefaultValue;
end;

class function TConfigurationProperty.GetEmpty: TConfigurationProperty;
begin
  Result.fValue := TValue.Empty;
end;

function TConfigurationProperty.GetValidator: TConfigurationPropertyValidator;
begin

end;

function TConfigurationProperty.GetValue: TValue;
begin
  if Assigned(fSourceCollection) then
    Result := fSourceCollection[fKey].fValue
  else
    Result := fValue;
end;

procedure TConfigurationProperty.SetDefaultValue(const value: TValue);
var
  prop: TConfigurationProperty;
begin
  if Assigned(fValidator) then
    fValidator(value);

  if Assigned(fTargetCollection) then
  begin
    prop.fDefaultValue := value;
    fTargetCollection[fKey] := prop;
  end
  else
    fDefaultValue := value;
end;

procedure TConfigurationProperty.SetValidator(
  const Value: TConfigurationPropertyValidator);
begin
  fValidator := Value;
end;

procedure TConfigurationProperty.SetValue(const value: TValue);
var
  prop: TConfigurationProperty;
begin
  if Assigned(fValidator) then
    fValidator(value);

  if Assigned(fTargetCollection) then
  begin
    prop.fValue := value;
    fTargetCollection[fKey] := prop;
  end
  else
    fValue := value;
end;

function TConfigurationProperty.IsInherited: Boolean;
begin
  Result := Assigned(fSourceCollection) and Assigned(fTargetCollection) and (fSourceCollection <> fTargetCollection);
end;

procedure TConfigurationProperty.Clear;
begin
  Value := TValue.Empty;
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

{ TConfigurationPropertiesCollection }

function TConfigurationPropertiesCollection.GetItem(
  const key: string): TConfigurationProperty;
begin
  Result := inherited GetItem(key);
  Result.fSourceCollection := Self; // value inheritancy must be implemented here
  Result.fTargetCollection := Self;
  Result.fKey := key;
end;

end.
