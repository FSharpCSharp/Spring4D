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
  Rtti, SysUtils,

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
  strict private
    const CInitializedFlag = '@';  // DO NOT LOCALIZE
  strict private
    fInitialized: string;
  private
    fValue: TValue;
    fSourceCollection: IDictionary<string, TConfigurationProperty>;
    fTargetCollection: IDictionary<string, TConfigurationProperty>;
    fKey: string;
    fValidator: TConfigurationPropertyValidator;
    fDefaultValue: TValue;

    procedure Initialize(sourceCollection, targetCollection: IDictionary<string, TConfigurationProperty>; key: string); overload;
    procedure Initialize(value: TConfigurationProperty); overload;
    function IsInitialized: boolean;
    //fConverter: IValueConverter;
  {$REGION 'Property Accessors'}
    function GetValue: TValue;
    procedure SetValue(const newValue: TValue);
    class function GetEmpty: TConfigurationProperty; static;
    function GetValidator: TConfigurationPropertyValidator;
    procedure SetValidator(const Value: TConfigurationPropertyValidator);
    function GetDefaultValue: TValue;
    procedure SetDefaultValue(const defaultValue: TValue);
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

  IConfigurationPropertiesCollection = interface(IDictionary<string, TConfigurationProperty>)
  ['{7AD8E4E8-3CDD-4EB3-8340-449C7DE0472E}']
    procedure SetParentNodeCollection(const value: IDictionary<string, TConfigurationProperty>);
    function GetParentNodeCollection: IDictionary<string, TConfigurationProperty>;
    property ParentNodeCollection: IDictionary<string, TConfigurationProperty> read GetParentNodeCollection write SetParentNodeCollection;
  end;

  TConfigurationPropertiesCollection = class(TDictionary<string, TConfigurationProperty>,
                                             IDictionary<string, TConfigurationProperty>,
                                             IConfigurationPropertiesCollection)
  private
    fParentNodeCollection: IDictionary<string, TConfigurationProperty>;
    procedure SetParentNodeCollection(const value: IDictionary<string, TConfigurationProperty>);
    function GetParentNodeCollection: IDictionary<string, TConfigurationProperty>;
  public
    destructor Destroy; override;
    function GetItem(const key: string): TConfigurationProperty; override;
    procedure SetItem(const key: string; const value: TConfigurationProperty); override;
    procedure Add(const key: string; const value: TConfigurationProperty); override;
    procedure AddOrSetValue(const key: string; const value: TConfigurationProperty); override;

    property ParentNodeCollection: IDictionary<string, TConfigurationProperty> read GetParentNodeCollection write SetParentNodeCollection;
  end;

implementation

{$REGION 'TConfigurationProperty'}

function TConfigurationProperty.GetDefaultValue: TValue;
begin
  if IsInitialized then
    Result := fTargetCollection[fKey].fDefaultValue
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
var
  fTargetValue: TValue;
  fTargetDefaultValue: TValue;
begin
  if IsInitialized then
    if IsInherited then
      if fTargetCollection.ContainsKey(fKey) then
      begin
        fTargetDefaultValue := fTargetCollection[fKey].DefaultValue;
        if not fTargetDefaultValue.IsEmpty then
          Result := fTargetDefaultValue
        else
          Result := fSourceCollection[fKey].fValue;
      end
      else
        Result := fSourceCollection[fKey].fValue
    else
      if fTargetCollection.ContainsKey(fKey) then
      begin
        fTargetValue := fTargetCollection[fKey].fValue;
        fTargetDefaultValue := fTargetCollection[fKey].DefaultValue;

        if fTargetValue.IsEmpty and not fTargetDefaultValue.IsEmpty then
          Result := fTargetDefaultValue
        else
          Result := fTargetValue;
      end
      else
        Result := fTargetCollection[fKey].fValue
  else
    Result := fValue;
end;

procedure TConfigurationProperty.SetDefaultValue(const defaultValue: TValue);
var
  prop: TConfigurationProperty;
begin
  if IsInitialized and Assigned(fValidator) then
    fValidator(value);

  if IsInitialized then
  begin
    prop.Initialize(Self);
    prop.fDefaultValue := defaultValue;
    fTargetCollection[fKey] := prop;
  end
  else
    fDefaultValue := value;
end;

procedure TConfigurationProperty.SetValidator(
  const Value: TConfigurationPropertyValidator);
var
  prop: TConfigurationProperty;
begin
  if IsInitialized then
  begin
    prop.Initialize(Self);
    prop.fValidator := value;
    fTargetCollection[fKey] := prop;
  end
  else
    fValidator := value;
end;

procedure TConfigurationProperty.SetValue(const newValue: TValue);
var
  prop: TConfigurationProperty;
begin
  if IsInitialized and Assigned(fValidator) then
    fValidator(newValue);

  if IsInitialized then
  begin
    fValue := newValue;
    prop.Initialize(Self);
    fTargetCollection[fKey] := prop;
  end
  else
    fValue := newValue;
end;

function TConfigurationProperty.IsInherited: Boolean;
begin
  Result := IsInitialized and (fSourceCollection <> fTargetCollection);
end;

function TConfigurationProperty.IsInitialized: boolean;
begin
  Result := Length(Self.fInitialized) > 0;
end;

procedure TConfigurationProperty.Clear;
begin
  Value := TValue.Empty;
end;

procedure TConfigurationProperty.Initialize(sourceCollection,
  targetCollection: IDictionary<string, TConfigurationProperty>; key: string);
begin
  Self.fSourceCollection := sourceCollection;
  Self.fTargetCollection := targetCollection;
  Self.fKey := key;
  Self.fInitialized := CInitializedFlag;
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

procedure TConfigurationProperty.Initialize(value: TConfigurationProperty);
begin
  Self := Value;
  Self.fInitialized := CInitializedFlag;
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

procedure TConfigurationPropertiesCollection.Add(const key: string;
  const value: TConfigurationProperty);
begin
  if not value.IsInitialized then
    value.Initialize(Self, Self, key);

  inherited Add(key, value);
end;

procedure TConfigurationPropertiesCollection.AddOrSetValue(const key: string;
  const value: TConfigurationProperty);
begin
  if not value.IsInitialized then
    value.Initialize(Self, Self, key);

  inherited AddOrSetValue(key, value);
end;

destructor TConfigurationPropertiesCollection.Destroy;
begin
  fParentNodeCollection := nil;
  inherited;
end;

function TConfigurationPropertiesCollection.GetItem(
  const key: string): TConfigurationProperty;
begin
  if TryGetValue(key, Result) then
    Result.Initialize(Self, Self, key)
  else
    SetItem(key, Result);

  if Result.fValue.IsEmpty then
  begin
    if Assigned(ParentNodeCollection) then
    begin
      Result := ParentNodeCollection[key];

      if Result.fValue.IsEmpty then
      begin
        Result := inherited GetItem(key);
        Result.Initialize(Self, Self, key);
      end
      else
        Result.Initialize(Result.fTargetCollection, Self, key);
    end;
  end;
end;

function TConfigurationPropertiesCollection.GetParentNodeCollection: IDictionary<string, TConfigurationProperty>;
begin
  Result := fParentNodeCollection;
end;

procedure TConfigurationPropertiesCollection.SetItem(const key: string;
  const value: TConfigurationProperty);
var
  prop: TConfigurationProperty;
begin
  if value.IsInitialized then
    inherited SetItem(key, value)
  else
  begin
    if not TryGetValue(key, prop) then
      prop.Initialize(Self, Self, key);

    prop.Value := value.Value;
  end;
end;

procedure TConfigurationPropertiesCollection.SetParentNodeCollection(
  const value: IDictionary<string, TConfigurationProperty>);
begin
  fParentNodeCollection := value;
end;

end.
