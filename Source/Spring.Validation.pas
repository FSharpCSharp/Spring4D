{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.Validation experimental;

{$I Spring.inc}

interface

uses
  Classes, Contnrs, Windows, Messages, Graphics, Controls,
  SysUtils, Variants, TypInfo,
  Spring.System;

type
  IValidator      = interface;
  IValueProvider  = interface;
  TBaseValidator  = class;
  TValidatorGroup = class;

  IValidator = interface
    procedure Validate;
    function GetIsValid: Boolean;
    function GetErrorMessage: string;
    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read GetErrorMessage;
  end;

  IValueProvider = interface(IValueProvider<Variant>)
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
  end;

  TBaseValidator = class(TInterfacedObject, IValidator)
  private
    fIsValid: Boolean;
    fErrorMessage: string;
    fProvider: IValueProvider;
    function GetIsValid: Boolean;
    function GetErrorMessage: string;
  protected
    function GetValue: Variant; virtual;
    function GetDisplayName: string; virtual;
    function DoAccept(const value: Variant): Boolean; virtual;
    function DoValidate(const value: Variant; out errorMessage: string): Boolean; virtual;
  public
    constructor Create(const provider: IValueProvider);
    procedure Validate; virtual;
    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read GetErrorMessage;
    property Provider: IValueProvider read fProvider;
  end;

  TValidatorGroup = class(TInterfaceBase, IValidator, IInterface)
  private
    fList: TInterfaceList;
    fFirstFailedValidator: IValidator;
    function GetCount: Integer;
    function GetItem(index: Integer): IValidator;
    procedure SetItem(index: Integer; const Value: IValidator);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetIsValid: Boolean;
    function GetErrorMessage: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Validate;
    procedure Add(const validator: IValidator);
    procedure Remove(const validator: IValidator);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[index: Integer]: IValidator read GetItem write SetItem;
    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read GetErrorMessage;
  end;

  // IRequiredValidator Label
  IRequiredValidator = interface
    ['{CDBC4AC3-0901-47C3-8BA7-3FBA792243C1}']
  end;

  TObjectPropertyValueProvider = class(TInterfacedObject, IValueProvider)
  private
    fObject: TObject;
    fPropertyName: string;
    fDisplayName: string;
    function GetDisplayName: string;
    function GetIsReadOnly: Boolean;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  public
    constructor Create(obj: TObject; const propertyName: string); overload;
    constructor Create(obj: TObject; const propertyName, displayName: string); overload;
    property DisplayName: string read GetDisplayName;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property Value: Variant read GetValue write SetValue;
  end;

  TErrorIndicator = class
  private
    fTitle: string;
    fDescription: string;
  public
    procedure ShowIcon(control: TWinControl; const hint: string = '');
    property Title: string read fTitle write fTitle;
    property Description: string read fDescription write fDescription;
  end;


  {$REGION 'Validators'}

  TRequiredValidator = class(TBaseValidator, IRequiredValidator)
  protected
    function DoValidate(const value: Variant; out errorMessage: string): Boolean; override;
  end;

  TRangeValidator = class(TBaseValidator)
  private
    fMinValue: Variant;
    fMaxValue: Variant;
  protected
    function DoAccept(const value: Variant): Boolean; override;
    function DoValidate(const value: Variant; out errorMessage: string): Boolean; override;
  public
    constructor Create(const provider: IValueProvider; const minValue, maxValue: Variant);
    property MinValue: Variant read fMinValue;
    property MaxValue: Variant read fMaxValue;
  end;

  TMinLengthValidator = class(TBaseValidator)
  private
    fMinLength: Integer;
  protected
    function DoAccept(const value: Variant): Boolean; override;
    function DoValidate(const value: Variant; out errorMessage: string): Boolean; override;
  public
    constructor Create(const provider: IValueProvider; const minLength: Integer);
    property MinLength: Integer read fMinLength;
  end;

  TMaxLengthValidator = class(TBaseValidator)
  private
    fMaxLength: Integer;
  protected
    function DoAccept(const value: Variant): Boolean; override;
    function DoValidate(const value: Variant; out errorMessage: string): Boolean; override;
  public
    constructor Create(const provider: IValueProvider; const maxLength: Integer);
    property MaxLength: Integer read fMaxLength;
  end;

  TValidationCompareOperator = (
    coEqualTo,
    coNotEqualTo,
    coGreaterThan,
    coGreaterThanOrEqualTo,
    coLessThan,
    coLessThanOrEqualTo
  );

  TCompareValidator = class(TBaseValidator)

  end;

  TRegularExpressionValidator = class(TBaseValidator)

  end;

  TUrlValidator = class(TBaseValidator)

  end;

  TEmailValidator = class(TBaseValidator)

  end;

  TISBNValidator = class(TBaseValidator)

  end;

  {$ENDREGION}


implementation

uses Spring.Resources;


{$REGION 'TObjectPropertyValueProvider'}

constructor TObjectPropertyValueProvider.Create(obj: TObject;
  const propertyName: string);
begin
  Create(obj, propertyName, propertyName);
end;

constructor TObjectPropertyValueProvider.Create(obj: TObject;
  const propertyName, displayName: string);
begin
  inherited Create;
  fObject := obj;
  fPropertyName := propertyName;
  fDisplayName := displayName;
end;

function TObjectPropertyValueProvider.GetDisplayName: string;
begin
  Result := fPropertyName;
end;

function TObjectPropertyValueProvider.GetIsReadOnly: Boolean;
var
  properyInfo: PPropInfo;
begin
  properyInfo := GetPropInfo(fObject.ClassInfo, fPropertyName);
  Result := (properyInfo <> nil) and Assigned(properyInfo.SetProc);
end;

function TObjectPropertyValueProvider.GetValue: Variant;
begin
  Result := GetPropValue(fObject, fPropertyName);
end;

procedure TObjectPropertyValueProvider.SetValue(const Value: Variant);
begin
  if not IsReadOnly then
    SetPropValue(fObject, fPropertyName, value);
end;

{$ENDREGION}


{$REGION 'TBaseValidator'}

constructor TBaseValidator.Create(const provider: IValueProvider);
begin
  inherited Create;
  fProvider := provider;
  fIsValid := True;
end;

function TBaseValidator.DoAccept(const value: Variant): Boolean;
begin
  Result := True;
end;

function TBaseValidator.DoValidate(const value: Variant;
  out errorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TBaseValidator.Validate;
var
  value: Variant;
begin
  value := GetValue;
  if DoAccept(value) then
  begin
    fIsValid := DoValidate(value, fErrorMessage);
  end
  else
  begin
    fIsValid := True;
    fErrorMessage := '';
  end;
end;

function TBaseValidator.GetDisplayName: string;
begin
  Result := Provider.DisplayName;
end;

function TBaseValidator.GetValue: Variant;
begin
  Result := Provider.Value;
end;

function TBaseValidator.GetErrorMessage: string;
begin
  Result := fErrorMessage;
end;

function TBaseValidator.GetIsValid: Boolean;
begin
  Result := fIsValid;
end;

{$ENDREGION}


{$REGION 'TRequiredValidator'}

function TRequiredValidator.DoValidate(const value: Variant;
  out errorMessage: string): Boolean;
begin
  Result := not VarIsNullOrEmpty(value);
  if not Result then
  begin
    errorMessage := Format(SValidationRequired, [GetDisplayName])
  end;
end;

{$ENDREGION}


{$REGION 'TRangeValidator'}

constructor TRangeValidator.Create(const provider: IValueProvider;
  const minValue, maxValue: Variant);
begin
  inherited Create(provider);
  fMinValue := minValue;
  fMaxValue := maxValue;
end;

function TRangeValidator.DoAccept(const value: Variant): Boolean;
begin
  Result := not VarIsNullOrEmpty(value) and (not VarIsNullOrEmpty(Self.MinValue) or
    not VarIsNullOrEmpty(Self.MaxValue));
end;

function TRangeValidator.DoValidate(const value: Variant;
  out errorMessage: string): Boolean;
begin
  if not VarIsNullOrEmpty(MinValue) and not VarIsNullOrEmpty(MaxValue) then
  begin
    Result := (value >= Self.MinValue) and (value <= Self.MaxValue);
    if not Result then
    begin
      errorMessage := Format(SValidationRange, [
        GetDisplayName, MinValue, MaxValue
      ]);
    end;
  end
  else if not VarIsNullOrEmpty(MinValue) then
  begin
    Result := value >= Self.MinValue;
    if not Result then
    begin
      errorMessage := Format(SValidationMinValue, [
        GetDisplayName, MinValue
      ]);
    end;
  end
  else if not VarIsNullOrEmpty(MaxValue) then
  begin
    Result := value <= Self.MaxValue;
    if not Result then
    begin
      errorMessage := Format(SValidationMaxValue, [
        GetDisplayName, MaxValue
      ]);
    end;
  end
  else
  begin
    Result := True;
  end;
end;

{$ENDREGION}


{$REGION 'TMinLengthValidator'}

constructor TMinLengthValidator.Create(const provider: IValueProvider;
  const minLength: Integer);
begin
  inherited Create(provider);
  fMinLength := minLength;
end;

function TMinLengthValidator.DoAccept(const value: Variant): Boolean;
begin
  Result := not VarIsNullOrEmpty(value) and (MinLength > 0);
end;

function TMinLengthValidator.DoValidate(const value: Variant;
  out errorMessage: string): Boolean;
var
  s: string;
begin
  s := VarToStrDef(value, '');
  Result := Length(s) >= MinLength;
  if not Result then
  begin
    errorMessage := Format(
      SValidationMinLength,
      [GetDisplayName, MinLength]
    );
  end;
end;

{$ENDREGION}


{$REGION 'TMaxLengthValidator'}

constructor TMaxLengthValidator.Create(const provider: IValueProvider;
  const maxLength: Integer);
begin
  inherited Create(provider);
  fMaxLength := maxLength;
end;

function TMaxLengthValidator.DoAccept(const value: Variant): Boolean;
begin
  Result := not VarIsNullOrEmpty(value) and (MaxLength > 0);
end;

function TMaxLengthValidator.DoValidate(const value: Variant;
  out errorMessage: string): Boolean;
var
  s: string;
begin
  s := VarToStrDef(value, '');
  Result := Length(s) <= MaxLength;
  if not Result then
  begin
    errorMessage := Format(
      SValidationMaxLength,
      [GetDisplayName, MaxLength]
    );
  end;
end;

{$ENDREGION}


{$REGION 'TValidatorGroup'}

constructor TValidatorGroup.Create;
begin
  inherited Create;
  fList := TInterfaceList.Create;
end;

destructor TValidatorGroup.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TValidatorGroup.Validate;
var
  validator: IValidator;
  i: Integer;
begin
  fFirstFailedValidator := nil;
  for i := 0 to fList.Count - 1 do
  begin
    validator := IValidator(fList[i]);
    validator.Validate;
    if not validator.IsValid then
    begin
      fFirstFailedValidator := validator;
      Break;
    end;
  end;
end;

procedure TValidatorGroup.Add(const validator: IValidator);
begin
  fList.Add(validator);
end;

procedure TValidatorGroup.Remove(const validator: IValidator);
begin
  fList.Remove(validator);
end;

procedure TValidatorGroup.Clear;
begin
  fList.Clear;
end;

function TValidatorGroup.GetErrorMessage: string;
begin
  if fFirstFailedValidator <> nil then
    Result := fFirstFailedValidator.ErrorMessage
  else
    Result := '';
end;

function TValidatorGroup.GetIsValid: Boolean;
begin
  if fFirstFailedValidator <> nil then
    Result := fFirstFailedValidator.IsValid
  else
    Result := True;
end;

function TValidatorGroup.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TValidatorGroup.GetItem(index: Integer): IValidator;
begin
  Result := IValidator(fList[index]);
end;

procedure TValidatorGroup.SetItem(index: Integer; const Value: IValidator);
begin
  fList[index] := value;
end;

function TValidatorGroup.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if (fFirstFailedValidator <> nil) and
    SameText(GUIDToString(iid), '{CDBC4AC3-0901-47C3-8BA7-3FBA792243C1}') then
  begin
    Result := fFirstFailedValidator.QueryInterface(iid, obj);
  end
  else
  begin
    Result := inherited QueryInterface(iid, obj);
  end;
end;

{$ENDREGION}


{$REGION 'TErrorIndicator'}

procedure TErrorIndicator.ShowIcon(control: TWinControl; const hint: string);
begin
end;

{$ENDREGION}

end.
