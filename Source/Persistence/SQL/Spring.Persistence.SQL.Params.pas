{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Persistence.SQL.Params;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  DB,
  Spring,
  Spring.Collections;

type
  TDBParamClass = class of TDBParam;

  /// <summary>
  ///   Represents query parameter.
  /// </summary>
  TDBParam = class
  private
    fName: string;
    fParamType: TFieldType;
    fValue: Variant;
    fTypeInfo: PTypeInfo;
  protected
    function GetName: string; virtual;
    procedure SetName(const name: string); virtual;
    procedure SetValue(const value: Variant); virtual;
    function FromTypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const name: string; const value: Variant); overload; virtual;
    constructor Create(const name: string; const value: TVarRec); overload; virtual;

    procedure SetFromTValue(const value: TValue);
    procedure SetFromVarRec(const value: TVarRec);
    procedure SetParamTypeFromTypeInfo(typeInfo: PTypeInfo);

    class function NormalizeParamName(const prefix: string; const paramName: string): string;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Name: string read GetName write SetName;
    property ParamType: TFieldType read fParamType;
    property Value: Variant read fValue write SetValue;
  end;

  TDBParams = record
  public
    class function Create(const values: array of const): IEnumerable<TDBParam>; static;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils;


{$REGION 'TDBParam'}

constructor TDBParam.Create;
begin
  inherited Create;
end;

constructor TDBParam.Create(const name: string; const value: Variant);
begin
  Create;
  fName := name;
  SetValue(value);
end;

constructor TDBParam.Create(const name: string; const value: TVarRec);
begin
  Create;
  fName := name;
  SetFromVarRec(value);
end;

function TDBParam.FromTypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType;
begin
  case typeInfo.Kind of
    tkInteger, tkSet: Result := ftInteger;
    tkInt64: Result := ftLargeint;
    tkChar, tkLString, tkString: Result := ftString;
    tkWChar, tkWString, tkUString: Result := ftWideString;
    tkEnumeration:
      if typeInfo = System.TypeInfo(Boolean) then
        Result := ftBoolean
      else
        Result := ftInteger;
    tkFloat:
      if typeInfo = System.TypeInfo(TDateTime) then
        Result := ftDateTime
      else if typeInfo = System.TypeInfo(TDate) then
        Result := ftDate
      else if typeInfo = System.TypeInfo(TTime) then
        Result := ftTime
      else if typeInfo = System.TypeInfo(Currency) then
        Result := ftCurrency
      else if typeInfo = System.TypeInfo(Extended) then
        Result := DB.ftExtended
      else
        Result := ftFloat;
    tkClass, tkArray, tkInterface, tkDynArray: Result := ftBlob;
    tkVariant: Result := ftVariant;
    tkRecord:
      if IsNullable(typeInfo) then
        Result := FromTypeInfoToFieldType(GetUnderlyingType(typeInfo))
      else
        Result := ftBlob;
    tkClassRef, tkPointer: Result := ftReference;
  else
    Result := ftUnknown;
  end;
end;

function TDBParam.GetName: string;
begin
  Result := AnsiUpperCase(fName);
  if not StartsStr(':', Result) then
    Result := ':' + Result;
end;

class function TDBParam.NormalizeParamName(const prefix,
  paramName: string): string;
begin
  Result := paramName;
  if StartsStr(prefix, paramName) then
    Result := Copy(paramName, 2, Length(paramName));
end;

procedure TDBParam.SetFromTValue(const value: TValue);
var
  variantValue: Variant;
begin
  variantValue := value.ToVariant;
  SetValue(variantValue);
end;

procedure TDBParam.SetFromVarRec(const value: TVarRec);
begin
  case value.VType of
{$IFNDEF NEXTGEN}
    vtAnsiString:
    begin
      fParamType := ftString;
      fValue := string(value.VAnsiString);
    end;
    vtString:
    begin
      fParamType := ftString;
      fValue := string(value.VString);
    end;
{$ENDIF}
{$IF Declared(WideString)}
    vtWideString:
    begin
      fParamType := ftWideString;
      fValue := string(value.VWideString);
    end;
{$IFEND}
    vtUnicodeString:
    begin
      fParamType := ftWideString;
      fValue := string(value.VUnicodeString);
    end;
    vtInt64:
    begin
      fParamType := ftLargeint;
      fValue := Int64(value.VInt64^);
    end;
    vtInteger:
    begin
      fParamType := ftInteger;
      fValue := value.VInteger;
    end;
    vtExtended:
    begin
      fParamType := DB.ftExtended;
      fValue := Extended(value.VExtended^);
    end;
    vtCurrency:
    begin
      fParamType := ftCurrency;
      fValue := Currency(value.VCurrency^);
    end;
    vtBoolean:
    begin
      fParamType := ftBoolean;
      fValue := value.VBoolean;
    end;
    vtVariant:
    begin
      fParamType := ftVariant;
      fValue := Variant(value.VVariant^);
    end
  else
    raise EBaseORMException.Create('Unknown parameter type');
  end;
end;

procedure TDBParam.SetName(const name: string);
begin
  fName := name;
end;

procedure TDBParam.SetParamTypeFromTypeInfo(typeInfo: PTypeInfo);
begin
  fTypeInfo := typeInfo;
  fParamType := FromTypeInfoToFieldType(fTypeInfo);
end;

procedure TDBParam.SetValue(const value: Variant);
begin
  fParamType := VarTypeToDataType(VarType(value));
  fValue := value;
end;

{$ENDREGION}


{$REGION 'TDBParams'}

class function TDBParams.Create(const values: array of const): IEnumerable<TDBParam>;
var
  i: Integer;
  param: TDBParam;
  params: IList<TDBParam>;
begin
  if Length(values) > 0 then
  begin
    params := TCollections.CreateObjectList<TDBParam>;
    for i := Low(values) to High(values) do
    begin
      param := TDBParam.Create;
      param.Name := Format(':%D', [i]);
      param.SetFromVarRec(values[i]);
      params.Add(param);
    end;
    Result := params;
  end
  else
    Result := TEnumerable.Empty<TDBParam>;
end;

{$ENDREGION}


end.
