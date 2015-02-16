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

interface

uses
  DB,
  TypInfo,
  Spring,
  Spring.Collections;

type
  TDBParamClass = class of TDBParam;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents query parameter.
  ///	</summary>
  {$ENDREGION}
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
    constructor Create(const name: string; const paramValue: Variant); overload; virtual;
    destructor Destroy; override;

    procedure SetFromTValue(const fromValue: TValue);
    procedure SetParamTypeFromTypeInfo(typeInfo: PTypeInfo);

    class function NormalizeParamName(const prefix: string; const paramName: string): string;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Name: string read GetName write SetName;
    property ParamType: TFieldType read fParamType;
    property Value: Variant read fValue write SetValue;
  end;

  procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
  procedure ConvertParams(const AFrom: array of const; ATo: IList<TDBParam>);
  function FromTValueTypeToFieldType(const AValue: TValue): TFieldType;

implementation

uses
  SysUtils,
  StrUtils,
  Variants,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils;

procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
begin
  case AFrom.VType of
    vtAnsiString:
    begin
      ATo.fParamType := ftString;
      ATo.fValue := string(AFrom.VAnsiString);
    end;
    vtWideString:
    begin
      ATo.fParamType := ftWideString;
      ATo.fValue := string(AFrom.VWideString);
    end;
    vtUnicodeString:
    begin
      ATo.fParamType := ftWideString;
      ATo.fValue := string(AFrom.VUnicodeString);
    end;
    vtString:
    begin
      ATo.fParamType := ftString;
      ATo.fValue := string(AFrom.VString);
    end;
    vtInt64:
    begin
      ATo.fParamType := ftLargeint;
      ATo.fValue := Int64(AFrom.VInt64^);
    end;
    vtInteger:
    begin
      ATo.fParamType := ftInteger;
      ATo.fValue := AFrom.VInteger;
    end;
    vtExtended:
    begin
      ATo.fParamType := DB.ftExtended;
      ATo.fValue := Extended(AFrom.VExtended^);
    end;
    vtCurrency:
    begin
      ATo.fParamType := ftCurrency;
      ATo.fValue := Currency(AFrom.VCurrency^);
    end;
    vtBoolean:
    begin
      ATo.fParamType := ftBoolean;
      ATo.fValue := AFrom.VBoolean;
    end;
    vtVariant:
    begin
      ATo.fParamType := ftVariant;
      ATo.fValue := Variant(AFrom.VVariant^);
    end
    else
    begin
      raise EBaseORMException.Create('Unknown parameter type');
    end;
  end;
end;

procedure ConvertParams(const AFrom: array of const; ATo: IList<TDBParam>);
var
  i: Integer;
  LParam: TDBParam;
begin
  for i := Low(AFrom) to High(AFrom) do
  begin
    LParam := TDBParam.Create;
    LParam.Name := Format(':%D', [i]);
    ConvertParam(AFrom[i], LParam);
    ATo.Add(LParam);
  end;
end;

function FromTValueTypeToFieldType(const AValue: TValue): TFieldType;
var
  LVariant: Variant;
begin
  LVariant := TUtils.AsVariant(AValue);
  Result := VarTypeToDataType(VarType(LVariant));
end;



{ TDBParam }

constructor TDBParam.Create(const name: string; const paramValue: Variant);
begin
  Create;
  fName := name;
  SetValue(paramValue);
end;

constructor TDBParam.Create;
begin
  inherited Create;
end;

destructor TDBParam.Destroy;
begin
  inherited Destroy;
end;

function TDBParam.FromTypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType;
begin
  case typeInfo.Kind of
    tkUnknown: Result := ftUnknown;
    tkInteger: Result := ftInteger;
    tkInt64: Result := ftLargeint;
    tkChar, tkLString, tkString: Result := ftString;
    tkWChar, tkWString, tkUString: Result := ftWideString;
    tkEnumeration, tkSet:
    begin
      if typeInfo = System.TypeInfo(Boolean) then
        Result := ftBoolean
      else
        Result := ftInteger;
    end;
    tkFloat:
    begin
      if typeInfo = System.TypeInfo(TDateTime) then
        Result := ftDateTime
      else if typeInfo = System.TypeInfo(TDate) then
        Result := ftDate
      else
        Result := ftFloat;
    end;
    tkClass, tkArray, tkInterface, tkDynArray: Result := ftBlob;
    tkVariant: Result := ftVariant;
    tkRecord:
    begin
      Result := ftBlob;
      if IsNullable(typeInfo) then
        Result := FromTypeInfoToFieldType(GetUnderlyingType(typeInfo));
    end;
    tkClassRef, tkPointer: Result := ftReference;
    else
      Result := ftUnknown;
  end;
end;

function TDBParam.GetName: string;
begin
  Result := UpperCase(fName);
  if (Length(Result) > 0) and not (CharInSet(Result[1], [':'])) then
  begin
    Result := ':' + Result;
  end;
end;

class function TDBParam.NormalizeParamName(const prefix,
  paramName: string): string;
begin
  Result := paramName;
  if (paramName <> '') and StartsStr(prefix, paramName) then
    Result := Copy(paramName, 2, Length(paramName));
end;

procedure TDBParam.SetFromTValue(const fromValue: TValue);
var
  variantValue: Variant;
begin
  variantValue := TUtils.AsVariant(fromValue);
  Value := variantValue;
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

end.
