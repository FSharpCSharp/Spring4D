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
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents query parameter.
  ///	</summary>
  {$ENDREGION}
  TDBParam = class
  private
    FName: string;
    FParamType: TFieldType;
    FValue: Variant;
    FTypeInfo: PTypeInfo;
  protected
    function GetName: string; virtual;
    procedure SetName(const Value: string); virtual;
    procedure SetValue(const Value: Variant); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AName: string; const AValue: Variant); overload; virtual;
    destructor Destroy; override;

    procedure SetFromTValue(const AValue: TValue);
    procedure SetParamTypeFromTypeInfo(ATypeInfo: PTypeInfo);


    property TypeInfo: PTypeInfo read FTypeInfo;
    property Name: string read GetName write SetName;
    property ParamType: TFieldType read FParamType write FParamType;
    property Value: Variant read FValue write SetValue;
  end;

  procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
  procedure ConvertParams(const AFrom: array of const; ATo: IList<TDBParam>);
  function FromTValueTypeToFieldType(const AValue: TValue): TFieldType;
  function FromTypeInfoToFieldType(ATypeInfo: PTypeInfo): TFieldType;

implementation

uses
  SysUtils,
  Variants,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils;

procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
begin
  case AFrom.VType of
    vtAnsiString:
    begin
      ATo.ParamType := ftString;
      ATo.FValue := string(AFrom.VAnsiString);
    end;
    vtWideString:
    begin
      ATo.ParamType := ftWideString;
      ATo.FValue := string(AFrom.VWideString);
    end;
    vtUnicodeString:
    begin
      ATo.ParamType := ftWideString;
      ATo.FValue := string(AFrom.VUnicodeString);
    end;
    vtString:
    begin
      ATo.ParamType := ftString;
      ATo.FValue := string(AFrom.VString);
    end;
    vtInt64:
    begin
      ATo.ParamType := ftLargeint;
      ATo.FValue := Int64(AFrom.VInt64^);
    end;
    vtInteger:
    begin
      ATo.ParamType := ftInteger;
      ATo.FValue := AFrom.VInteger;
    end;
    vtExtended:
    begin
      ATo.ParamType := DB.ftExtended;
      ATo.FValue := Extended(AFrom.VExtended^);
    end;
    vtCurrency:
    begin
      ATo.ParamType := ftCurrency;
      ATo.FValue := Currency(AFrom.VCurrency^);
    end;
    vtBoolean:
    begin
      ATo.ParamType := ftBoolean;
      ATo.FValue := AFrom.VBoolean;
    end;
    vtVariant:
    begin
      ATo.ParamType := ftVariant;
      ATo.FValue := Variant(AFrom.VVariant^);
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

function FromTypeInfoToFieldType(ATypeInfo: PTypeInfo): TFieldType;
begin
  case ATypeInfo.Kind of
    tkUnknown: Result := ftUnknown;
    tkInteger: Result := ftInteger;
    tkChar, tkLString, tkString: Result := ftString;
    tkEnumeration, tkSet: Result := ftInteger;
    tkFloat: Result := ftFloat;
    tkClass: Result := ftBlob;
    tkWChar, tkWString, tkUString: Result := ftWideString;
    tkVariant: Result := ftVariant;
    tkArray, tkRecord, tkInterface, tkDynArray: Result := ftBlob;
    tkInt64: Result := ftLargeint;
    tkClassRef: Result := ftReference;
    tkPointer: Result := ftReference;
    else
      Result := ftUnknown;
  end;
end;

{ TDBParam }

constructor TDBParam.Create(const AName: string; const AValue: Variant);
begin
  Create;
  FName := AName;
  SetValue(AValue);
end;

constructor TDBParam.Create;
begin
  inherited Create;
end;

destructor TDBParam.Destroy;
begin
  //
  inherited Destroy;
end;

function TDBParam.GetName: string;
begin
  Result := UpperCase(FName);
  if (Length(Result) > 0) and not (CharInSet(Result[1], [':'])) then
  begin
    Result := ':' + Result;
  end;
end;

procedure TDBParam.SetFromTValue(const AValue: TValue);
var
  LVariant: Variant;
begin
  LVariant := TUtils.AsVariant(AValue);
  Value := LVariant;
end;

procedure TDBParam.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDBParam.SetParamTypeFromTypeInfo(ATypeInfo: PTypeInfo);
begin
  FTypeInfo := ATypeInfo;
  FParamType := FromTypeInfoToFieldType(FTypeInfo);
end;

procedure TDBParam.SetValue(const Value: Variant);
begin
  FParamType := VarTypeToDataType(VarType(Value));
  FValue := Value;
end;

end.
