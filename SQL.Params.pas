(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.Params;

interface

uses
  DB, TypInfo, Generics.Collections, Rtti;

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
  private
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
  public
    procedure SetFromTValue(const AValue: TValue);
    procedure SetParamTypeFromTypeInfo(ATypeInfo: PTypeInfo);

    property TypeInfo: PTypeInfo read FTypeInfo;
    property Name: string read GetName write SetName;
    property ParamType: TFieldType read FParamType write FParamType;
    property Value: Variant read FValue write SetValue;
  end;

  procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
  procedure ConvertParams(const AFrom: array of const; ATo: TObjectList<TDBParam>);
  function FromTValueTypeToFieldType(const AValue: TValue): TFieldType;
  function FromTypeInfoToFieldType(ATypeInfo: PTypeInfo): TFieldType;

implementation

uses
  SysUtils
  ,Core.Exceptions
  ,Core.Utils
  ,Variants
  ;

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

procedure ConvertParams(const AFrom: array of const; ATo: TObjectList<TDBParam>);
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
