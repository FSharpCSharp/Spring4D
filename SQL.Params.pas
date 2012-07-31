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
  DB, Generics.Collections, Rtti;

type
  TDBParam = class
  strict private
    FName: string;
    FParamType: TFieldType;
    FValue: Variant;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  public
    property Name: string read GetName write SetName;
    property ParamType: TFieldType read FParamType write FParamType;
    property Value: Variant read FValue write FValue;
  end;

  procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
  procedure ConvertParams(const AFrom: array of const; ATo: TObjectList<TDBParam>);
  function FromTValueTypeToFieldType(const AValue: TValue): TFieldType;

implementation

uses
  SysUtils
  ,Core.Exceptions
  ,Variants
  ;

procedure ConvertParam(const AFrom: TVarRec; out ATo: TDBParam);
begin
  case AFrom.VType of
    vtAnsiString:
    begin
      ATo.ParamType := ftString;
      ATo.Value := string(AFrom.VAnsiString);
    end;
    vtWideString:
    begin
      ATo.ParamType := ftWideString;
      ATo.Value := string(AFrom.VWideString);
    end;
    vtUnicodeString:
    begin
      ATo.ParamType := ftWideString;
      ATo.Value := string(AFrom.VUnicodeString);
    end;
    vtString:
    begin
      ATo.ParamType := ftString;
      ATo.Value := string(AFrom.VString);
    end;
    vtInt64:
    begin
      ATo.ParamType := ftLargeint;
      ATo.Value := Int64(AFrom.VInt64^);
    end;
    vtInteger:
    begin
      ATo.ParamType := ftInteger;
      ATo.Value := AFrom.VInteger;
    end;
    vtExtended:
    begin
      ATo.ParamType := ftExtended;
      ATo.Value := Extended(AFrom.VExtended^);
    end;
    vtCurrency:
    begin
      ATo.ParamType := ftCurrency;
      ATo.Value := Currency(AFrom.VCurrency^);
    end;
    vtBoolean:
    begin
      ATo.ParamType := ftBoolean;
      ATo.Value := AFrom.VBoolean;
    end;
    vtVariant:
    begin
      ATo.ParamType := ftVariant;
      ATo.Value := Variant(AFrom.VVariant^);
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
  LVariant := AValue.AsVariant;
  Result := VarTypeToDataType(VarType(LVariant));
end;

{ TDBParam }

function TDBParam.GetName: string;
begin
  Result := FName;
end;

procedure TDBParam.SetName(const Value: string);
begin
  FName := Value;
end;

end.
