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
unit SvSerializerSuperJson;

interface

uses
  Classes, SvSerializer, SysUtils, superobject, Rtti, SvSerializerAbstract;

type
  TSvSuperJsonSerializer = class(TSvAbstractSerializer<ISuperObject>)
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function ToString(): string; override;
    function SOString(const AValue: string): ISuperObject;

    //getters
    function GetAsString(AValue: ISuperObject): string; override;
    function GetAsDouble(AValue: ISuperObject): Double; override;
    function GetAsBoolean(AValue: ISuperObject): Boolean; override;
    function GetValueByName(const AName: string; AObject: ISuperObject): ISuperObject; override;
    function GetArraySize(AValue: ISuperObject): Integer; override;
    function GetArrayElement(AArray: ISuperObject; AIndex: Integer): ISuperObject; override;
    function GetObjectSize(AValue: ISuperObject): Integer; override;

    function EnumerateObject(AObject: ISuperObject): TArray<TEnumEntry<ISuperObject>>; override;
    //setters
    function CreateObject(): ISuperObject; override;
    function CreateArray(): ISuperObject; override;
    function CreateBoolean(AValue: Boolean): ISuperObject; override;
    function CreateString(const AValue: string): ISuperObject; override;
    function CreateNull(): ISuperObject; override;
    function CreateInteger(AValue: Integer): ISuperObject; override;
    function CreateInt64(AValue: Int64): ISuperObject; override;
    function CreateDouble(AValue: Double): ISuperObject; override;

    //
    function IsAssigned(AValue: ISuperObject): Boolean; override;
    function IsNumber(AValue: ISuperObject): Boolean; override;
    function IsString(AValue: ISuperObject): Boolean; override;
    function IsBoolean(AValue: ISuperObject): Boolean; override;
    function IsNull(AValue: ISuperObject): Boolean; override;
    function IsArray(AValue: ISuperObject): Boolean; override;
    function IsObject(AValue: ISuperObject): Boolean; override;

    procedure ArrayAdd(AArray: ISuperObject; const AValue: ISuperObject); override;
    procedure ObjectAdd(AObject: ISuperObject; const AName: string; const AValue: ISuperObject); override;

    function GetValueAsVariant(AObject: ISuperObject): Variant;
  public
    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;
  end;

implementation

uses
  Variants
  ,SvSerializerFactory
  ;

{ TSvSuperJsonSerializer }

procedure TSvSuperJsonSerializer.ArrayAdd(AArray: ISuperObject; const AValue: ISuperObject);
begin
  AArray.AsArray.Add(AValue);
end;

procedure TSvSuperJsonSerializer.BeginDeSerialization(AStream: TStream);
begin
  inherited;
  if AStream is TStringStream then
    RootObject := TSuperObject.ParseString(PWideChar(TStringStream(AStream).DataString), False)
  else
    RootObject := TSuperObject.ParseStream(AStream, False);
end;

procedure TSvSuperJsonSerializer.BeginSerialization;
begin
  inherited;
  RootObject := nil;
  //RootObject := SO();
end;

constructor TSvSuperJsonSerializer.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  RootObject := nil;
end;

function TSvSuperJsonSerializer.CreateArray: ISuperObject;
begin
  Result := TSuperObject.Create(stArray);
end;

function TSvSuperJsonSerializer.CreateBoolean(AValue: Boolean): ISuperObject;
begin
  Result := SO(AValue);
end;

function TSvSuperJsonSerializer.CreateDouble(AValue: Double): ISuperObject;
begin
  Result := SO(AValue);
end;

function TSvSuperJsonSerializer.CreateInt64(AValue: Int64): ISuperObject;
begin
  Result := SO(AValue);
end;

function TSvSuperJsonSerializer.CreateInteger(AValue: Integer): ISuperObject;
begin
  Result := SO(AValue);
end;

function TSvSuperJsonSerializer.CreateNull: ISuperObject;
begin
  Result := TSuperObject.Create(stNull);
end;

function TSvSuperJsonSerializer.CreateObject: ISuperObject;
begin
  Result := SO();
end;

function TSvSuperJsonSerializer.CreateString(const AValue: string): ISuperObject;
begin
  Result := SOString(AValue);
end;

destructor TSvSuperJsonSerializer.Destroy;
begin
  RootObject := nil;
  inherited;
end;

procedure TSvSuperJsonSerializer.EndDeSerialization(AStream: TStream);
begin
  inherited;
end;

procedure TSvSuperJsonSerializer.EndSerialization;
begin
  inherited;
  RootObject := nil;
end;

function TSvSuperJsonSerializer.EnumerateObject(AObject: ISuperObject): TArray<TEnumEntry<ISuperObject>>;
var
  i: Integer;
  LItem: TSuperAvlEntry;
  LObject: TSuperTableString;
begin
  LObject := AObject.AsObject;
  SetLength(Result, LObject.count);
  i := 0;
  for LItem in LObject do
  begin
    Result[i].Key := LItem.Name;
    Result[i].Value := LItem.Value;
    Inc(i);
  end;
end;

function TSvSuperJsonSerializer.GetArrayElement(AArray: ISuperObject;
  AIndex: Integer): ISuperObject;
begin
  Result := AArray.AsArray[AIndex];
end;

function TSvSuperJsonSerializer.GetArraySize(AValue: ISuperObject): Integer;
begin
  Result := AValue.AsArray.Length;
end;

function TSvSuperJsonSerializer.GetAsBoolean(AValue: ISuperObject): Boolean;
begin
  Result := AValue.AsBoolean;
end;

function TSvSuperJsonSerializer.GetAsDouble(AValue: ISuperObject): Double;
begin
  Result := AValue.AsDouble;
end;

function TSvSuperJsonSerializer.GetAsString(AValue: ISuperObject): string;
begin
  Result := AValue.AsString;
end;

function TSvSuperJsonSerializer.GetObjectSize(AValue: ISuperObject): Integer;
begin
  Result := AValue.AsObject.count;
end;

function TSvSuperJsonSerializer.GetValueAsVariant(AObject: ISuperObject): Variant;
var
  LInt64: Int64;
  LInt: Integer;
begin
  Result := Null;
  if Assigned(AObject) then
  begin
    case AObject.DataType of
      stBoolean: Result := AObject.AsBoolean;
      stDouble: Result := AObject.AsDouble ;
      stCurrency: Result := AObject.AsCurrency ;
      stInt:
      begin
        LInt64 := AObject.AsInteger;
        if TryStrToInt(IntToStr(LInt64), LInt) then
          Result := LInt
        else
          Result := LInt64;
      end;

      stObject, stArray, stString, stMethod: Result := AObject.AsString;
    end;
  end;
end;

function TSvSuperJsonSerializer.GetValueByName(const AName: string;
  AObject: ISuperObject): ISuperObject;
var
  LObj: TSuperTableString;
  LEntry: TSuperAvlEntry;
begin
  Result := nil;
  LObj := AObject.AsObject;

  if not Assigned(LObj) then
    Exit;

  for LEntry in LObj do
  begin
    if SameText(LEntry.Name, AName) then
    begin
      Exit(LEntry.Value);
    end;
  end;
end;

function TSvSuperJsonSerializer.IsArray(AValue: ISuperObject): Boolean;
begin
  Result := AValue.IsType(stArray);
end;

function TSvSuperJsonSerializer.IsAssigned(AValue: ISuperObject): Boolean;
begin
  Result := Assigned(AValue);
end;

function TSvSuperJsonSerializer.IsBoolean(AValue: ISuperObject): Boolean;
begin
  Result := AValue.IsType(stBoolean);
end;

function TSvSuperJsonSerializer.IsNull(AValue: ISuperObject): Boolean;
begin
  Result := AValue.IsType(stNull);
end;

function TSvSuperJsonSerializer.IsNumber(AValue: ISuperObject): Boolean;
begin
  Result := (AValue.IsType(stInt)) or (AValue.IsType(stDouble)) or (AValue.IsType(stCurrency));
end;

function TSvSuperJsonSerializer.IsObject(AValue: ISuperObject): Boolean;
begin
  Result := AValue.IsType(stObject);
end;

function TSvSuperJsonSerializer.IsString(AValue: ISuperObject): Boolean;
begin
  Result := AValue.IsType(stString);
end;

procedure TSvSuperJsonSerializer.ObjectAdd(AObject: ISuperObject; const AName: string;
  const AValue: ISuperObject);
begin
  AObject.AsObject.O[AName] := AValue;
end;

function TSvSuperJsonSerializer.SOString(const AValue: string): ISuperObject;
var
  LValue: Variant;
begin
  LValue := AValue;
  Result := SO(LValue);
end;

function TSvSuperJsonSerializer.ToString: string;
begin
  Result := RootObject.AsJSon(True, True);
end;

initialization
  TSerializerFactory.RegisterSerializer(sstSuperJson, TSvSuperJsonSerializer);

end.
