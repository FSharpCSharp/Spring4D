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
unit SvSerializerJson;

interface

uses
  Classes, SvSerializer, SysUtils, DBXJSON, Rtti;

type
  TSvJsonString = class(TJSONString)
  private
    {$IF CompilerVersion < 23}
    function EscapeValue(const AValue: string): string;
    {$IFEND}
  public
    constructor Create(const AValue: string); overload;
  end;

  TSvJsonSerializer = class(TSvAbstractSerializer<TJSONValue>)
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function ToString(): string; override;

    //getters
    function GetAsString(AValue: TJSONValue): string; override;
    function GetAsDouble(AValue: TJSONValue): Double; override;
    function GetAsBoolean(AValue: TJSONValue): Boolean; override;
    function GetValueByName(const AName: string; AObject: TJSONValue): TJSONValue; override;
    function GetArraySize(AValue: TJSONValue): Integer; override;
    function GetArrayElement(AArray: TJSONValue; AIndex: Integer): TJSONValue; override;
    function GetObjectSize(AValue: TJSONValue): Integer; override;

    function EnumerateObject(AObject: TJSONValue): TArray<TEnumEntry<TJSONValue>>; override;
    //setters
    function CreateObject(): TJSONValue; override;
    function CreateArray(): TJSONValue; override;
    function CreateBoolean(AValue: Boolean): TJSONValue; override;
    function CreateString(const AValue: string): TJSONValue; override;
    function CreateNull(): TJSONValue; override;
    function CreateInteger(AValue: Integer): TJSONValue; override;
    function CreateInt64(AValue: Int64): TJSONValue; override;
    function CreateDouble(AValue: Double): TJSONValue; override;

    //
    function IsAssigned(AValue: TJSONValue): Boolean; override;
    function IsNumber(AValue: TJSONValue): Boolean; override;
    function IsString(AValue: TJSONValue): Boolean; override;
    function IsBoolean(AValue: TJSONValue): Boolean; override;
    function IsNull(AValue: TJSONValue): Boolean; override;
    function IsArray(AValue: TJSONValue): Boolean; override;
    function IsObject(AValue: TJSONValue): Boolean; override;

    procedure ArrayAdd(AArray: TJSONValue; const AValue: TJSONValue); override;
    procedure ObjectAdd(AObject: TJSONValue; const AName: string; const AValue: TJSONValue); override;
  public
    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;
    
  end;

implementation

const
  CT_QUALIFIEDNAME = 'QualifiedName';
  CT_DATASET_RECORDS = 'rows';

{ TSvJsonSerializerFactory }

procedure TSvJsonSerializer.ArrayAdd(AArray: TJSONValue; const AValue: TJSONValue);
begin
  (AArray as TJSONArray).AddElement(AValue);
end;

procedure TSvJsonSerializer.BeginDeSerialization(AStream: TStream);
var
  LBytes: TBytesStream;
  LJsonVal: TJSONValue;
begin
  inherited;
  RootObject := nil;

  if Assigned(AStream) then
  begin
    //parse json stream
    LBytes := TBytesStream.Create();
    try
      LBytes.CopyFrom(AStream, AStream.Size);
      LBytes.Position := 0;

      if LBytes.Size > 0 then
      begin
        LJsonVal := TJSONObject.ParseJSONValue(LBytes.Bytes, 0, LBytes.Size, True);

        if Assigned(LJsonVal) and (LJsonVal is TJSONObject) then
        begin
          RootObject := LJsonVal;
        end;
      end;

    finally
      LBytes.Free;
    end;
  end
  else
  begin
    PostError('Cannot deserialize from nil stream');
    raise ESvSerializeException.Create('Cannot deserialize from nil stream');
  end;
end;

procedure TSvJsonSerializer.EndDeSerialization(AStream: TStream);
begin
  inherited;
  if Assigned(RootObject) then
    RootObject.Free;
end;

procedure TSvJsonSerializer.BeginSerialization;
begin
  inherited;
  RootObject := TJSONObject.Create;
end;

constructor TSvJsonSerializer.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  RootObject := nil;
end;

function TSvJsonSerializer.CreateArray: TJSONValue;
begin
  Result := TJSONArray.Create();
end;

function TSvJsonSerializer.CreateBoolean(AValue: Boolean): TJSONValue;
begin
  if AValue then
    Result := TJSONTrue.Create
  else
    Result := TJSONFalse.Create;
end;

function TSvJsonSerializer.CreateDouble(AValue: Double): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue);
end;

function TSvJsonSerializer.CreateInt64(AValue: Int64): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue);
end;

function TSvJsonSerializer.CreateInteger(AValue: Integer): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue);
end;

function TSvJsonSerializer.CreateNull: TJSONValue;
begin
  Result := TJSONNull.Create;
end;

function TSvJsonSerializer.CreateObject: TJSONValue;
begin
  Result := TJSONObject.Create();
end;

function TSvJsonSerializer.CreateString(const AValue: string): TJSONValue;
begin
  Result := TSvJsonString.Create(AValue);
end;

destructor TSvJsonSerializer.Destroy;
begin
  inherited Destroy;
end;

procedure TSvJsonSerializer.EndSerialization;
begin
  inherited;
  if Assigned(RootObject) then
    RootObject.Free;
end;

function TSvJsonSerializer.EnumerateObject(AObject: TJSONValue): TArray<TEnumEntry<TJSONValue>>;
var
  LJsonObject: TJSONObject;
  i: Integer;
begin
  LJsonObject := (AObject as TJSONObject);
  SetLength(Result, LJsonObject.Size);
  for i := Low(Result) to High(Result) do
  begin
    Result[i].Key := LJsonObject.Get(i).JsonString.Value;
    Result[i].Value := LJsonObject.Get(i).JsonValue;
  end;
end;

function TSvJsonSerializer.GetArrayElement(AArray: TJSONValue; AIndex: Integer): TJSONValue;
begin
  Result := (AArray as TJSONArray).Get(AIndex);
end;

function TSvJsonSerializer.GetArraySize(AValue: TJSONValue): Integer;
begin
  Result := (AValue as TJSONArray).Size;
end;

function TSvJsonSerializer.GetAsBoolean(AValue: TJSONValue): Boolean;
begin
  if AValue is TJSONTrue then
    Result := True
  else
    Result := False;
end;

function TSvJsonSerializer.GetAsDouble(AValue: TJSONValue): Double;
begin
  Result := (AValue as TJSONNumber).AsDouble;
end;

function TSvJsonSerializer.GetAsString(AValue: TJSONValue): string;
begin
  Result := AValue.Value;
end;

function TSvJsonSerializer.GetObjectSize(AValue: TJSONValue): Integer;
begin
  Result := (AValue as TJSONObject).Size;
end;

function TSvJsonSerializer.GetValueByName(const AName: string; AObject: TJSONValue): TJSONValue;
begin
  Result := (AObject as TJSONObject).Get(AName).JsonValue;
end;

function TSvJsonSerializer.IsArray(AValue: TJSONValue): Boolean;
begin
  Result := AValue is TJSONArray;
end;

function TSvJsonSerializer.IsAssigned(AValue: TJSONValue): Boolean;
begin
  Result := Assigned(AValue);
end;

function TSvJsonSerializer.IsBoolean(AValue: TJSONValue): Boolean;
begin
  Result := (AValue is TJSONTrue) or (AValue is TJSONFalse);
end;

function TSvJsonSerializer.IsNull(AValue: TJSONValue): Boolean;
begin
  Result := AValue is TJSONNull;
end;

function TSvJsonSerializer.IsNumber(AValue: TJSONValue): Boolean;
begin
  Result := AValue is TJSONNumber;
end;

function TSvJsonSerializer.IsObject(AValue: TJSONValue): Boolean;
begin
  Result := AValue is TJSONObject;
end;

function TSvJsonSerializer.IsString(AValue: TJSONValue): Boolean;
begin
  Result := AValue is TJSONString;
end;

procedure TSvJsonSerializer.ObjectAdd(AObject: TJSONValue; const AName: string;
  const AValue: TJSONValue);
begin
  (AObject as TJSONObject).AddPair(AName, AValue);
end;

function TSvJsonSerializer.ToString: string;
begin
  Result := RootObject.ToString;
end;

{ TSvJsonString }

constructor TSvJsonString.Create(const AValue: string);
begin
  {$IF CompilerVersion >= 23}
  inherited Create(AValue);
  {$ELSE}
  inherited Create(EscapeValue(AValue));
  {$IFEND}
end;

{$IF CompilerVersion < 23}
function TSvJsonString.EscapeValue(const AValue: string): string;

  procedure AddChars(const AChars: string; var Dest: string; var AIndex: Integer); inline;
  begin
    System.Insert(AChars, Dest, AIndex);
    System.Delete(Dest, AIndex + 2, 1);
    Inc(AIndex, 2);
  end;

  procedure AddUnicodeChars(const AChars: string; var Dest: string; var AIndex: Integer); inline;
  begin
    System.Insert(AChars, Dest, AIndex);
    System.Delete(Dest, AIndex + 6, 1);
    Inc(AIndex, 6);
  end;

var
  i, ix: Integer;
  LChar: Char;
begin
  Result := AValue;
  ix := 1;
  for i := 1 to System.Length(AValue) do
  begin
    LChar :=  AValue[i];
    case LChar of
      '/', '\', '"':
      begin
        System.Insert('\', Result, ix);
        Inc(ix, 2);
      end;
      #8:  //backspace \b
      begin
        AddChars('\b', Result, ix);
      end;
      #9:
      begin
        AddChars('\t', Result, ix);
      end;
      #10:
      begin
        AddChars('\n', Result, ix);
      end;
      #12:
      begin
        AddChars('\f', Result, ix);
      end;
      #13:
      begin
        AddChars('\r', Result, ix);
      end;
      #0 .. #7, #11, #14 .. #31:
      begin
        AddUnicodeChars('\u' + IntToHex(Word(LChar), 4), Result, ix);
      end
      else
      begin
        if Word(LChar) > 127 then
        begin
          AddUnicodeChars('\u' + IntToHex(Word(LChar), 4), Result, ix);
        end
        else
        begin
          Inc(ix);
        end;
      end;
    end;
  end;
end;
{$IFEND}

initialization
  TSerializerFactory.RegisterSerializer(sstJson, TSvJsonSerializer);


end.
