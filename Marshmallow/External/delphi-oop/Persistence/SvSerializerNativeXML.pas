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
unit SvSerializerNativeXML;

interface

uses
  Classes, SvSerializer, SysUtils, NativeXml, Rtti, SvSerializerAbstract;

type
  TSvXMLNode = class(TsdElement)

  end;

  TSvNativeXMLSerializer = class(TSvAbstractSerializer<TXMLNode>)
  private
    FXML: TNativeXml;
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function ToString(): string; override;

    //getters
    function GetAsString(AValue: TXMLNode): string; override;
    function GetAsDouble(AValue: TXMLNode): Double; override;
    function GetAsBoolean(AValue: TXMLNode): Boolean; override;
    function GetValueByName(const AName: string; AObject: TXMLNode): TXMLNode; override;
    function GetArraySize(AValue: TXMLNode): Integer; override;
    function GetArrayElement(AArray: TXMLNode; AIndex: Integer): TXMLNode; override;
    function GetObjectSize(AValue: TXMLNode): Integer; override;

    function EnumerateObject(AObject: TXMLNode): TArray<TEnumEntry<TXMLNode>>; override;

    //setters
    function CreateRootObject(AType: TRttiType): TXMLNode; override;
    function CreateObject(): TXMLNode; override;
    function CreateArray(): TXMLNode; override;
    function CreateBoolean(AValue: Boolean): TXMLNode; override;
    function CreateString(const AValue: string): TXMLNode; override;
    function CreateNull(): TXMLNode; override;
    function CreateInteger(AValue: Integer): TXMLNode; override;
    function CreateInt64(AValue: Int64): TXMLNode; override;
    function CreateDouble(AValue: Double): TXMLNode; override;

    //
    function IsAssigned(AValue: TXMLNode): Boolean; override;
    function IsNumber(AValue: TXMLNode): Boolean; override;
    function IsString(AValue: TXMLNode): Boolean; override;
    function IsBoolean(AValue: TXMLNode): Boolean; override;
    function IsNull(AValue: TXMLNode): Boolean; override;
    function IsArray(AValue: TXMLNode): Boolean; override;
    function IsObject(AValue: TXMLNode): Boolean; override;

    procedure ArrayAdd(AArray: TXMLNode; const AValue: TXMLNode); override;
    procedure ObjectAdd(AObject: TXMLNode; const AName: string; const AValue: TXMLNode); override;
  public
    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;
  end;


implementation

uses
  SvSerializerFactory
  ;

{ TSvNativeXMLSerializer }

procedure TSvNativeXMLSerializer.ArrayAdd(AArray: TXMLNode; const AValue: TXMLNode);
var
  LIndex: Integer;
begin
  LIndex := AArray.NodeAdd(AValue);
  AValue.NameUnicode := IntToStr(LIndex);
end;

procedure TSvNativeXMLSerializer.BeginDeSerialization(AStream: TStream);
begin
  inherited;
  FXML.LoadFromStream(AStream);
  if not FXML.IsEmpty then
    RootObject := FXML.Root;
end;

procedure TSvNativeXMLSerializer.BeginSerialization;
begin
  inherited;
  RootObject := nil;
end;

constructor TSvNativeXMLSerializer.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  RootObject := nil;
  FXML := TNativeXML.CreateEx(nil, True, False, False, 'root');
  FXML.XmlFormat := xfReadable;
end;

function TSvNativeXMLSerializer.CreateArray: TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.AttributeAdd('type', 'array');
end;

function TSvNativeXMLSerializer.CreateBoolean(AValue: Boolean): TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.ValueAsBool := AValue;
  Result.AttributeAdd('type', 'boolean');
end;

function TSvNativeXMLSerializer.CreateDouble(AValue: Double): TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.ValueUnicode := FloatToStr(AValue, FFormatSettings);
  Result.AttributeAdd('type', 'number');
end;

function TSvNativeXMLSerializer.CreateInt64(AValue: Int64): TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.ValueAsInt64 := AValue;
  Result.AttributeAdd('type', 'number');
end;

function TSvNativeXMLSerializer.CreateInteger(AValue: Integer): TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.ValueAsInteger := AValue;
  Result.AttributeAdd('type', 'number');
end;

function TSvNativeXMLSerializer.CreateNull: TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.Value := '';
  Result.Clear;
end;

function TSvNativeXMLSerializer.CreateObject: TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
end;

function TSvNativeXMLSerializer.CreateRootObject(AType: TRttiType): TXMLNode;
var
  LEnumMethod: TRttiMethod;
begin
  Result := nil;
  if not IsTypeEnumerable(AType, LEnumMethod) then
  begin
    Result := CreateObject;
  end;
end;

function TSvNativeXMLSerializer.CreateString(const AValue: string): TXMLNode;
begin
  Result := TSvXMLNode.Create(FXML);
  Result.ValueUnicode := AValue;
  Result.AttributeAdd('type', 'string');
end;

destructor TSvNativeXMLSerializer.Destroy;
begin
  FXML.Free;
  if Assigned(RootObject) and (RootObject is TSvXMLNode) then
    RootObject.Free;
  inherited Destroy;
end;

procedure TSvNativeXMLSerializer.EndDeSerialization(AStream: TStream);
begin
  inherited;
end;

procedure TSvNativeXMLSerializer.EndSerialization;
begin
  inherited;
end;

function TSvNativeXMLSerializer.EnumerateObject(AObject: TXMLNode): TArray<TEnumEntry<TXMLNode>>;
var
  i: Integer;
begin
  SetLength(Result, AObject.ContainerCount);
  for i := Low(Result) to High(Result) do
  begin
    Result[i].Key := AObject.Containers[i].NameUnicode;
    Result[i].Value := AObject.Containers[i];
  end;
end;

function TSvNativeXMLSerializer.GetArrayElement(AArray: TXMLNode; AIndex: Integer): TXMLNode;
begin
  Result := AArray.Containers[AIndex];
end;

function TSvNativeXMLSerializer.GetArraySize(AValue: TXMLNode): Integer;
begin
  Result := AValue.ContainerCount;
end;

function TSvNativeXMLSerializer.GetAsBoolean(AValue: TXMLNode): Boolean;
begin
  Result := AValue.ValueAsBool;
end;

function TSvNativeXMLSerializer.GetAsDouble(AValue: TXMLNode): Double;
begin
  Result := StrToFloatDef(AValue.ValueUnicode, 0, FFormatSettings);
end;

function TSvNativeXMLSerializer.GetAsString(AValue: TXMLNode): string;
begin
  Result := AValue.ValueUnicode;
end;

function TSvNativeXMLSerializer.GetObjectSize(AValue: TXMLNode): Integer;
begin
  Result := AValue.ContainerCount;
end;

function TSvNativeXMLSerializer.GetValueByName(const AName: string; AObject: TXMLNode): TXMLNode;
begin
  Result := AObject.NodeByName(UTF8String(AName));
end;

function TSvNativeXMLSerializer.IsArray(AValue: TXMLNode): Boolean;
begin
  Result := AValue.AttributeValueByNameWide['type'] = 'array';
end;

function TSvNativeXMLSerializer.IsAssigned(AValue: TXMLNode): Boolean;
begin
  Result := Assigned(AValue);
end;

function TSvNativeXMLSerializer.IsBoolean(AValue: TXMLNode): Boolean;
begin
  Result := AValue.AttributeValueByNameWide['type'] = 'boolean';
end;

function TSvNativeXMLSerializer.IsNull(AValue: TXMLNode): Boolean;
begin
  Result := AValue.IsEmpty;
end;

function TSvNativeXMLSerializer.IsNumber(AValue: TXMLNode): Boolean;
begin
  Result := AValue.AttributeValueByNameWide['type'] = 'number';
end;

function TSvNativeXMLSerializer.IsObject(AValue: TXMLNode): Boolean;
begin
  Result := AValue.ContainerCount > 0;
end;

function TSvNativeXMLSerializer.IsString(AValue: TXMLNode): Boolean;
begin
  Result := AValue.AttributeValueByNameWide['type'] = 'string';
end;

procedure TSvNativeXMLSerializer.ObjectAdd(AObject: TXMLNode; const AName: string; const AValue: TXMLNode);
begin
  AValue.NameUnicode := AName;
  AObject.NodeAdd(AValue);
end;

function TSvNativeXMLSerializer.ToString: string;
begin
  Result := string(RootObject.WriteToString);
end;

initialization
  TSerializerFactory.RegisterSerializer(sstNativeXML, TSvNativeXMLSerializer);

end.
