(*
* Copyright (c) 2011, Linas Naginionis
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
{*******************************************************}
{                                                       }
{       SvSerializer                                    }
{                                                       }
{       Copyright (C) 2011 "Linas Naginionis"           }
{                                                       }
{*******************************************************}

unit SvSerializer;

interface

uses
  SysUtils, Classes, Rtti, Generics.Collections, Types, TypInfo;

type
  TSvVisibilities = set of TMemberVisibility;

  TSvSerializer = class;

  SvSerialize = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string = ''); overload;
    property Name: string read FName;
  end;

  {$REGION 'Documentation'}
  ///	<remarks>
  ///	  Properties marked as [SvTransient] are ignored during
  ///	  serialization/deserialization
  ///	</remarks>
  {$ENDREGION}
  SvTransientAttribute = class(TCustomAttribute);

  ESvSerializeException = class(Exception);

  TSvSerializeFormat = (sstJson = 0, sstSuperJson, sstNativeXML);

  ISerializer = interface(IInvokable)
    ['{6E0A63A4-0101-4239-A4A9-E74BC4A97C1C}']
    procedure BeginSerialization();
    procedure EndSerialization();
    procedure BeginDeSerialization(AStream: TStream);
    procedure EndDeSerialization(AStream: TStream);

    function ToString(): string;

    procedure SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray);
    procedure DeSerializeObject(const AKey: string; obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray);
    function GetObjectUniqueName(const AKey: string; obj: TObject): string; overload;
    function GetObjectUniqueName(const AKey: string; obj: TValue): string; overload;
    procedure PostError(const ErrorText: string);
    function IsTypeEnumerable(ARttiType: TRttiType; out AEnumMethod: TRttiMethod): Boolean;
    function IsTransient(AProp: TRttiProperty): Boolean;
    function GetRawPointer(const AValue: TValue): Pointer;
    procedure ClearErrors();
  end;

  TEnumEntry<T> = record
    Key: string;
    Value: T;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  <c>TSvSerializer</c> class which can serialize almost any type to a
  ///	  filename, <c>string</c> or a <c>TStream</c>.
  ///	</summary>
  ///	<remarks>
  ///	  In order to use custom serialization formats, they must be registered.
  ///	  This could be done by adding their units to the project uses. Some
  ///	  serialization formats requires additional libraries. They can be
  ///	  located in project's <i>Externals</i> directory. E.g. if your project
  ///	  wants to use <c>sstSuperJson</c> or <c>sstNativeXML</c>, then
  ///	  <c>superobject</c> and <c>NativeXML</c> sources must be added to
  ///	  project's search paths. Also  <c>SvSerializerSuperJson </c>and
  ///	  <c>SvSerializerNativeXML </c>units must be added somewhere in your
  ///	  projects uses.
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	var
  ///	  LFoobar: TFoobar;
  ///	  LJsonString, LXMLString: string;
  ///	begin
  ///	  LFoobar := TFoobar.Create;
  ///	  try
  ///	    LFoobar.Foo := 'Foo';
  ///	    LFoobar.Bar := 'Bar';
  ///	    LFoobar.Skip := True;
  ///	    //serialize to json string
  ///	    TSvSerializer.SerializeObject(LFoobar, LJsonString, sstSuperJson);
  ///	    //serialize to xml string
  ///	    TSvSerializer.SerializeObject(LFoobar, LXMLString, sstNativeXML);
  ///	  finally
  ///	    LFoobar.Free;
  ///	  end; end;</code>
  ///	  <code lang="Delphi">
  ///	var
  ///	  LFoobar: TFoobar;
  ///	  LJsonString: string;
  ///	begin
  ///	  LJsonString := '{"Foo": "Foo", "Bar": "Bar"}';
  ///	  LFoobar := TFoobar.Create;
  ///	  try
  ///	    TSvSerializer.DeserializeObject(LFoobar, LJsonString, sstSuperJson);
  ///	    CheckEqualsString('Foo', LFooBar.Foo);
  ///	  finally
  ///	    LFoobar.Free;
  ///	  end;
  ///	end;</code>
  ///	</example>
  ///	<seealso href="https://code.google.com/p/delphi-oop/wiki/SvSerializer">
  ///	  Wiki
  ///	</seealso>
  {$ENDREGION}
  TSvSerializer = class
  private
    FObjs: TDictionary<string, TPair<TValue,TStringDynArray>>;
    FSerializeFormat: TSvSerializeFormat;
    FErrors: TList<string>;

    procedure SetSerializeFormat(const Value: TSvSerializeFormat);
    function GetObject(const AName: string): TObject;
    function GetCount: Integer;
    function GetErrorCount: Integer;      protected

    procedure DoSerialize(AStream: TStream); virtual;
    procedure DoDeSerialize(AStream: TStream); virtual;
  protected
    procedure AddOneObject(AObject: TObject); virtual;
  public
    constructor Create(AFormat: TSvSerializeFormat = sstJson); virtual;
    destructor Destroy; override;

    function CreateConcreateSerializer(): ISerializer;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds object to be used in serialization. Properties will be
    ///	  serialized with SvSerialize attribute
    ///	</summary>
    ///	<param name="AKey">
    ///	  unique key name which defines where to store object properties
    ///	</param>
    ///	<param name="obj">
    ///	  object to serialize
    ///	</param>
    {$ENDREGION}
    procedure AddObject(const AKey: string; const obj: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds object and it's named properties which will be used in
    ///	  serialization
    ///	</summary>
    ///	<param name="AKey">
    ///	  unique key name which defines where to store object properties
    ///	</param>
    ///	<param name="obj">
    ///	  object to serialize
    ///	</param>
    ///	<param name="APropNames">
    ///	  object properties to serialize
    ///	</param>
    {$ENDREGION}
    procedure AddObjectCustomProperties(const AKey: string; const obj: TObject;
      APropNames: array of string);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds object and all of it's properties in given visibility which will
    ///	  be used in serialization
    ///	</summary>
    ///	<param name="AKey">
    ///	  unique key name which defines where to store object properties
    ///	</param>
    ///	<param name="obj">
    ///	  object to serialize
    ///	</param>
    ///	<param name="AVisibilities">
    ///	  Visibilities of properties to serialize
    ///	</param>
    {$ENDREGION}
    procedure AddObjectProperties(const AKey: string; const obj: TObject;
      AVisibilities: TSvVisibilities = [mvPublished]);
    procedure RemoveObject(const AKey: string); overload;
    procedure RemoveObject(const AObj: TObject); overload;
    procedure ClearObjects;

    property Errors: TList<string> read FErrors;
    property Count: Integer read GetCount;
    property Objects[const AName: string]: TObject read GetObject; default;

    class function GetAttribute(AProp: TRttiProperty): SvSerialize;
    class function TryGetAttribute(AProp: TRttiProperty; out AAtribute: SvSerialize): Boolean;
    
    class function GetPropertyByName(const APropName: string; ARttiType: TRttiType): TRttiProperty;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Deserializes all added objects from the file
    ///	</summary>
    ///	<param name="AFilename">
    ///	  filename from where to load object's properties
    ///	</param>
    {$ENDREGION}
    procedure DeSerialize(const AFromFilename: string); overload; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Deserializes all added objects from the stream
    ///	</summary>
    ///	<param name="AStream">
    ///	  stream from where to load object's properties
    ///	</param>
    {$ENDREGION}
    procedure DeSerialize(AFromStream: TStream); overload; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Deserializes all added objects from the string
    ///	</summary>
    ///	<param name="AStream">
    ///	  stream from where to load object's properties
    ///	</param>
    {$ENDREGION}
    procedure DeSerialize(const AFromString: string; const AEncoding: TEncoding); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Serializes all added objects to the file
    ///	</summary>
    ///	<param name="AFilename">
    ///	  filename where to store objects
    ///	</param>
    {$ENDREGION}
    procedure Serialize(const AToFilename: string); overload; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Serializes all added objects to the stream.
    ///	</summary>
    ///	<param name="AStream">
    ///	  stream where to store objects
    ///	</param>
    {$ENDREGION}
    procedure Serialize(AToStream: TStream); overload; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Serializes all added objects to the string
    ///	</summary>
    ///	<param name="AStream">
    ///	  stream where to store objects
    ///	</param>
    {$ENDREGION}
    procedure Serialize(var AToString: string; const AEncoding: TEncoding); overload; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for serializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure SerializeObject(AObject: TObject; var AToString: string; AOutputFormat: TSvSerializeFormat = sstSuperJson); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for serializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure SerializeObject(AObject: TObject; AToStream: TStream; AOutputFormat: TSvSerializeFormat = sstSuperJson); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for serializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure SerializeObjectToFilename(AObject: TObject; const AFilename: string; AOutputFormat: TSvSerializeFormat = sstSuperJson);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for deserializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure DeSerializeObject(AObject: TObject; const AFromString: string; AOutputFormat: TSvSerializeFormat = sstSuperJson); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for deserializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure DeSerializeObject(AObject: TObject; AFromStream: TStream; AOutputFormat: TSvSerializeFormat = sstSuperJson); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Simplified method for deserializing only given <c>TObject</c>. All
    ///	  other added object will be cleared from the <c>Serializer</c>.
    ///	</summary>
    {$ENDREGION}
    class procedure DeSerializeObjectFromFilename(AObject: TObject; const AFilename: string; AOutputFormat: TSvSerializeFormat = sstSuperJson);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Marshalls record's properties into stream
    ///	</summary>
    {$ENDREGION}
    procedure Marshall<T: record>(const AWhat: T; AToStream: TStream); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Marshalls record's properties into file
    ///	</summary>
    {$ENDREGION}
    procedure Marshall<T: record>(const AWhat: T; var AToString: string; const AEncoding: TEncoding); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Marshalls record's properties into string
    ///	</summary>
    {$ENDREGION}
    procedure Marshall<T: record>(const AWhat: T; const AToFilename: string); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns record unmarshalled from stream
    ///	</summary>
    {$ENDREGION}
    function UnMarshall<T: record>(AFromStream: TStream): T; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns record unmarshalled from file
    ///	</summary>
    {$ENDREGION}
    function UnMarshall<T: record>(const AFromFilename: string): T; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns record unmarshalled from string
    ///	</summary>
    {$ENDREGION}
    function UnMarshall<T: record>(const AFromString: string; AEncoding: TEncoding): T; overload;

    function GetErrors(): TArray<string>;
    function GetErrorsAsString(): string;

    property ErrorCount: Integer read GetErrorCount;
    property SerializeFormat: TSvSerializeFormat read FSerializeFormat write SetSerializeFormat;
  end;

  TSvObjectHelper = class helper for TObject
  public
    function ToSerializedString(ASerializerFormat: TSvSerializeFormat): string;
    function ToJsonString(): string;
    function ToXmlString(): string;
    procedure FromSerializedString(const AValue: string; ASerializerFormat: TSvSerializeFormat);
    constructor FromJsonString(const AJsonString: string);
    constructor FromXmlString(const AXmlString: string);
  end;



implementation

uses
  Variants
 ,DB
 ,SvSerializerFactory
 ,SvSerializerRtti
  ;

{ SvSerialize }

constructor SvSerialize.Create(const  AName: string);
begin
  inherited Create();
  FName := AName;
end;

{ TSvBaseSerializer }

procedure TSvSerializer.AddObject(const AKey: string; const obj: TObject);
begin
  AddObjectCustomProperties(AKey, obj, []);
end;

procedure TSvSerializer.AddObjectCustomProperties(const AKey: string; const obj: TObject;
  APropNames: array of string);
var
  LPair: TPair<TValue,TStringDynArray>;
  LArray: TStringDynArray;
  i: Integer;
begin
  if Assigned(obj) then
  begin
    LPair.Key := obj;
    SetLength(LArray, Length(APropNames));
    for i := Low(APropNames) to High(APropNames) do
    begin
      LArray[i] := APropNames[i];
    end;

    LPair.Value := LArray;
    FObjs.AddOrSetValue(AKey, LPair);
  end;
end;

procedure TSvSerializer.AddObjectProperties(const AKey: string; const obj: TObject; AVisibilities: TSvVisibilities);
var
  LType: TRttiType;
  LCurrProp: TRttiProperty;
  LArray: array of string;
  LStrings: TStringlist;
  i: Integer;
  LValue: TValue;
begin
  if Assigned(obj) then
  begin
    LValue := obj;
    LType := TSvRttiInfo.GetType(LValue);
    LStrings := TStringList.Create;
    try
      for LCurrProp in LType.GetProperties do
      begin
        if LCurrProp.Visibility in AVisibilities then
        begin
          LStrings.Add(LCurrProp.Name);
        end;

      end;

      SetLength(LArray, LStrings.Count);
      for i := 0 to LStrings.Count - 1 do
      begin
        LArray[i] := LStrings[i];
      end;

      AddObjectCustomProperties(AKey, obj, LArray);

    finally
      LStrings.Free;
    end;

  end;
end;

procedure TSvSerializer.AddOneObject(AObject: TObject);
begin
  ClearObjects;
  AddObject('', AObject);
end;

procedure TSvSerializer.ClearObjects;
begin
  FObjs.Clear;
end;

constructor TSvSerializer.Create(AFormat: TSvSerializeFormat);
begin
  inherited Create();
  FSerializeFormat := AFormat;
  FObjs := TDictionary<string, TPair<TValue,TStringDynArray>>.Create();
  FErrors := TList<string>.Create();
end;

function TSvSerializer.CreateConcreateSerializer(): ISerializer;
begin
  Result := TSerializerFactory.GetInstance(Self, FSerializeFormat);
end;

procedure TSvSerializer.DeSerialize(const AFromFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFromFilename, fmOpenRead or fmShareDenyNone);
  try
    DeSerialize(fs);
  finally
    fs.Free;
  end;
end;

procedure TSvSerializer.DeSerialize(AFromStream: TStream);
begin
  {DONE -oLinas -cGeneral : deserialize from stream}
  DoDeSerialize(AFromStream);
end;

procedure TSvSerializer.DeSerialize(const AFromString: string;
  const AEncoding: TEncoding);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AFromString, AEncoding);
  try
    DeSerialize(ss);
  finally
    ss.Free;
  end;
end;

class procedure TSvSerializer.DeSerializeObject(AObject: TObject; const AFromString: string; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.DeSerialize(AFromString, TEncoding.UTF8);
  finally
    LSerializer.Free;
  end;
end;

class procedure TSvSerializer.DeSerializeObject(AObject: TObject; AFromStream: TStream; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.DeSerialize(AFromStream);
  finally
    LSerializer.Free;
  end;
end;

class procedure TSvSerializer.DeSerializeObjectFromFilename(AObject: TObject; const AFilename: string; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.DeSerialize(AFilename);
  finally
    LSerializer.Free;
  end;
end;

destructor TSvSerializer.Destroy;
begin
  FObjs.Free;
  FErrors.Free;
  inherited Destroy;
end;

procedure TSvSerializer.DoDeSerialize(AStream: TStream);
var
  LPair: TPair<string, TPair<TValue,TStringDynArray>>;
  LSerializer: ISerializer;
begin
  inherited;
  LSerializer := CreateConcreateSerializer();
  try
    LSerializer.BeginDeSerialization(AStream);
    for LPair in FObjs do
    begin
      LSerializer.DeSerializeObject(LPair.Key, LPair.Value.Key, AStream, LPair.Value.Value);
    end;
  finally
    LSerializer.EndDeSerialization(AStream);
  end;
end;

procedure TSvSerializer.DoSerialize(AStream: TStream);
var
  LPair: TPair<string, TPair<TValue,TStringDynArray>>;
  LSerializer: ISerializer;
begin
  inherited;
  LSerializer := CreateConcreateSerializer();
  try
    LSerializer.BeginSerialization;
    for LPair in FObjs do
    begin
      LSerializer.SerializeObject(LPair.Key, LPair.Value.Key, AStream, LPair.Value.Value);
    end;

  finally
    LSerializer.EndSerialization;
  end;
end;

class function TSvSerializer.GetAttribute(AProp: TRttiProperty): SvSerialize;
var
  LAttr: TCustomAttribute;
begin
  for LAttr in AProp.GetAttributes do
  begin
    if LAttr is SvSerialize then
    begin
      Exit(SvSerialize(LAttr));
    end;
  end;

  Result := nil;
end;

function TSvSerializer.GetCount: Integer;
begin
  Result := FObjs.Count;
end;

function TSvSerializer.GetErrorCount: Integer;
begin
  Result := FErrors.Count;
end;

function TSvSerializer.GetErrors: TArray<string>;
begin
  Result := FErrors.ToArray;
end;

function TSvSerializer.GetErrorsAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FErrors.Count - 1 do
  begin
    Result := Result + FErrors[I] + #13#10;
  end;
end;

function TSvSerializer.GetObject(const AName: string): TObject;
var
  LPair: TPair<TValue,TStringDynArray>;
begin
  if FObjs.TryGetValue(AName, LPair) then
    Result := LPair.Key.AsObject
  else
    Result := nil;
end;

class function TSvSerializer.GetPropertyByName(const APropName: string; ARttiType: TRttiType): TRttiProperty;
var
  LProp: TRttiProperty;
begin
  for LProp in ARttiType.GetProperties do
  begin
    if SameText(APropName, LProp.Name) then
    begin
      Exit(LProp);
    end;
  end;
  Result := nil;
end;

procedure TSvSerializer.Marshall<T>(const AWhat: T; var AToString: string;
  const AEncoding: TEncoding);
var
  ss: TStringStream;
begin
  AToString := '';
  ss := TStringStream.Create('', AEncoding);
  try
    Marshall(AWhat, ss);

    AToString := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TSvSerializer.Marshall<T>(const AWhat: T; AToStream: TStream);
var
  LValue: TValue;
  LArray: TStringDynArray;
  LSerializer: ISerializer;
begin
  LValue := TValue.From<T>(AWhat);
  LSerializer := CreateConcreateSerializer;
  try
    LSerializer.BeginSerialization;
    LSerializer.SerializeObject('Main', LValue, AToStream, LArray);
  finally
    LSerializer.EndSerialization;
  end;
end;


procedure TSvSerializer.Marshall<T>(const AWhat: T; const AToFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AToFilename, fmCreate);
  try
    Marshall<T>(AWhat, fs);
  finally
    fs.Free;
  end;
end;


procedure TSvSerializer.RemoveObject(const AObj: TObject);
var
  LPair: TPair<string, TPair<TValue,TStringDynArray>>;
  ptrLeft, ptrRight: Pointer;
begin
  Assert(Assigned(AObj), 'Cannot remove nil object');

  for LPair in FObjs do
  begin
    ptrLeft := AObj;
    ptrRight := LPair.Value.Key.AsObject;

    if ptrLeft = ptrRight then
    begin
      RemoveObject(LPair.Key);
      Exit;
    end;
  end;
end;

procedure TSvSerializer.RemoveObject(const AKey: string);
begin
  FObjs.Remove(AKey);
end;

procedure TSvSerializer.Serialize(const AToFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AToFilename, fmCreate);
  try
    Serialize(fs);
  finally
    fs.Free;
  end;
end;

procedure TSvSerializer.Serialize(AToStream: TStream);
begin
  {DONE -oLinas -cGeneral : serialize to stream}
  DoSerialize(AToStream);
end;

procedure TSvSerializer.Serialize(var AToString: string; const AEncoding: TEncoding);
var
  ss: TStringStream;
begin
  AToString := '';
  ss := TStringStream.Create('', AEncoding);
  try
    Serialize(ss);

    AToString := ss.DataString;
  finally
    ss.Free;
  end;
end;

class procedure TSvSerializer.SerializeObject(AObject: TObject; AToStream: TStream; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.Serialize(AToStream);
  finally
    LSerializer.Free;
  end;
end;

class procedure TSvSerializer.SerializeObjectToFilename(AObject: TObject; const AFilename: string; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.Serialize(AFilename);
  finally
    LSerializer.Free;
  end;
end;

class procedure TSvSerializer.SerializeObject(AObject: TObject; var AToString: string; AOutputFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(AOutputFormat);
  try
    LSerializer.AddObject('', AObject);
    LSerializer.Serialize(AToString, TEncoding.UTF8);
  finally
    LSerializer.Free;
  end;
end;

procedure TSvSerializer.SetSerializeFormat(const Value: TSvSerializeFormat);
begin
  if FSerializeFormat <> Value then
  begin
    FSerializeFormat := Value;
  end;
end;



class function TSvSerializer.TryGetAttribute(AProp: TRttiProperty;
  out AAtribute: SvSerialize): Boolean;
begin
  AAtribute := GetAttribute(AProp);
  Result := Assigned(AAtribute);
end;

function TSvSerializer.UnMarshall<T>(const AFromString: string;
  AEncoding: TEncoding): T;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AFromString, AEncoding);
  try
    Result := UnMarshall<T>(ss);
  finally
    ss.Free;
  end;
end;

function TSvSerializer.UnMarshall<T>(AFromStream: TStream): T;
var
  LValue: TValue;
  LArray: TStringDynArray;
  LSerializer: ISerializer;
begin          
  LValue := TValue.From<T>(Result);
  LSerializer := CreateConcreateSerializer;
  try
    LSerializer.BeginDeSerialization(AFromStream);
    LSerializer.DeSerializeObject('Main', LValue, AFromStream, LArray);
  finally
    LSerializer.EndDeSerialization(AFromStream);
  end;
  Result := LValue.AsType<T>;
end;

function TSvSerializer.UnMarshall<T>(const AFromFilename: string): T;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFromFilename, fmOpenRead or fmShareDenyNone);
  try
    Result := UnMarshall<T>(fs);
  finally
    fs.Free;
  end;
end;

{ TSvObjectHelper }

constructor TSvObjectHelper.FromJsonString(const AJsonString: string);
begin
  inherited Create();
  FromSerializedString(AJsonString, sstSuperJson);
end;

procedure TSvObjectHelper.FromSerializedString(const AValue: string; ASerializerFormat: TSvSerializeFormat);
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(ASerializerFormat);
  try
    LSerializer.AddObject('', Self);
    LSerializer.DeSerialize(AValue, TEncoding.UTF8);
  finally
    LSerializer.Free;
  end;
end;

constructor TSvObjectHelper.FromXmlString(const AXmlString: string);
begin
  inherited Create();
  FromSerializedString(AXmlString, sstNativeXML);
end;

function TSvObjectHelper.ToJsonString: string;
begin
  Result := ToSerializedString(sstSuperJson);
end;

function TSvObjectHelper.ToSerializedString(ASerializerFormat: TSvSerializeFormat): string;
var
  LSerializer: TSvSerializer;
begin
  LSerializer := TSvSerializer.Create(ASerializerFormat);
  try
    LSerializer.AddObject('', Self);
    LSerializer.Serialize(Result, TEncoding.UTF8);
  finally
    LSerializer.Free;
  end;
end;

function TSvObjectHelper.ToXmlString: string;
begin
  Result := ToSerializedString(sstNativeXML);
end;




end.

