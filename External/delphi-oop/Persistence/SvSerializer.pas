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

  /// <remarks>
  /// Properties marked as [SvTransient] are ignored during serialization/deserialization
  /// </remarks>
  SvTransientAttribute = class(TCustomAttribute);

  ESvSerializeException = class(Exception);

  TSvSerializeFormat = (sstJson = 0, sstSuperJson, sstNativeXML);

  TConstructorMethod = reference to function(): TObject;

  TSvRttiInfo = class
  strict private
    class var
      FCtx: TRttiContext;
    class var
      FRegisteredConstructors: TDictionary<PTypeInfo,TConstructorMethod>;
    class constructor Create;
    class destructor Destroy;
  public
    class property Context: TRttiContext read FCtx;
    class function FindType(const AQualifiedName: string): TRttiType;
    class function GetType(ATypeInfo: Pointer): TRttiType; overload;
    class function GetType(AClass: TClass): TRttiType; overload;
    class function GetType(const Value: TValue): TRttiType; overload;
    class function GetTypes: TArray<TRttiType>;
    class function GetPackages: TArray<TRttiPackage>;
    class function GetBasicMethod(const AMethodName: string; AType: TRttiType): TRttiMethod;
    class procedure SetValue(AProp: TRttiProperty; const AInstance, AValue: TValue); overload;
    class procedure SetValue(AField: TRttiField; const AInstance, AValue: TValue); overload;
    class function GetValue(AProp: TRttiProperty; const AInstance: TValue): TValue;

    class function CreateType(ATypeInfo: PTypeInfo): TObject; overload;

    class procedure RegisterConstructor(ATypeInfo: PTypeInfo; AMethod: TConstructorMethod);
  end;

  ISerializer = interface
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

  TSvAbstractNonGenericSerializer = class(TInterfacedObject, ISerializer)
  protected
    procedure BeginSerialization(); virtual; abstract;
    procedure EndSerialization(); virtual; abstract;
    procedure BeginDeSerialization(AStream: TStream); virtual; abstract;
    procedure EndDeSerialization(AStream: TStream);virtual; abstract;

    function ToString(): string; reintroduce; virtual; abstract;

    procedure SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); virtual; abstract;
    procedure DeSerializeObject(const AKey: string; obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); virtual; abstract;
    function GetObjectUniqueName(const AKey: string; obj: TObject): string; overload; virtual; abstract;
    function GetObjectUniqueName(const AKey: string; obj: TValue): string; overload; virtual; abstract;
    procedure PostError(const ErrorText: string); virtual; abstract;
    function IsTypeEnumerable(ARttiType: TRttiType; out AEnumMethod: TRttiMethod): Boolean; virtual; abstract;
    function IsTransient(AProp: TRttiProperty): Boolean; virtual; abstract;
    function GetRawPointer(const AValue: TValue): Pointer; virtual; abstract;
    procedure ClearErrors(); virtual; abstract;
  public
    constructor Create(AOwner: TSvSerializer); virtual;
  end;

  TSvAbstractNonGenericSerializerClass = class of TSvAbstractNonGenericSerializer;

  TSvAbstractSerializer<T> = class(TSvAbstractNonGenericSerializer)
  private
    FOwner: TSvSerializer;
    FErrors: TList<string>;
    FStringStream: TStringStream;
    FStream: TStream;
    FOldNullStrConvert: Boolean;
    FRootObj: T;
    function GetRootObj: T;
    procedure SetRootObj(const Value: T);
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function FindRecordFieldName(const AFieldName: string; ARecord: TRttiRecordType): TRttiField; virtual;

    procedure SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); override;
    procedure DeSerializeObject(const AKey: string; obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); override;
    function GetObjectUniqueName(const AKey: string; obj: TObject): string; overload; override;
    function GetObjectUniqueName(const AKey: string; obj: TValue): string; overload; override;
    procedure PostError(const ErrorText: string); override;
    function IsTypeEnumerable(ARttiType: TRttiType; out AEnumMethod: TRttiMethod): Boolean; override;
    function IsTransient(AProp: TRttiProperty): Boolean; override;
    function GetRawPointer(const AValue: TValue): Pointer; override;
    
    function DoSetFromNumber(AJsonNumber: T): TValue; virtual;
    function DoSetFromString(AJsonString: T; AType: TRttiType; var ASkip: Boolean): TValue; virtual;
    function DoSetFromArray(AJsonArray: T; AType: TRttiType; const AObj: TValue; AProp: TRttiProperty; var ASkip: Boolean): TValue; virtual;
    function DoSetFromObject(AJsonObject: T; AType: TRttiType; const AObj: TValue; AProp: TRttiProperty; var ASkip: Boolean): TValue; virtual;
    //
    function DoGetFromArray(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function DoGetFromClass(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function DoGetFromEnum(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function DoGetFromInterface(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function DoGetFromRecord(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function DoGetFromVariant(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    //
    function GetValue(const AFrom: TValue; AProp: TRttiProperty): T; virtual;
    function SetValue(const AFrom: T; const AObj: TValue; AProp: TRttiProperty; AType: TRttiType; var Skip: Boolean): TValue; virtual;
    //needed methods for ancestors to implement
    //getters
    function GetAsString(AValue: T): string; virtual; abstract;
    function GetAsDouble(AValue: T): Double; virtual; abstract;
    function GetAsBoolean(AValue: T): Boolean; virtual; abstract;
    function GetArraySize(AValue: T): Integer; virtual; abstract;
    function GetArrayElement(AArray: T; AIndex: Integer): T; virtual; abstract;
    function GetObjectSize(AValue: T): Integer; virtual; abstract;
    
    function GetValueByName(const AName: string; AObject: T): T; virtual; abstract;
    function EnumerateObject(AObject: T): TArray<TEnumEntry<T>>; virtual; abstract;
    //setters
    function CreateObject(): T; virtual; abstract;
    function CreateArray(): T; virtual; abstract;
    function CreateBoolean(AValue: Boolean): T; virtual; abstract;
    function CreateString(const AValue: string): T; virtual; abstract;
    function CreateNull(): T; virtual; abstract;
    function CreateInteger(AValue: Integer): T; virtual; abstract;
    function CreateInt64(AValue: Int64): T; virtual; abstract;
    function CreateDouble(AValue: Double): T; virtual; abstract;

    //
    function IsAssigned(AValue: T): Boolean; virtual; abstract;
    function IsNumber(AValue: T): Boolean; virtual; abstract;
    function IsString(AValue: T): Boolean; virtual; abstract;
    function IsBoolean(AValue: T): Boolean; virtual; abstract;
    function IsNull(AValue: T): Boolean; virtual; abstract;
    function IsArray(AValue: T): Boolean; virtual; abstract;
    function IsObject(AValue: T): Boolean; virtual; abstract;

    procedure ArrayAdd(AArray: T; const AValue: T); virtual; abstract;
    procedure ObjectAdd(AObject: T; const AName: string; const AValue: T); virtual; abstract;


    property RootObject: T read GetRootObj write SetRootObj;
  public
    FFormatSettings, FOldFormatSettings: TFormatSettings;

    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;

    procedure ClearErrors(); override;

    property Errors: TList<string> read FErrors;
    property Owner: TSvSerializer read FOwner;
    property Stream: TStream read FStream write FStream;
    property StringStream: TStringStream read FStringStream;
  end;

  /// <summary>
  /// Serializer class which can serialize almost any type to a file or a stream
  /// </summary>
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
    property Errors: TList<string> read FErrors;
  public
    constructor Create(AFormat: TSvSerializeFormat = sstJson); virtual;
    destructor Destroy; override;

    function CreateConcreateSerializer(): ISerializer;
    /// <summary>
    /// Adds object to be used in serialization. Properties will be serialized with SvSerialize attribute
    /// </summary>
    /// <param name="AKey">unique key name which defines where to store object properties</param>
    /// <param name="obj">object to serialize</param>
    procedure AddObject(const AKey: string; const obj: TObject);
    /// <summary>
    /// Adds object and it's named properties which will be used in serialization
    /// </summary>
    /// <param name="AKey">unique key name which defines where to store object properties</param>
    /// <param name="obj">object to serialize</param>
    /// <param name="APropNames">object properties to serialize</param>
    procedure AddObjectCustomProperties(const AKey: string; const obj: TObject;
      APropNames: array of string);
    /// <summary>
    /// Adds object and all of it's properties in given visibility which will be used in serialization
    /// </summary>
    /// <param name="AKey">unique key name which defines where to store object properties</param>
    /// <param name="obj">object to serialize</param>
    /// <param name="AVisibilities">Visibilities of properties to serialize</param>
    procedure AddObjectProperties(const AKey: string; const obj: TObject;
      AVisibilities: TSvVisibilities = [mvPublished]);
    procedure RemoveObject(const AKey: string); overload;
    procedure RemoveObject(const AObj: TObject); overload;
    procedure ClearObjects;

    property Count: Integer read GetCount;
    property Objects[const AName: string]: TObject read GetObject; default;

    class function GetAttribute(AProp: TRttiProperty): SvSerialize;
    class function TryGetAttribute(AProp: TRttiProperty; out AAtribute: SvSerialize): Boolean;
    
    class function GetPropertyByName(const APropName: string; ARttiType: TRttiType): TRttiProperty;
    /// <summary>
    /// Deserializes all added objects from the file
    /// </summary>
    /// <param name="AFilename">filename from where to load object's properties</param>
    procedure DeSerialize(const AFromFilename: string); overload; virtual;
    /// <summary>
    /// Deserializes all added objects from the stream
    /// </summary>
    /// <param name="AStream">stream from where to load object's properties</param>
    procedure DeSerialize(AFromStream: TStream); overload; virtual;
    /// <summary>
    /// Deserializes all added objects from the string
    /// </summary>
    /// <param name="AStream">stream from where to load object's properties</param>
    procedure DeSerialize(const AFromString: string; const AEncoding: TEncoding); overload;
    /// <summary>
    /// Serializes all added objects to the file
    /// </summary>
    /// <param name="AFilename">filename where to store objects</param>
    procedure Serialize(const AToFilename: string); overload; virtual;
    /// <summary>
    /// Serializes all added objects to the stream
    /// </summary>
    /// <param name="AStream">stream where to store objects</param>
    procedure Serialize(AToStream: TStream); overload; virtual;
    /// <summary>
    /// Serializes all added objects to the string
    /// </summary>
    /// <param name="AStream">stream where to store objects</param>
    procedure Serialize(var AToString: string; const AEncoding: TEncoding); overload; virtual;
    /// <summary>
    ///  Marshalls record's properties into stream
    /// </summary>
    procedure Marshall<T: record>(const AWhat: T; AToStream: TStream); overload;
    /// <summary>
    ///  Marshalls record's properties into file
    /// </summary>
    procedure Marshall<T: record>(const AWhat: T; var AToString: string; const AEncoding: TEncoding); overload;
    /// <summary>
    ///  Marshalls record's properties into string
    /// </summary>
    procedure Marshall<T: record>(const AWhat: T; const AToFilename: string); overload;
    /// <summary>
    ///  Returns record unmarshalled from stream
    /// </summary>
    function UnMarshall<T: record>(AFromStream: TStream): T; overload;
    /// <summary>
    ///  Returns record unmarshalled from file
    /// </summary>
    function UnMarshall<T: record>(const AFromFilename: string): T; overload;
    /// <summary>
    ///  Returns record unmarshalled from string
    /// </summary>
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

  TSerializerFactory = class sealed
  private
    class var
      FRegisteredSerializers: TDictionary<TSvSerializeFormat,TSvAbstractNonGenericSerializerClass>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class function GetInstance(AOwner: TSvSerializer; ASerializerFormat: TSvSerializeFormat): ISerializer;
    class procedure RegisterSerializer(ASerializerFormat: TSvSerializeFormat; AClass: TSvAbstractNonGenericSerializerClass);
  end;

  ESerializerFactoryException = class(Exception);

implementation

uses
  Variants
 ,DB
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

{ TSvSerializerFactory }

function TSvAbstractSerializer<T>.GetObjectUniqueName(const AKey: string; obj: TObject): string;
begin
  if Assigned(obj) then
  begin
    Result := Format('%S.%S',[obj.ClassName, AKey]);
  end
  else
  begin
    raise ESvSerializeException.Create('Cannot get object unique name. Object cannot be nil');
  end;
end;

procedure TSvAbstractSerializer<T>.BeginDeSerialization(AStream: TStream);
begin
  ClearErrors;
  FOldNullStrConvert := NullStrictConvert;
  NullStrictConvert := False;
end;

procedure TSvAbstractSerializer<T>.BeginSerialization;
begin
  ClearErrors;
  FStringStream := TStringStream.Create('', TEncoding.UTF8);
  FOldNullStrConvert := NullStrictConvert;
  NullStrictConvert := False;
end;

procedure TSvAbstractSerializer<T>.ClearErrors;
begin
  FErrors.Clear;
  FOwner.FErrors.Clear;
end;

constructor TSvAbstractSerializer<T>.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FErrors := TList<string>.Create;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FFormatSettings.DateSeparator := '-';
  FOldFormatSettings := FormatSettings;
end;

procedure TSvAbstractSerializer<T>.DeSerializeObject(const AKey: string; obj: TValue;
  AStream: TStream; ACustomProps: TStringDynArray);
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
  LObject: T;
  LPair: T;
  LPropName: string;
  I: Integer;
  LSkip: Boolean;
  LField: TRttiField;
  LAttrib: SvSerialize;
begin
  inherited;
  LObject := System.Default(T);
  if not obj.IsEmpty and IsAssigned(RootObject) then
  begin
    if AKey = '' then
    begin
      LObject := RootObject;
    end
    else
    begin
      LObject := GetValueByName(GetObjectUniqueName(AKey, obj), RootObject);
    end;

    if IsAssigned(LObject) then
    begin
      LType := TSvRttiInfo.GetType(obj);

      if Length(ACustomProps) > 0 then
      begin
        for I := Low(ACustomProps) to High(ACustomProps) do
        begin
          LProp := LType.GetProperty(ACustomProps[I]);
          if Assigned(LProp) and (LProp.IsWritable) then
          begin
            LPropName := LProp.Name;  
            LPair := GetValueByName(LPropName, LObject);
            if IsAssigned(LPair) then
            begin
              LValue := SetValue(LPair, obj, LProp, LProp.PropertyType, LSkip);
              if not LSkip then
                TSvRttiInfo.SetValue(LProp, obj, LValue);
            end;
          end;
        end;
      end
      else
      begin
        if LType.IsRecord then
        begin
          for LField in LType.AsRecord.GetFields do
          begin
            LPropName := LField.Name;
            LPair := GetValueByName(LPropName, LObject);
            if IsAssigned(LPair) then
            begin
              LValue := SetValue(LPair, obj, TRttiProperty(LField), LField.FieldType, LSkip);
              if not LSkip then
                TSvRttiInfo.SetValue(LField, obj, LValue);
            end;
          end;
        end
        else
        begin
          for LProp in LType.GetProperties do
          begin
            if not LProp.IsWritable then
              Continue;

            if IsTransient(LProp) then
              Continue;

            LPropName := LProp.Name;
            if TSvSerializer.TryGetAttribute(LProp, LAttrib) then
            begin
              if (LAttrib.Name <> '') then
                LPropName := LAttrib.Name;
            end;

            LPair := GetValueByName(LPropName, LObject);
            if IsAssigned(LPair) then
            begin
              LValue := SetValue(LPair, obj, LProp, LProp.PropertyType, LSkip);
              if not LSkip then
                TSvRttiInfo.SetValue(LProp, obj, LValue);
            end;
          end;
        end;
      end;
    end;
  end;
end;

destructor TSvAbstractSerializer<T>.Destroy;
begin
  FErrors.Free;
  inherited Destroy;
end;

function TSvAbstractSerializer<T>.DoGetFromArray(const AFrom: TValue; AProp: TRttiProperty): T;
var
  i: Integer;
begin
  Result := CreateArray();
  for i := 0 to AFrom.GetArrayLength - 1 do
  begin
    ArrayAdd(Result, GetValue(AFrom.GetArrayElement(i), nil))
  end;
end;

function TSvAbstractSerializer<T>.DoGetFromClass(const AFrom: TValue; AProp: TRttiProperty): T;
var
  i, iRecNo: Integer;
  LJsonArray: T;
  LJsonObject: T;
  LType, LEnumType: TRttiType;
  LEnumMethod, LMoveNextMethod: TRttiMethod;
  LEnumerator: TValue;
  LCurrentProp: TRttiProperty;
  LDst: TDataSet;
begin
  Result := System.Default(T);
  LType := TSvRttiInfo.GetType(AFrom.TypeInfo);
  if Assigned(LType) and (AFrom.IsObject) then
  begin
    if AFrom.AsObject is TDataset then
    begin
      LDst := TDataSet(AFrom.AsObject);  
      Result := CreateArray;
      LDst.DisableControls;
      FormatSettings := FFormatSettings;
      try
        iRecNo := LDst.RecNo;
        LDst.First;
        while not LDst.Eof do
        begin
          LJsonObject := CreateObject;
          for i := 0 to LDst.Fields.Count - 1 do
          begin
            if LDst.Fields[i].IsNull then
            begin
              ObjectAdd(LJsonObject, LDst.Fields[i].FieldName, CreateNull);
            end
            else
            begin
              ObjectAdd(LJsonObject, LDst.Fields[i].FieldName, CreateString(LDst.Fields[i].AsString));
            end;  
          end; 
          ArrayAdd(Result, LJsonObject);
          LDst.Next;
        end;

        LDst.RecNo := iRecNo;
      finally
        FormatSettings := FOldFormatSettings;
        LDst.EnableControls;
      end;
    end
    else
    begin
      if IsTypeEnumerable(LType, LEnumMethod) then
      begin
        //enumerator exists
        Result := CreateArray;
        LJsonArray := Result;
        LEnumerator := LEnumMethod.Invoke(AFrom,[]);
        LEnumType :=  TSvRttiInfo.GetType(LEnumerator.TypeInfo);
        LMoveNextMethod := LEnumType.GetMethod('MoveNext');
        LCurrentProp := LEnumType.GetProperty('Current');
        Assert(Assigned(LMoveNextMethod), 'MoveNext method not found');
        Assert(Assigned(LCurrentProp), 'Current property not found');
        while LMoveNextMethod.Invoke(LEnumerator,[]).asBoolean do
        begin
          ArrayAdd(LJsonArray, GetValue(LCurrentProp.GetValue(LEnumerator.AsObject), LCurrentProp));
        end;

        if LEnumerator.IsObject then
        begin
          LEnumerator.AsObject.Free;
        end;
      end
      else
      begin
        //other object types
        Result := CreateObject;
        for LCurrentProp in LType.GetProperties do
        begin
          if IsTransient(LCurrentProp) then
          begin
            Continue;
          end;
          if LCurrentProp.Visibility in [mvPublic,mvPublished] then
          begin
            //try to serialize only published properties
            ObjectAdd(Result, LCurrentProp.Name, GetValue(LCurrentProp.GetValue(AFrom.AsObject), LCurrentProp));
          end;
        end;
      end;
    end;
  end;
end;

function TSvAbstractSerializer<T>.DoGetFromEnum(const AFrom: TValue; AProp: TRttiProperty): T;
var
  bVal: Boolean;
begin
  if AFrom.TryAsType<Boolean>(bVal) then
  begin
    Result := CreateBoolean(bVal);
  end
  else
  begin
    Result := CreateString(AFrom.ToString);
  end;
end;

function TSvAbstractSerializer<T>.DoGetFromInterface(const AFrom: TValue; AProp: TRttiProperty): T;
var
  LJsonArray: T;
  LEnumType, LFromType: TRttiType;
  LEnumMethod, LMoveNextMethod: TRttiMethod;
  LEnumerator: TValue;
  LCurrentMethod: TRttiMethod;
begin
  LFromType := TSvRttiInfo.GetType(AFrom);

  if not IsTypeEnumerable(LFromType, LEnumMethod) then
  begin
    Result := CreateString('Unsupported interface type. Must be enumerable.');
  end;

  Result := CreateArray;
  LJsonArray := Result;
  LEnumerator := LEnumMethod.Invoke(AFrom,[]);
  LEnumType :=  TRttiContext.Create.GetType(LEnumerator.TypeInfo);
  LMoveNextMethod := LEnumType.GetMethod('MoveNext');
  LCurrentMethod := LEnumType.GetMethod('GetCurrent');
  if not Assigned(LCurrentMethod) then
    LCurrentMethod := LEnumType.GetMethod('DoGetCurrent');

  Assert(Assigned(LMoveNextMethod), 'MoveNext method not found');
  Assert(Assigned(LCurrentMethod), 'GetCurrent method not found');
  while LMoveNextMethod.Invoke(LEnumerator,[]).asBoolean do
  begin
    ArrayAdd(LJsonArray, GetValue(LCurrentMethod.Invoke(LEnumerator, []), nil));
  end;

  if LEnumerator.IsObject then
  begin
    LEnumerator.AsObject.Free;
  end;
end;

function TSvAbstractSerializer<T>.DoGetFromRecord(const AFrom: TValue; AProp: TRttiProperty): T;
var
  LType: TRttiType;
  LRecordType: TRttiRecordType;
  LField: TRttiField;
begin
  LType := TSvRttiInfo.GetType(AFrom.TypeInfo);
  LRecordType := LType.AsRecord;
  Result := CreateObject;
  for LField in LRecordType.GetFields do
  begin
    ObjectAdd(Result, LField.Name, GetValue(LField.GetValue(AFrom.GetReferenceToRawData), nil));
  end;
end;

function TSvAbstractSerializer<T>.DoGetFromVariant(const AFrom: TValue; AProp: TRttiProperty): T;
var
  LVariant: Variant;
begin
  LVariant := AFrom.AsVariant;

  if VarIsNull(LVariant) or VarIsEmpty(LVariant) then
    Result := CreateNull
  else
    Result := CreateString(VarToStr(LVariant));
end;

function TSvAbstractSerializer<T>.DoSetFromArray(AJsonArray: T; AType: TRttiType;
  const AObj: TValue; AProp: TRttiProperty; var ASkip: Boolean): TValue;
var
  LJsonValue: T;
  arrVal: array of TValue;
  i, x: Integer;
  LDst: TDataSet;
  LField: TField;
  sVal: string;
  LObject: TObject;
  LValue, LEnumerator: TValue;
  bCreated: Boolean;
  LEnumMethod, LClearMethod: TRttiMethod;
  LParamsArray: TArray<TRttiParameter>;
  LJsonArray: T;
  LEnumArray: TArray<TEnumEntry<T>>;
begin
  bCreated := False;
  LValue := TValue.Empty;
  if Assigned(AType) then
  begin
    LJsonArray := AJsonArray;
    case AType.TypeKind of
      tkArray:
      begin
        SetLength(arrVal, GetArraySize(LJsonArray));

        for i := 0 to Length(arrVal)-1 do
        begin
          arrVal[i] := SetValue(GetArrayElement(LJsonArray, i), AObj, AProp, TRttiArrayType(AType).ElementType, ASkip);
        end;

        Result := TValue.FromArray(AType.Handle, arrVal);
      end;
      tkDynArray:
      begin
        SetLength(arrVal, GetArraySize(LJsonArray));

        for i := 0 to Length(arrVal)-1 do
        begin
          arrVal[i] := SetValue(GetArrayElement(LJsonArray, i), AObj, AProp, TRttiDynamicArrayType(AType).ElementType, ASkip);
        end;

        Result := TValue.FromArray(AType.Handle, arrVal);
      end;
      tkClass, tkInterface:
      begin
        if Assigned(AType) then
        begin
          if Assigned(AProp) then
          begin
            Result := TSvRttiInfo.GetValue(AProp, AObj);
            if (Result.IsObject) and (Result.AsObject is TDataSet) then
            begin
              //deserialize TDataSet
              LDst := TDataSet(Result.AsObject);  
              if IsAssigned(LJsonArray) then
              begin
                LDst.DisableControls;
                FormatSettings := FFormatSettings;
                try
                  for i := 0 to GetArraySize(LJsonArray) - 1 do
                  begin
                    try
                      LDst.Append;  
                      LJsonValue := GetArrayElement(LJsonArray, i); 
                      LEnumArray := EnumerateObject(LJsonValue);                      
                      for x := 0 to Length(LEnumArray) - 1 do
                      begin
                        //get fieldname from json object
                        sVal := LEnumArray[x].Key;
                        LField := LDst.FindField(sVal);
                        if Assigned(LField) then
                        begin
                          //check if not null
                          if IsNull(LEnumArray[x].Value) then
                            LField.Clear
                          else
                            LField.AsString := GetAsString(LEnumArray[x].Value);
                        end;
                      end;

                      LDst.Post;
                    except
                      on E:Exception do
                      begin
                        PostError(E.Message);
                      end;
                    end;
                  end;

                finally
                  LDst.EnableControls;
                  FormatSettings := FOldFormatSettings;
                end;
                Exit;
              end;   
            end;
          end
          else
          begin
            //if AProp not assigned then we must create it
            if AType.IsInstance then
            begin
              LObject := TSvRttiInfo.CreateType(AType.Handle);
              if Assigned(LObject) then
              begin
                LValue := LObject;
                bCreated := True;
              end;
            end;
          end;

          LEnumMethod := TSvRttiInfo.GetBasicMethod('Add', AType);
          if Assigned(LEnumMethod) and ( (Assigned(AProp)) or not (LValue.IsEmpty)  ) then
          begin
            if LValue.IsEmpty and Assigned(AProp) then
              LValue := TSvRttiInfo.GetValue(AProp, AObj);
           // AValue := AProp.GetValue(AObj);

            if (LValue.IsObject) and (LValue.AsObject = nil) then
            begin
              LValue := TSvRttiInfo.CreateType(AProp.PropertyType.Handle);
              bCreated := True;
            end;

            LClearMethod := TSvRttiInfo.GetBasicMethod('Clear', AType);
            if Assigned(LClearMethod) and (Length(LClearMethod.GetParameters) = 0) then
            begin
              LClearMethod.Invoke(LValue, []);
            end;

            LParamsArray := LEnumMethod.GetParameters;

            if Length(LParamsArray) > 1 then
            begin
              SetLength(arrVal, Length(LParamsArray));
              //probably we are dealing with key value pair class like TDictionary    
              for i := 0 to GetArraySize(LJsonArray) - 1 do
              begin
                LJsonValue := GetArrayElement(LJsonArray, i);                     

              //  Assert(Length(LParamsArray) = GetObjectSize(LJsonValue), 'Parameters count differ');                
                if IsObject(LJsonValue) then
                begin
                  LEnumArray := EnumerateObject(LJsonValue);
                  for x := 0 to Length(LEnumArray) - 1 do
                  begin
                    arrVal[x] := SetValue(LEnumArray[x].Value,
                      AObj, nil, LParamsArray[x].ParamType, ASkip);
                  end;
                end
                else if IsArray(LJsonValue) then
                begin
                  for x := 0 to GetArraySize(LJsonValue) - 1 do
                  begin
                    arrVal[x] :=
                      SetValue(GetArrayElement(LJsonValue, x), AObj, nil, LParamsArray[x].ParamType, ASkip);
                  end;
                end;

                LEnumerator := LEnumMethod.Invoke(LValue, arrVal);
              end;
            end
            else
            begin
              SetLength(arrVal, GetArraySize(LJsonArray));

              for i := 0 to Length(arrVal)-1 do
              begin
                LJsonValue := GetArrayElement(LJsonArray, i);   
                {TODO -oLinas -cGeneral : fix arguments}
                //AParams[0].ParamType.AsInstance.
                arrVal[i] := SetValue(LJsonValue, AObj, nil, LParamsArray[0].ParamType, ASkip);
                LEnumerator := LEnumMethod.Invoke(LValue, [arrVal[i]]);
              end;
            end;

            if bCreated then
            begin
              Result := LValue;
              ASkip := False;
              Exit;
            end;
            ASkip := True;
          end;
        end;
      end
      else
      begin
        ASkip := True;
        PostError('Cannot assign array data to non array type');
       // raise ESvSerializeException.Create('Cannot assign array data to non array type');
      end;
    end;
  end;
end;

function TSvAbstractSerializer<T>.DoSetFromNumber(AJsonNumber: T): TValue;
var
  sVal: string;
  LInt: Integer;
  LInt64: Int64;
begin
  sVal := GetAsString(AJsonNumber);

  if TryStrToInt(sVal, LInt) then
  begin
    Result := LInt;
  end
  else if TryStrToInt64(sVal, LInt64) then
  begin
    Result := LInt64;
  end
  else
  begin
    Result := GetAsDouble(AJsonNumber);
  end;
end;

function TSvAbstractSerializer<T>.DoSetFromObject(AJsonObject: T; AType: TRttiType;
  const AObj: TValue; AProp: TRttiProperty; var ASkip: Boolean): TValue;
var
  i: Integer;
  LField: TRttiField ;
  LRecordType: TRttiRecordType ;
  LCurrProp: TRttiProperty;
  LObject: TObject;
  LJsonObject: T;
  LEnumerator: TArray<TEnumEntry<T>>;
begin
  if Assigned(AType) then
  begin
    LJsonObject := AJsonObject;
    case AType.TypeKind of
      tkRecord:
      begin
        TValue.MakeWithoutCopy(nil, AType.Handle, Result);
        LRecordType := TSvRttiInfo.GetType(AType.Handle).AsRecord;

        LEnumerator := EnumerateObject(LJsonObject);
        
        for i := 0 to Length(LEnumerator) - 1 do
        begin
          //search for property name             
          LField := FindRecordFieldName(LEnumerator[i].Key, LRecordType);
          if Assigned(LField) then
          begin
            {DONE -oLinas -cGeneral : fix arguments}
            LField.SetValue(Result.GetReferenceToRawData,
              SetValue(LEnumerator[i].Value, AObj, nil, LField.FieldType, ASkip));
          end;
        end;
      end;
      tkClass:
      begin
        //AType := TSvRttiInfo.GetType(AType.Handle);
        if Assigned(AProp) and (AObj.AsObject <> nil) then
        begin
          Result := TSvRttiInfo.GetValue(AProp, AObj);
          if (Result.IsObject) and (Result.AsObject = nil) then
          begin
            Result := TSvRttiInfo.CreateType(AType.Handle);
          end;

          LEnumerator := EnumerateObject(LJsonObject);           
          for i := 0 to Length(LEnumerator) - 1 do
          begin
            LCurrProp := AType.GetProperty(LEnumerator[i].Key);
            if Assigned(LCurrProp) then
            begin
              if IsTransient(LCurrProp) then
              begin
                Continue;
              end;    
              LCurrProp.SetValue(GetRawPointer(Result), SetValue(LEnumerator[i].Value, Result {AObj}, LCurrProp,
                LCurrProp.PropertyType, ASkip));
            end;
          end;
         //  Result := AProp.GetValue(AObj);
        end
        else
        begin
          {DONE -oLinas -cGeneral : create new class and set all props}
          LObject := TSvRttiInfo.CreateType(AType.Handle);
          if Assigned(LObject) then
          begin
            Result := LObject;
            LEnumerator := EnumerateObject(LJsonObject);   
            for i := 0 to Length(LEnumerator) - 1 do
            begin
              LCurrProp := AType.GetProperty(LEnumerator[i].Key);
              if Assigned(LCurrProp) then
              begin
                if IsTransient(LCurrProp) then
                begin
                  Continue;
                end;
                LCurrProp.SetValue(Result.AsObject, SetValue(LEnumerator[i].Value, Result, LCurrProp,
                  LCurrProp.PropertyType, ASkip));
              end;
            end;
          end;
        end;
      end
      else
      begin
        ASkip := True;
      end;
    end;
  end;  
end;

function TSvAbstractSerializer<T>.DoSetFromString(AJsonString: T; AType: TRttiType;
  var ASkip: Boolean): TValue;
var
  i: Integer;
begin
  if Assigned(AType) then
  begin
    case AType.TypeKind of
      tkEnumeration:
      begin
        Result := TValue.FromOrdinal(AType.Handle,
          GetEnumValue(AType.Handle, GetAsString(AJsonString)));
      end;
      tkSet:
      begin
        i := StringToSet(AType.Handle, GetAsString(AJsonString));
        TValue.Make(@i, AType.Handle, Result);
      end;
      tkVariant:
      begin
        Result := TValue.FromVariant(GetAsString(AJsonString));
      end;
      tkFloat:
      begin
        if (TypeInfo(TDate) = AType.Handle) then
        begin
          Result := StrToDateDef(GetAsString(AJsonString), MinDateTime, FFormatSettings);
        end
        else if (TypeInfo(TDateTime) = AType.Handle) then
        begin
          Result := StrToDateTimeDef(GetAsString(AJsonString), MinDateTime, FFormatSettings);
        end
        else
        begin
          Result := StrToFloatDef(GetAsString(AJsonString), 0, FFormatSettings);
        end;
      end;
      tkInteger:
      begin
        Result := StrToIntDef(GetAsString(AJsonString), 0);
      end;
      tkInt64:
      begin
        Result := StrToInt64Def(GetAsString(AJsonString), 0);
      end;
      tkUString, tkWString, tkLString, tkWChar, tkChar, tkString:
      begin
        //avoid skip
        Result := GetAsString(AJsonString);
      end
      else
      begin
        //error msg value, skip
        PostError('Cannot set unknown type value: ' + AType.ToString);
        ASkip := True;
      end;
    end;
  end
  else
  begin
    Result := GetAsString(AJsonString);
  end;
end;

procedure TSvAbstractSerializer<T>.EndDeSerialization(AStream: TStream);
begin
  NullStrictConvert := FOldNullStrConvert;
  FOwner.FErrors.AddRange(FErrors);
end;

procedure TSvAbstractSerializer<T>.EndSerialization;
begin
  NullStrictConvert := FOldNullStrConvert;
  FStringStream.WriteString(Self.ToString());
  FStringStream.Position := 0;
  FStream.CopyFrom(FStringStream, FStringStream.Size);
  FStringStream.Free;
  FOwner.FErrors.AddRange(FErrors);
end;

function TSvAbstractSerializer<T>.FindRecordFieldName(const AFieldName: string; ARecord: TRttiRecordType): TRttiField;
var
  LField: TRttiField;
begin
  for LField in ARecord.GetFields do
  begin
    if SameText(AFieldName, LField.Name) then
      Exit(LField);
  end;
  Result := nil;
end;

function TSvAbstractSerializer<T>.GetRootObj: T;
begin
  Result := FRootObj;
end;

function TSvAbstractSerializer<T>.GetObjectUniqueName(const AKey: string; obj: TValue): string;
begin
  if not obj.IsEmpty then
  begin
    Result := Format('%S.%S',[obj.TypeInfo.Name, AKey]);
  end
  else
  begin
    raise ESvSerializeException.Create('Cannot get object unique name. Object cannot be nil');
  end;
end;

function TSvAbstractSerializer<T>.GetRawPointer(const AValue: TValue): Pointer;
begin
  if AValue.IsObject then
    Result := AValue.AsObject
  else
    Result := AValue.GetReferenceToRawData;
end;

function TSvAbstractSerializer<T>.GetValue(const AFrom: TValue; AProp: TRttiProperty): T;
begin
  if IsTransient(AProp) then
    Exit( CreateNull());

  if AFrom.IsEmpty then
    Result := CreateNull()
  else
  begin
    //Result := nil;
    case AFrom.Kind of
      tkInteger: Result := CreateInteger(AFrom.AsInteger);
      tkInt64: Result := CreateInt64(AFrom.AsInt64);
      tkEnumeration:
      begin
        Result := DoGetFromEnum(AFrom, AProp);
      end;
      tkSet:
      begin
        Result := CreateString(AFrom.ToString);
      end;
      tkFloat: Result := CreateDouble(AFrom.AsExtended);
      tkString, tkWChar, tkLString, tkWString, tkChar, tkUString:
        Result := CreateString(AFrom.AsString);
      tkArray, tkDynArray:
      begin
        Result := DoGetFromArray(AFrom, AProp);
      end;
      tkVariant:
      begin
        Result := DoGetFromVariant(AFrom, AProp);
      end;
      tkClass:
      begin
        Result := DoGetFromClass(AFrom, AProp);
      end;
      tkInterface:
      begin
        Result := DoGetFromInterface(AFrom, AProp);
      end;
      tkRecord:
      begin
        Result := DoGetFromRecord(AFrom, AProp);
      end
     { tkMethod: ;
      tkInterface: ;
      tkClassRef: ;
      tkPointer: ;
      tkProcedure: ; }
      else
      begin
        PostError('Unsupported type: ' + AFrom.ToString);
        Result := CreateString('Unsupported type: ' + AFrom.ToString);
        //  raise ESvSerializeException.Create('Unsupported type: ' + AFrom.ToString);
      end;
    end;
  end;
end;

function TSvAbstractSerializer<T>.IsTransient(AProp: TRttiProperty): Boolean;
var
  LAttrib: TCustomAttribute;
begin
  if Assigned(AProp) then
  begin
    for LAttrib in AProp.GetAttributes do
    begin
      if LAttrib is SvTransientAttribute then
      begin
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TSvAbstractSerializer<T>.IsTypeEnumerable(ARttiType: TRttiType; out AEnumMethod: TRttiMethod): Boolean;
begin
  AEnumMethod := ARttiType.GetMethod('GetEnumerator');
  Result := Assigned(AEnumMethod);
end;


procedure TSvAbstractSerializer<T>.PostError(const ErrorText: string);
begin
  if ErrorText <> '' then
    FErrors.Add(ErrorText);
end;

procedure TSvAbstractSerializer<T>.SerializeObject(const AKey: string; const obj: TValue;
  AStream: TStream; ACustomProps: TStringDynArray);
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
  LObject: T;
  LPropName: string;
  I: Integer;
  LField: TRttiField;
  LAttrib: SvSerialize;
begin
  if not obj.IsEmpty and (Assigned(AStream)) then
  begin
    FStream := AStream;
    LType := TSvRttiInfo.GetType(obj);
    //create main object
    if AKey = '' then
    begin
      LObject := RootObject;
    end
    else
    begin
      LObject := CreateObject;
      ObjectAdd(RootObject, GetObjectUniqueName(AKey, obj), LObject);
    end;

    if Length(ACustomProps) > 0 then
    begin
      for I := Low(ACustomProps) to High(ACustomProps) do
      begin
        LProp := LType.GetProperty(ACustomProps[I]);
        if Assigned(LProp) then
        begin
          LValue := TSvRttiInfo.GetValue(LProp, obj); 
          LPropName := LProp.Name;        
          ObjectAdd(LObject, LPropName, GetValue(LValue, LProp));
        end;        
      end;
    end
    else
    begin
      if LType.IsRecord then
      begin
        for LField in LType.AsRecord.GetFields do
        begin
          LValue := LField.GetValue(obj.GetReferenceToRawData);
          LPropName := LField.Name;
          ObjectAdd(LObject, LPropName, GetValue(LValue, TRttiProperty(LField)));
        end;
      end
      else
      begin
        for LProp in LType.GetProperties do
        begin
          if IsTransient(LProp) then
            Continue;

          LValue := TSvRttiInfo.GetValue(LProp, obj);

          LPropName := LProp.Name;
          if TSvSerializer.TryGetAttribute(LProp, LAttrib) then
          begin
            if (LAttrib.Name <> '') then
              LPropName := LAttrib.Name;
          end;

          ObjectAdd(LObject, LPropName, GetValue(LValue, LProp));
        end;
      end;
    end;
  end;
end;

procedure TSvAbstractSerializer<T>.SetRootObj(const Value: T);
begin
  FRootObj := Value;
end;

function TSvAbstractSerializer<T>.SetValue(const AFrom: T; const AObj: TValue; AProp: TRttiProperty;
  AType: TRttiType; var Skip: Boolean): TValue;
begin
  Skip := False;

  if IsTransient(AProp) then
  begin
    Skip := True;
    Exit(TValue.Empty);
  end;

  if IsAssigned(AFrom) then
  begin
    if IsNumber(AFrom) then
    begin
      Result := DoSetFromNumber(AFrom);
    end
    else if IsString(AFrom) then
    begin
      Result := DoSetFromString(AFrom, AType, Skip);
    end
    else if IsBoolean(AFrom) then
    begin
      Result := GetAsBoolean(AFrom);
    end
    else if IsNull(AFrom) then
    begin
      Result := TValue.Empty;
    end
    else if IsArray(AFrom) then
    begin
      Result := DoSetFromArray(AFrom, AType, AObj, AProp, Skip);
    end
    else if IsObject(AFrom) then
    begin
      Result := DoSetFromObject(AFrom, AType, AObj, AProp, Skip);
    end
    else
    begin
      Skip := True;
      PostError('Unsupported value type: ' + GetTypeName( System.TypeInfo(T)));
    end;
  end;
end;

{ TSvRttiInfo }

class constructor TSvRttiInfo.Create;
begin
  FCtx := TRttiContext.Create;
  FRegisteredConstructors := TDictionary<PTypeInfo,TConstructorMethod>.Create;
end;

class destructor TSvRttiInfo.Destroy;
begin
  FCtx.Free;
  FRegisteredConstructors.Free;
end;

class function TSvRttiInfo.FindType(const AQualifiedName: string): TRttiType;
begin
  Result := FCtx.FindType(AQualifiedName);
end;

class function TSvRttiInfo.GetBasicMethod(const AMethodName: string; AType: TRttiType): TRttiMethod;
var
  LMethod: TRttiMethod;
  iParCount, iCurrParCount, iCount: Integer;
begin
  LMethod := nil;
  iParCount := 0;
  iCurrParCount := 0;
  for Result in AType.GetMethods do
  begin
    if SameText(Result.Name, AMethodName) then
    begin
      iCount := Length(Result.GetParameters);
      if (iCount < iParCount) or (iCount = 0) then
      begin
        Exit;
      end
      else
      begin
        if (iCount > iCurrParCount) then
        begin
          Inc(iParCount);
        end;

        iCurrParCount := iCount;
        LMethod := Result;
      end;
    end;
  end;

  Result := LMethod;
end;

class function TSvRttiInfo.GetPackages: TArray<TRttiPackage>;
begin
  Result := FCtx.GetPackages;
end;

class function TSvRttiInfo.GetType(AClass: TClass): TRttiType;
begin
  Result := FCtx.GetType(AClass);
end;

class function TSvRttiInfo.GetTypes: TArray<TRttiType>;
begin
  Result := FCtx.GetTypes;
end;

class function TSvRttiInfo.GetValue(AProp: TRttiProperty; const AInstance: TValue): TValue;
begin
  if AInstance.IsObject then
    Result := AProp.GetValue(AInstance.AsObject)
  else
    Result := AProp.GetValue(AInstance.GetReferenceToRawData);
end;

class procedure TSvRttiInfo.RegisterConstructor(ATypeInfo: PTypeInfo; AMethod: TConstructorMethod);
begin
  FRegisteredConstructors.AddOrSetValue(ATypeInfo, AMethod);
end;

class procedure TSvRttiInfo.SetValue(AField: TRttiField; const AInstance, AValue: TValue);
begin
  if AInstance.IsObject then
    AField.SetValue(AInstance.AsObject, AValue)
  else
    AField.SetValue(AInstance.GetReferenceToRawData, AValue);
end;

class procedure TSvRttiInfo.SetValue(AProp: TRttiProperty; const AInstance, AValue: TValue);
begin
  if AInstance.IsObject then
    AProp.SetValue(AInstance.AsObject, AValue)
  else
    AProp.SetValue(AInstance.GetReferenceToRawData, AValue);
end;

class function TSvRttiInfo.GetType(ATypeInfo: Pointer): TRttiType;
begin
  Result := FCtx.GetType(ATypeInfo);
end;

class function TSvRttiInfo.GetType(const Value: TValue): TRttiType;
begin
  Result := GetType(Value.TypeInfo);
end;

class function TSvRttiInfo.CreateType(ATypeInfo: PTypeInfo): TObject;
var
  LType: TRttiType;
  LMethCreate: TRttiMethod;
  LInstanceType: TRttiInstanceType;
  LRegisteredConstructorMethod: TConstructorMethod;
begin
  if FRegisteredConstructors.TryGetValue(ATypeInfo, LRegisteredConstructorMethod) then
  begin
    Result := LRegisteredConstructorMethod();
  end
  else
  begin
    LType := GetType(ATypeInfo);
    for LMethCreate in LType.GetMethods do
    begin
      if (LMethCreate.IsConstructor) and (Length(LMethCreate.GetParameters) = 0)  then
      begin
        LInstanceType := LType.AsInstance;

        Result := LMethCreate.Invoke(LInstanceType.MetaclassType, []).AsObject;
        Exit;
      end;
    end;
    Result := nil;
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

{ TSerializerFactory }

class constructor TSerializerFactory.Create;
begin
  FRegisteredSerializers := TDictionary<TSvSerializeFormat,TSvAbstractNonGenericSerializerClass>.Create();
end;

class destructor TSerializerFactory.Destroy;
begin
  FRegisteredSerializers.Free;
end;

class function TSerializerFactory.GetInstance(AOwner: TSvSerializer;
  ASerializerFormat: TSvSerializeFormat): ISerializer;
var
  LSerializerClass: TSvAbstractNonGenericSerializerClass;
begin
  if not FRegisteredSerializers.TryGetValue(ASerializerFormat, LSerializerClass) then
    raise ESerializerFactoryException.CreateFmt('Serializer not registered: "%S".',
      [GetEnumName(TypeInfo(TSvSerializeFormat), Ord(ASerializerFormat))]);

  Result := LSerializerClass.Create(AOwner);
end;

class procedure TSerializerFactory.RegisterSerializer(ASerializerFormat: TSvSerializeFormat;
  AClass: TSvAbstractNonGenericSerializerClass);
begin
  FRegisteredSerializers.AddOrSetValue(ASerializerFormat, AClass);
end;

{ TSvAbstractNonGenericSerializer }

constructor TSvAbstractNonGenericSerializer.Create(AOwner: TSvSerializer);
begin
  inherited Create();
end;

end.

