unit SvSerializerAbstract;

interface

uses
  SvSerializer, SysUtils, Classes, Rtti, Generics.Collections, Types, TypInfo
  ;

type
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
    function GetFieldName(AField: TRttiField): string;
    function GetPropertyName(AProp: TRttiProperty): string;
    function GetRttiProperty(AType: TRttiType; const APropertyName: string): TRttiProperty;
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function DeserializeDataset(AArray: T; var AValue: TValue): Boolean; virtual;
    function SerializeDataset(const ADataset: TValue): T; virtual;
    function SerializeEnumerable(const AEnumerable: TValue; AEnumMethod: TRttiMethod): T; virtual;

    function TryDeserializeContainer(AArray: T; var AValue: TValue; AType: TRttiType
      ; var ACreated: Boolean; AProp: TRttiProperty; const AObj: TValue; var ASkip: Boolean): Boolean; virtual;
    procedure SetClassProperties(var AValue: TValue; var ASkip: Boolean; AType: TRttiType; AObjectToEnumerate: T); virtual;

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
    function IsFieldTransient(AField: TRttiField): Boolean;
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
    function CreateRootObject(AType: TRttiType): T; virtual;
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

implementation

uses
  Variants
 ,DB
 ,SvSerializerRtti
 ,Spring.Persistence.Mapping.Attributes
  ;


{ TSvAbstractNonGenericSerializer }

constructor TSvAbstractNonGenericSerializer.Create(AOwner: TSvSerializer);
begin
  inherited Create();
end;

{ TSvAbstractSerializer<T> }

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
  FOwner.Errors.Clear;
end;

constructor TSvAbstractSerializer<T>.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FErrors := TList<string>.Create;
  FFormatSettings := AOwner.FmtSettings;
  FOldFormatSettings := FormatSettings;
end;

function TSvAbstractSerializer<T>.CreateRootObject(AType: TRttiType): T;
var
  LEnumMethod: TRttiMethod;
begin
  Result := System.Default(T);
  if not IsTypeEnumerable(AType, LEnumMethod) then
    Result := CreateObject;
end;

function TSvAbstractSerializer<T>.DeserializeDataset(AArray: T; var AValue: TValue): Boolean;
var
  LDst: TDataSet;
  i, x: Integer;
  LJsonValue: T;
  LEnumArray: TArray<TEnumEntry<T>>;
  sVal: string;
  LField: TField;
begin
  Result := False;
  LDst := TDataSet(AValue.AsObject);
  if IsAssigned(AArray) then
  begin
    LDst.DisableControls;
    FormatSettings := FFormatSettings;
    try
      for i := 0 to GetArraySize(AArray) - 1 do
      begin
        try
          LDst.Append;
          LJsonValue := GetArrayElement(AArray, i);
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
      Result := True;
    finally
      LDst.EnableControls;
      FormatSettings := FOldFormatSettings;
    end;
  end;
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
  LEnumMethod: TRttiMethod;
begin
  inherited;
  if (obj.IsEmpty) and not (IsAssigned(RootObject)) then
    Exit;

  if (AKey = '') then
    LObject := RootObject
  else
    LObject := GetValueByName(GetObjectUniqueName(AKey, obj), RootObject);

  if not IsAssigned(LObject) then
    Exit;

  LType := TSvRttiInfo.GetType(obj);

  if Length(ACustomProps) > 0 then
  begin
    for I := Low(ACustomProps) to High(ACustomProps) do
    begin
      LProp := LType.GetProperty(ACustomProps[I]);
      if Assigned(LProp) and (LProp.IsWritable) then
      begin
        LPropName := GetPropertyName(LProp);
        LPair := GetValueByName(LPropName, LObject);
        if IsAssigned(LPair) then
        begin
          LValue := SetValue(LPair, obj, LProp, LProp.PropertyType, LSkip);
          if not LSkip and LProp.IsWritable  then
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
        LPropName := GetFieldName(LField);
        LPair := GetValueByName(LPropName, LObject);
        if IsAssigned(LPair) then
        begin
          LValue := SetValue(LPair, obj, TRttiProperty(LField), LField.FieldType, LSkip);
          if not LSkip then
            TSvRttiInfo.SetValue(LField, obj, LValue);
        end;
      end;
    end
    else if (IsTypeEnumerable(LType, LEnumMethod)) then
    begin
      TryDeserializeContainer(RootObject, obj, LType, LSkip, nil, obj, LSkip);
    end
    else
    begin
      for LProp in LType.GetProperties do
      begin
        if not LProp.IsWritable then
          Continue;

        if IsTransient(LProp) then
          Continue;

        LPropName := GetPropertyName(LProp);
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
  LType: TRttiType;
  LEnumMethod: TRttiMethod;
  LCurrentProp: TRttiProperty;
  LPropName: string;
begin
  Result := System.Default(T);
  LType := TSvRttiInfo.GetType(AFrom.TypeInfo);
  if Assigned(LType) and (AFrom.IsObject) then
  begin
    if AFrom.AsObject is TDataset then
    begin
      Result := SerializeDataset(AFrom);
    end
    else
    begin
      if IsTypeEnumerable(LType, LEnumMethod) then
      begin
        Result := SerializeEnumerable(AFrom, LEnumMethod);
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
            LPropName := GetPropertyName(LCurrentProp);
            //try to serialize only published properties
            ObjectAdd(Result, LPropName, GetValue(LCurrentProp.GetValue(AFrom.AsObject), LCurrentProp));
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
  LFieldName: string;
begin
  LType := TSvRttiInfo.GetType(AFrom.TypeInfo);
  LRecordType := LType.AsRecord;
  Result := CreateObject;
  for LField in LRecordType.GetFields do
  begin
    if IsFieldTransient(LField) then
      Continue;

    LFieldName := GetFieldName(LField);
    ObjectAdd(Result, LFieldName, GetValue(LField.GetValue(AFrom.GetReferenceToRawData), nil));
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
  arrVal: array of TValue;
  i: Integer;
  LObject: TObject;
  LValue: TValue;
  bCreated: Boolean;
begin
  bCreated := False;
  LValue := TValue.Empty;
  if Assigned(AType) then
  begin
    case AType.TypeKind of
      tkArray:
      begin
        SetLength(arrVal, GetArraySize(AJsonArray));
        for i := 0 to Length(arrVal)-1 do
        begin
          arrVal[i] := SetValue(GetArrayElement(AJsonArray, i), AObj, AProp, TRttiArrayType(AType).ElementType, ASkip);
        end;
        Result := TValue.FromArray(AType.Handle, arrVal);
      end;
      tkDynArray:
      begin
        SetLength(arrVal, GetArraySize(AJsonArray));
        for i := 0 to Length(arrVal)-1 do
        begin
          arrVal[i] := SetValue(GetArrayElement(AJsonArray, i), AObj, AProp, TRttiDynamicArrayType(AType).ElementType, ASkip);
        end;
        Result := TValue.FromArray(AType.Handle, arrVal);
      end;
      tkClass, tkInterface:
      begin
        if not Assigned(AType) then
          Exit;

        if not Assigned(AProp) then
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
        end
        else
        begin
          Result := TSvRttiInfo.GetValue(AProp, AObj);
          if (Result.IsObject) and (Result.AsObject is TDataSet) then
          begin
            DeserializeDataset(AJsonArray, Result);
            Exit;
          end;
        end;

        if TryDeserializeContainer(AJsonArray, LValue, AType, bCreated, AProp, AObj, ASkip) then
        begin
          if bCreated then
          begin
            Result := LValue;
            ASkip := False;
            Exit;
          end;
        end;
        ASkip := True;
      end
      else
      begin
        ASkip := True;
        PostError('Cannot assign array data to non array type');
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
  LObject: TObject;
  LEnumerator: TArray<TEnumEntry<T>>;
begin
  if Assigned(AType) then
  begin
    case AType.TypeKind of
      tkRecord:
      begin
        TValue.MakeWithoutCopy(nil, AType.Handle, Result);
        LRecordType := TSvRttiInfo.GetType(AType.Handle).AsRecord;

        LEnumerator := EnumerateObject(AJsonObject);
        for i := 0 to Length(LEnumerator) - 1 do
        begin
          //search for property name
          LField := FindRecordFieldName(LEnumerator[i].Key, LRecordType);
          if Assigned(LField) and not IsFieldTransient(LField) then
          begin
            {DONE -oLinas -cGeneral : fix arguments}
            LField.SetValue(GetRawPointer(Result),
              SetValue(LEnumerator[i].Value, AObj, nil, LField.FieldType, ASkip));
          end;
        end;
      end;
      tkClass:
      begin
        if Assigned(AProp) and ( (AObj.IsObject) and (AObj.AsObject <> nil)) then
        begin
          Result := TSvRttiInfo.GetValue(AProp, AObj);
          if (Result.IsObject) and (Result.AsObject = nil) then
          begin
            Result := TSvRttiInfo.CreateType(AType.Handle);
          end;
          SetClassProperties(Result, ASkip, AType, AJsonObject);
        end
        else
        begin
          {DONE -oLinas -cGeneral : create new class and set all props}
          LObject := TSvRttiInfo.CreateType(AType.Handle);
          if Assigned(LObject) then
          begin
            Result := LObject;
            SetClassProperties(Result, ASkip, AType, AJsonObject);
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
  FOwner.Errors.AddRange(FErrors);
end;

procedure TSvAbstractSerializer<T>.EndSerialization;
begin
  NullStrictConvert := FOldNullStrConvert;

  if FStream is TStringStream then
  begin
    TStringStream(FStream).WriteString(Self.ToString());
  end
  else
  begin
    FStringStream.WriteString(Self.ToString());
    FStringStream.Position := 0;
    FStream.CopyFrom(FStringStream, FStringStream.Size);
  end;
  FStringStream.Free;
  FOwner.Errors.AddRange(FErrors);
end;

function TSvAbstractSerializer<T>.FindRecordFieldName(const AFieldName: string; ARecord: TRttiRecordType): TRttiField;
var
  LField: TRttiField;
  LCurrentFieldname: string;
begin
  for LField in ARecord.GetFields do
  begin
    LCurrentFieldname := GetFieldName(LField);
    if SameText(AFieldName, LCurrentFieldname) then
      Exit(LField);
  end;
  Result := nil;
end;

function TSvAbstractSerializer<T>.GetRootObj: T;
begin
  Result := FRootObj;
end;

function TSvAbstractSerializer<T>.GetRttiProperty(AType: TRttiType;
  const APropertyName: string): TRttiProperty;
var
  LPropName: string;
begin
  for Result in AType.GetProperties do
  begin
    LPropName := GetPropertyName(Result);

    if SameText(APropertyName, LPropName) then
    begin
      Exit;
  end;
  end;
  Result := nil;
end;

function TSvAbstractSerializer<T>.GetFieldName(AField: TRttiField): string;
var
  LAttrib: TCustomAttribute;
begin
  Result := AField.Name;
  for LAttrib in AField.GetAttributes do
  begin
    if LAttrib is ColumnAttribute then
    begin
      Exit(ColumnAttribute(LAttrib).ColumnName);
    end
    else if LAttrib is SvSerializeAttribute then
    begin
      if (SvSerializeAttribute(LAttrib).Name <> '') then
        Exit(SvSerializeAttribute(LAttrib).Name);
end;
  end;
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

function TSvAbstractSerializer<T>.GetPropertyName(AProp: TRttiProperty): string;
var
  LAttrib: TCustomAttribute;
begin
  Result := AProp.Name;
  for LAttrib in AProp.GetAttributes do
  begin
    if LAttrib is ColumnAttribute then
    begin
      Exit(ColumnAttribute(LAttrib).ColumnName);
    end
    else if LAttrib is SvSerializeAttribute then
    begin
      if (SvSerializeAttribute(LAttrib).Name <> '') then
        Exit(SvSerializeAttribute(LAttrib).Name)
  end;
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
      tkFloat:
      begin
        if (TypeInfo(TDate) = AFrom.TypeInfo) then
        begin
          Result := CreateString(DateToStr(AFrom.AsExtended, FFormatSettings));
        end
        else if (TypeInfo(TDateTime) = AFrom.TypeInfo)  then
        begin
          Result := CreateString(DateTimeToStr(AFrom.AsExtended, FFormatSettings));
        end
        else
          Result := CreateDouble(AFrom.AsExtended);
      end;
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

function TSvAbstractSerializer<T>.IsFieldTransient(AField: TRttiField): Boolean;
var
  LAttrib: TCustomAttribute;
begin
  if Assigned(AField) then
  begin
    for LAttrib in AField.GetAttributes do
    begin
      if LAttrib is SvTransientAttribute then
      begin
        Exit(True);
      end;
    end;
  end;
  Result := False;
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

function TSvAbstractSerializer<T>.SerializeDataset(const ADataset: TValue): T;
var
  LDst: TDataSet;
  iRecNo, i: Integer;
  LJsonObject: T;
begin
  LDst := TDataSet(ADataset.AsObject);
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
end;

function TSvAbstractSerializer<T>.SerializeEnumerable(const AEnumerable: TValue; AEnumMethod: TRttiMethod): T;
var
  LEnumerator: TValue;
  LEnumType: TRttiType;
  LMoveNextMethod: TRttiMethod;
  LCurrentProp: TRttiProperty;
begin
  Result := CreateArray;
  LEnumerator := AEnumMethod.Invoke(AEnumerable,[]);
  LEnumType :=  TSvRttiInfo.GetType(LEnumerator.TypeInfo);
  LMoveNextMethod := LEnumType.GetMethod('MoveNext');
  LCurrentProp := LEnumType.GetProperty('Current');
  Assert(Assigned(LMoveNextMethod), 'MoveNext method not found');
  Assert(Assigned(LCurrentProp), 'Current property not found');
  while LMoveNextMethod.Invoke(LEnumerator,[]).asBoolean do
  begin
    ArrayAdd(Result, GetValue(LCurrentProp.GetValue(GetRawPointer(LEnumerator)), LCurrentProp));
  end;

  if LEnumerator.IsObject then
  begin
    LEnumerator.AsObject.Free;
  end;
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
  LEnumMethod: TRttiMethod;
begin
  if (obj.IsEmpty) and not (Assigned(AStream)) then
    Exit;

  FStream := AStream;
  LType := TSvRttiInfo.GetType(obj);

  if not IsAssigned(RootObject) then
    RootObject := CreateRootObject(LType);

  //create main object
  if (AKey = '') then
  begin
    LObject := RootObject;
  end
  else
  begin
    if not IsAssigned(RootObject) then
      RootObject := CreateObject();
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
        LPropName := GetPropertyName(LProp);
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
        LPropName := GetFieldName(LField);
        ObjectAdd(LObject, LPropName, GetValue(LValue, TRttiProperty(LField)));
      end;
    end
    else if IsTypeEnumerable(LType, LEnumMethod) then
    begin
      RootObject := GetValue(obj, nil);
    end
    else
    begin
      for LProp in LType.GetProperties do
      begin
        if IsTransient(LProp) then
          Continue;

        LValue := TSvRttiInfo.GetValue(LProp, obj);
        LPropName := GetPropertyName(LProp);
        ObjectAdd(LObject, LPropName, GetValue(LValue, LProp));
      end;
    end;
  end;
end;

procedure TSvAbstractSerializer<T>.SetClassProperties(var AValue: TValue; var ASkip: Boolean;
  AType: TRttiType; AObjectToEnumerate: T);
var
  i: Integer;
  LCurrProp: TRttiProperty;
  LEnumerator: TArray<TEnumEntry<T>>;
  LValue: TValue;
begin
  LEnumerator := EnumerateObject(AObjectToEnumerate);
  for i := 0 to Length(LEnumerator) - 1 do
  begin
    LCurrProp := GetRttiProperty(AType, LEnumerator[i].Key);
    if Assigned(LCurrProp) then
    begin
      if IsTransient(LCurrProp) or (not LCurrProp.IsWritable) then
      begin
        Continue;
      end;

      LValue := SetValue(LEnumerator[i].Value, AValue, LCurrProp, LCurrProp.PropertyType, ASkip);
      if not ASkip then
        TSvRttiInfo.SetValue(LCurrProp, AValue, LValue);
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
      //try get value as string and convert it to corresponding type
      Result := DoSetFromString(AFrom, AType, Skip);
     // if Skip then
     //   PostError('Unsupported value type: ' + GetTypeName( System.TypeInfo(T)));
    end;
  end;
end;

function TSvAbstractSerializer<T>.TryDeserializeContainer(AArray: T; var AValue: TValue;
  AType: TRttiType; var ACreated: Boolean; AProp: TRttiProperty; const AObj: TValue; var ASkip: Boolean): Boolean;
var
  LEnumMethod, LClearMethod: TRttiMethod;
  LParamsArray: TArray<TRttiParameter>;
  arrVal: array of TValue;
  LJsonValue: T;
  LEnumArray: TArray<TEnumEntry<T>>;
  i, x: Integer;
  LEnumerator: TValue;
begin
  Result := False;
  LEnumMethod := TSvRttiInfo.GetBasicMethod('Add', AType);
  if Assigned(LEnumMethod) and ( (Assigned(AProp)) or not (AValue.IsEmpty)  ) then
  begin

    if AValue.IsEmpty and Assigned(AProp) then
      AValue := TSvRttiInfo.GetValue(AProp, AObj);
   // AValue := AProp.GetValue(AObj);

    if (AValue.IsObject) and (AValue.AsObject = nil) then
    begin
      AValue := TSvRttiInfo.CreateType(AProp.PropertyType.Handle);
      ACreated := True;
    end;

    LClearMethod := TSvRttiInfo.GetBasicMethod('Clear', AType);
    if Assigned(LClearMethod) and (Length(LClearMethod.GetParameters) = 0) then
    begin
      LClearMethod.Invoke(AValue, []);
    end;

    LParamsArray := LEnumMethod.GetParameters;

    if Length(LParamsArray) > 1 then
    begin
      SetLength(arrVal, Length(LParamsArray));
      //probably we are dealing with key value pair class like TDictionary
      for i := 0 to GetArraySize(AArray) - 1 do
      begin
        LJsonValue := GetArrayElement(AArray, i);

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

        LEnumerator := LEnumMethod.Invoke(AValue, arrVal);
      end;
    end
    else
    begin
      SetLength(arrVal, GetArraySize(AArray));

      for i := 0 to Length(arrVal)-1 do
      begin
        LJsonValue := GetArrayElement(AArray, i);
        {TODO -oLinas -cGeneral : fix arguments}
        //AParams[0].ParamType.AsInstance.
        arrVal[i] := SetValue(LJsonValue, AObj, nil, LParamsArray[0].ParamType, ASkip);
        LEnumerator := LEnumMethod.Invoke(AValue, [arrVal[i]]);
      end;
    end;

    Result := True;
  end;
end;

end.
