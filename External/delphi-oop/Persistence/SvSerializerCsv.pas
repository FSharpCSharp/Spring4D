unit SvSerializerCsv;

interface

uses
  Classes, SvSerializer, SysUtils, SvCsvSerializer, Rtti, SvSerializerAbstract, Types;

type
  TSvSerializerCSV = class(TSvAbstractSerializer<TCsvBaseElement>)
  private
    FInternalSerializer: TSvCsvSerializer;

    function GetColumnCountFromObject(const AValue: TValue): Integer;
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function ToString(): string; override;

    //getters
    function GetAsString(AValue: TCsvBaseElement): string; override;
    function GetAsDouble(AValue: TCsvBaseElement): Double; override;
    function GetAsBoolean(AValue: TCsvBaseElement): Boolean; override;
    function GetValueByName(const AName: string; AObject: TCsvBaseElement): TCsvBaseElement; override;
    function GetArraySize(AValue: TCsvBaseElement): Integer; override;
    function GetArrayElement(AArray: TCsvBaseElement; AIndex: Integer): TCsvBaseElement; override;
    function GetObjectSize(AValue: TCsvBaseElement): Integer; override;

    function EnumerateObject(AObject: TCsvBaseElement): TArray<TEnumEntry<TCsvBaseElement>>; override;
    //setters
    function CreateObject(): TCsvBaseElement; override;
    function CreateArray(): TCsvBaseElement; override;
    function CreateBoolean(AValue: Boolean): TCsvBaseElement; override;
    function CreateString(const AValue: string): TCsvBaseElement; override;
    function CreateNull(): TCsvBaseElement; override;
    function CreateInteger(AValue: Integer): TCsvBaseElement; override;
    function CreateInt64(AValue: Int64): TCsvBaseElement; override;
    function CreateDouble(AValue: Double): TCsvBaseElement; override;

    //
    function IsAssigned(AValue: TCsvBaseElement): Boolean; override;
    function IsNumber(AValue: TCsvBaseElement): Boolean; override;
    function IsString(AValue: TCsvBaseElement): Boolean; override;
    function IsBoolean(AValue: TCsvBaseElement): Boolean; override;
    function IsNull(AValue: TCsvBaseElement): Boolean; override;
    function IsArray(AValue: TCsvBaseElement): Boolean; override;
    function IsObject(AValue: TCsvBaseElement): Boolean; override;

    procedure ArrayAdd(AArray: TCsvBaseElement; const AValue: TCsvBaseElement); override;
    procedure ObjectAdd(AObject: TCsvBaseElement; const AName: string; const AValue: TCsvBaseElement); override;

    procedure SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); override;
  public
    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;
  end;

implementation

uses
  SvSerializerFactory
  ;


{ TSvSerializerCSV }

procedure TSvSerializerCSV.ArrayAdd(AArray: TCsvBaseElement; const AValue: TCsvBaseElement);
var
  LRoot: TCsvRootElement;
begin
  LRoot := AArray as TCsvRootElement;
  LRoot.AddElement(AValue as TCsvObjectElement);
end;

procedure TSvSerializerCSV.BeginDeSerialization(AStream: TStream);
begin
  inherited;
  if AStream is TStringStream then
  begin
    FInternalSerializer.ParseCsvString(TStringStream(AStream).DataString);
  end
  else
  begin
    FInternalSerializer.ParseStream(AStream);
  end;

  RootObject := FInternalSerializer.Root;
end;

procedure TSvSerializerCSV.BeginSerialization;
begin
  inherited;
  RootObject := FInternalSerializer.Root;
end;

constructor TSvSerializerCSV.Create(AOwner: TSvSerializer);
begin
  inherited;
  FInternalSerializer := TSvCsvSerializer.Create();
  FInternalSerializer.Delimiter := AOwner.CsvDelimiter;
  FInternalSerializer.QuoteChar := AOwner.CsvQuoteChar;
  FInternalSerializer.FirstLineIsColumns := AOwner.CsvFirstLineColumns;
  FInternalSerializer.WriterUseQuotes := AOwner.CsvWriterUseQuotes;
end;

function TSvSerializerCSV.CreateArray: TCsvBaseElement;
begin
  Result := RootObject;
end;

function TSvSerializerCSV.CreateBoolean(AValue: Boolean): TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
  Result.Value := BoolToStr(AValue);
end;

function TSvSerializerCSV.CreateDouble(AValue: Double): TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
  Result.Value := FloatToStr(AValue, FFormatSettings);
end;

function TSvSerializerCSV.CreateInt64(AValue: Int64): TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
  Result.Value := IntToStr(AValue);
end;

function TSvSerializerCSV.CreateInteger(AValue: Integer): TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
  Result.Value := IntToStr(AValue);
end;

function TSvSerializerCSV.CreateNull: TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
end;

function TSvSerializerCSV.CreateObject: TCsvBaseElement;
begin
  Result := TCsvObjectElement.Create;
end;

function TSvSerializerCSV.CreateString(const AValue: string): TCsvBaseElement;
begin
  Result := TCsvBaseElement.Create;
  Result.Value := AValue;
end;

destructor TSvSerializerCSV.Destroy;
begin
  FInternalSerializer.Free;
  inherited;
end;

procedure TSvSerializerCSV.EndDeSerialization(AStream: TStream);
begin
  inherited;
end;

procedure TSvSerializerCSV.EndSerialization;
begin
  inherited;
end;

function TSvSerializerCSV.EnumerateObject(
  AObject: TCsvBaseElement): TArray<TEnumEntry<TCsvBaseElement>>;
var
  i: Integer;
  LObject: TCsvObjectElement;
  LElement: TCsvNamedElement;
begin
  LObject := TCsvObjectElement(AObject);
  SetLength(Result, LObject.Count);

  for i := 0 to Length(Result) - 1 do
  begin
    LElement := LObject.GetObjectAt(i);
    Result[i].Key := LElement.Name;
    Result[i].Value := LElement.ElementValue;
  end;
end;

function TSvSerializerCSV.GetArrayElement(AArray: TCsvBaseElement;
  AIndex: Integer): TCsvBaseElement;
begin
  Result := TCsvRootElement(AArray).GetObjectElement(AIndex);
end;

function TSvSerializerCSV.GetArraySize(AValue: TCsvBaseElement): Integer;
begin
  Result := TCsvRootElement(AValue).Count;
end;

function TSvSerializerCSV.GetAsBoolean(AValue: TCsvBaseElement): Boolean;
begin
  Result := StrToBoolDef(AValue.Value, False);
end;

function TSvSerializerCSV.GetAsDouble(AValue: TCsvBaseElement): Double;
begin
  Result := StrToFloatDef(AValue.Value, 0, FFormatSettings);
end;

function TSvSerializerCSV.GetAsString(AValue: TCsvBaseElement): string;
begin
  Result := AValue.Value;
end;

function TSvSerializerCSV.GetColumnCountFromObject(const AValue: TValue): Integer;
var
  LProp: TRttiProperty;
  LAttrib: TCustomAttribute;
  LTransientColumns: Integer;
  LType: TRttiType;
  LEnumMethod: TRttiMethod;
  LEnumValue: TValue;
begin
  Result := 0;
  LTransientColumns := 0;

  LType := TRttiContext.Create.GetType(AValue.TypeInfo);

  if IsTypeEnumerable(LType, LEnumMethod) then
  begin
    LEnumValue := LEnumMethod.Invoke(AValue, []);
    LProp := TRttiContext.Create.GetType(LEnumValue.TypeInfo).GetProperty('Current');
    LType := LProp.PropertyType;
    if LEnumValue.IsObject then
    begin
      LEnumValue.AsObject.Free;
    end;
  end;

  for LProp in LType.GetProperties do
  begin
    Inc(Result);
    for LAttrib in LProp.GetAttributes do
    begin
      if LAttrib.ClassType = SvTransientAttribute then
      begin
        Inc(LTransientColumns);
        Continue;
      end;
    end;
  end;
  Result := Result - LTransientColumns;
end;

function TSvSerializerCSV.GetObjectSize(AValue: TCsvBaseElement): Integer;
var
  LObjectElement: TCsvObjectElement;
begin
  Result := 0;
  if AValue is TCsvObjectElement then
  begin
    LObjectElement := TCsvObjectElement(AValue);
    Result := LObjectElement.Count;
  end;
end;

function TSvSerializerCSV.GetValueByName(const AName: string;
  AObject: TCsvBaseElement): TCsvBaseElement;
begin
  Result := nil;
  if AObject is TCsvObjectElement then
  begin
    Result := TCsvObjectElement(AObject).GetObjectByName(AName);
  end
  else if AObject is TCsvRootElement then
  begin
    Result := TCsvRootElement(AObject).GetElementByName(0, AName);
  end;
end;

function TSvSerializerCSV.IsArray(AValue: TCsvBaseElement): Boolean;
begin
  Result := (AValue is TCsvRootElement);
end;

function TSvSerializerCSV.IsAssigned(AValue: TCsvBaseElement): Boolean;
begin
  Result := Assigned(AValue);
end;

function TSvSerializerCSV.IsBoolean(AValue: TCsvBaseElement): Boolean;
var
  LOut: Boolean;
begin
  Result := TryStrToBool(AValue.Value, LOut);
end;

function TSvSerializerCSV.IsNull(AValue: TCsvBaseElement): Boolean;
begin
  Result := False;
end;

function TSvSerializerCSV.IsNumber(AValue: TCsvBaseElement): Boolean;
var
  LOut: Extended;
begin
  Result := TryStrToFloat(AValue.Value, LOut, FFormatSettings);
end;

function TSvSerializerCSV.IsObject(AValue: TCsvBaseElement): Boolean;
begin
  Result := AValue is TCsvObjectElement;
end;

function TSvSerializerCSV.IsString(AValue: TCsvBaseElement): Boolean;
begin
  Result := (AValue.ClassType = TCsvBaseElement);
end;

procedure TSvSerializerCSV.ObjectAdd(AObject: TCsvBaseElement; const AName: string;
  const AValue: TCsvBaseElement);
var
  LObject: TCsvObjectElement;
  LRoot: TCsvRootElement;
  LValue: TCsvObjectElement;
begin
  if AObject is TCsvRootElement then
  begin
    LRoot := TCsvRootElement(AObject);

    if LRoot.Count = 0 then
    begin
      if not (AValue is TCsvObjectElement) then
      begin
        LValue := TCsvObjectElement.Create;
        LValue.AddObject(AName, AValue.Value);
      end
      else
        LValue := TCsvObjectElement(AValue);

      LRoot.AddElementWithLines(LValue);
    end
    else
    begin
      LValue := TCsvObjectElement(LRoot.GetObjectElement(LRoot.Count - 1));
      LValue.AddObject(AName, AValue.Value);
      LRoot.AddInternalLines(AName, AValue.Value);
    end;
  end
  else if AObject is TCsvObjectElement then
  begin
    LObject := AObject as TCsvObjectElement;
    LObject.AddObject(AName, AValue.Value);
    FInternalSerializer.Root.AddInternalLines(AName, AValue.Value);
  end;
  AValue.Free;
end;

procedure TSvSerializerCSV.SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
  ACustomProps: TStringDynArray);
begin
  FInternalSerializer.Root.ColumnCount := GetColumnCountFromObject(obj);
  inherited;
end;

function TSvSerializerCSV.ToString: string;
begin
  Result := FInternalSerializer.ToString;
end;

initialization
  TSerializerFactory.RegisterSerializer(sstCSV, TSvSerializerCSV);

end.
