unit SvCsvSerializer;


interface

uses
  Generics.Collections
  ,Classes
  ,SvCsvParser
  ,SysUtils
  ;

type
  ESvCSVSerializerException = class(Exception);

  TCsvBaseElement = class
  private
    FValue: string;
  public
    procedure SetValue(const Value: string); virtual;
  public
    property Value: string read FValue write SetValue;
  end;

  TCsvNamedElement = class(TCsvBaseElement)
  private
    FName: string;
    function GetElementValue: TCsvBaseElement;
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property ElementValue: TCsvBaseElement read GetElementValue;
  end;

  TCsvObjectElement = class(TCsvBaseElement)
  private
    FElements: TObjectDictionary<string, TCsvNamedElement>;
  protected
    function GetKey(const AName: string): string; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure AddObject(const AName, AValue: string);
    function GetObjectAt(AIndex: Integer): TCsvNamedElement;
    function GetObjectByName(const AName: string): TCsvNamedElement;
    function TryGetObjectByName(const AName: string; out AObject: TCsvNamedElement): Boolean;

    function Count: Integer;
  end;

  TColumnIndexKey = string;

  TCsvRootElement = class(TCsvBaseElement)
  private
    FColumnCount: Integer;
    FInternalLines: TObjectList<TCsvBaseElement>;
    FFirstLineIsColumns: Boolean;
    FColumnIndex: TDictionary<TColumnIndexKey, Integer>;
    FElements: TObjectList<TCsvObjectElement>;
    function GetLineCount: Integer;
    function GetCount: Integer;
  protected
    function GetInternalIndex(const ALine, AColumn: Integer): Integer; virtual;
    function GetColumnIndexKey(const AKey: string): TColumnIndexKey;
    function TryGetColumnIndex(const AColumnName: string; out AIndex: Integer): Boolean; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear();

    procedure AddElement(AElement: TCsvObjectElement);
    procedure AddElementWithLines(AElement: TCsvBaseElement);
    procedure AddInternalLines(const AName, AValue: string);

    function GetElement(const ALine: Integer; AColumn: Integer): TCsvBaseElement; virtual;
    function GetObjectElement(const AIndex: Integer): TCsvBaseElement;

    function GetElementValue(const ALine: Integer; AColumn: Integer): string; virtual;
    function GetElementByName(const ALine: Integer; const AColumnName: string): TCsvBaseElement; virtual;

    property FirstLineIsColumns: Boolean read FFirstLineIsColumns write FFirstLineIsColumns;
    property ColumnCount: Integer read FColumnCount write FColumnCount;
    property LineCount: Integer read GetLineCount;

    property Count: Integer read GetCount;


  end;

  TSvCsvSerializer = class
  private
    FRoot: TCsvRootElement;
    FDelimiter: Char;
    FQuoteChar: Char;
    FWriterUseQuotes: Boolean;
    procedure SetFirstLineIsColumns(const Value: Boolean);
    function GetFirstLineIsColumns: Boolean;
  protected
    procedure RebuildColumnIndexCache();
    procedure BuildFromParser(AParser: TSvCsvParser);
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure ParseCsvString(const ACsvString: string);
    procedure ParseStream(AStream: TStream);

    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property FirstLineIsColumns: Boolean read GetFirstLineIsColumns write SetFirstLineIsColumns;
    property WriterUseQuotes: Boolean read FWriterUseQuotes write FWriterUseQuotes;
    function ToString(): string; override;

    property Root: TCsvRootElement read FRoot;
  end;

implementation

{ TCsvRootElement }

procedure TCsvRootElement.AddElement(AElement: TCsvObjectElement);
begin
  FElements.Add(AElement);
end;

procedure TCsvRootElement.AddElementWithLines(AElement: TCsvBaseElement);
var
  LObjectElement: TCsvObjectElement;
  LNamedElement: TCsvNamedElement;
  i: Integer;
begin
  if not (AElement is TCsvObjectElement) then
  begin
    raise ESvCSVSerializerException.Create('Cannot add non object element');
  end;

  LObjectElement := AElement as TCsvObjectElement;
  for i := 0 to LObjectElement.Count - 1 do
  begin
    LNamedElement := LObjectElement.GetObjectAt(i);
    AddInternalLines(LNamedElement.Name, LNamedElement.Value);
  end;
  AddElement(LObjectElement);
end;

procedure TCsvRootElement.AddInternalLines(const AName, AValue: string);
var
  LElement: TCsvNamedElement;
begin
  LElement := TCsvNamedElement.Create(AName);
  LElement.SetValue(AValue);
  FInternalLines.Add(LElement);
end;

procedure TCsvRootElement.Clear;
begin
  FInternalLines.Clear;
  FColumnIndex.Clear;
  FElements.Clear;
  FColumnCount := 0;
end;

constructor TCsvRootElement.Create;
begin
  inherited Create;
  FInternalLines := TObjectList<TCsvBaseElement>.Create(True);
  FColumnIndex := TDictionary<TColumnIndexKey, Integer>.Create(100);
  FElements := TObjectList<TCsvObjectElement>.Create(True);
  FColumnCount := 0;
end;

destructor TCsvRootElement.Destroy;
begin
  FInternalLines.Free;
  FColumnIndex.Free;
  FElements.Free;
  inherited Destroy;
end;

function TCsvRootElement.GetColumnIndexKey(const AKey: string): TColumnIndexKey;
begin
  Result := UpperCase(AKey);
end;

function TCsvRootElement.GetCount: Integer;
begin
  Result := FElements.Count;
end;

function TCsvRootElement.GetElement(const ALine: Integer; AColumn: Integer): TCsvBaseElement;
begin
  Result := FInternalLines[GetInternalIndex(ALine, AColumn)];
end;

function TCsvRootElement.GetElementByName(const ALine: Integer; const AColumnName: string): TCsvBaseElement;
var
  LColumnIndex: Integer;
begin
  if not TryGetColumnIndex(AColumnName, LColumnIndex) then
    raise ESvCSVSerializerException.Create(Format('Column "%S" does not exist.', [AColumnName]));

  Result := GetElement(ALine, LColumnIndex);
end;

function TCsvRootElement.GetElementValue(const ALine: Integer; AColumn: Integer): string;
var
  LElement: TCsvBaseElement;
begin
  LElement := GetElement(ALine, AColumn);
  if not Assigned(LElement) then
    raise ESvCSVSerializerException.Create(Format('Element (%D:%D) does not exist.', [ALine, AColumn]));
  Result := LElement.Value;
end;

function TCsvRootElement.GetInternalIndex(const ALine, AColumn: Integer): Integer;
begin
  if (FColumnCount < 1) then
    raise ESvCSVSerializerException.Create('CSV columns are not yet defined');

  Result := (FColumnCount * ALine) + AColumn;
end;

function TCsvRootElement.GetLineCount: Integer;
begin
  Result := 0;
  if (FColumnCount > 0) then
  begin
    Result := FInternalLines.Count div FColumnCount;
  end;
end;

function TCsvRootElement.GetObjectElement(const AIndex: Integer): TCsvBaseElement;
begin
  Result := FElements[AIndex];
end;

function TCsvRootElement.TryGetColumnIndex(const AColumnName: string; out AIndex: Integer): Boolean;
begin
  Result := FColumnIndex.TryGetValue(GetColumnIndexKey(AColumnName), AIndex);
end;

{ TSvCsvSerializer }

procedure TSvCsvSerializer.BuildFromParser(AParser: TSvCsvParser);
var
  LColumn, LRow: Integer;
  LElement: TCsvBaseElement;
  LObjectElement: TCsvObjectElement;
  LElementValue, LElementName: string;
begin
  if not FirstLineIsColumns then
    raise ESvCSVSerializerException.Create('Cannot parse CSV file without header columns.');

  FRoot.ColumnCount := AParser.Cols;

  for LRow := 1 to AParser.Rows - 1 do
  begin
    LObjectElement := nil;
    for LColumn := 0 to AParser.Cols - 1 do
    begin
      if (LColumn = 0) then
      begin
        LObjectElement := TCsvObjectElement.Create;
      end;

      LElementValue := AParser.Cell[LRow, LColumn];
      LElementName := AParser.Cell[0, LColumn];
      LElement := TCsvNamedElement.Create(LElementName);
      LObjectElement.AddObject(LElementName, LElementValue);

      LElement.Value := LElementValue;
      FRoot.FInternalLines.Add(LElement);

      if (LColumn = AParser.Cols - 1) then
      begin
        FRoot.FElements.Add(LObjectElement);
      end;
    end;
  end;

  if (FRoot.LineCount > 0) then
  begin
    RebuildColumnIndexCache();
  end;
end;

constructor TSvCsvSerializer.Create;
begin
  inherited Create;
  FRoot := TCsvRootElement.Create;
  FDelimiter := ',';
  FQuoteChar := '"';
end;

destructor TSvCsvSerializer.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TSvCsvSerializer.GetFirstLineIsColumns: Boolean;
begin
  Result := FRoot.FirstLineIsColumns;
end;

procedure TSvCsvSerializer.ParseCsvString(const ACsvString: string);
var
  LParser: TSvCsvParser;
begin
  FRoot.Clear;
  LParser := TSvCsvParser.Create;
  try
    LParser.Delim := Delimiter;
    LParser.Quote := QuoteChar;
    LParser.Parse(ACsvString);
    BuildFromParser(LParser);
  finally
    LParser.Free;
  end;
end;

procedure TSvCsvSerializer.ParseStream(AStream: TStream);
var
  LParser: TSvCsvParser;
begin
  FRoot.Clear;
  LParser := TSvCsvParser.Create;
  try
    LParser.Delim := Delimiter;
    LParser.Quote := QuoteChar;
    LParser.ParseFromStream(AStream);
    BuildFromParser(LParser);
  finally
    LParser.Free;
  end;
end;

procedure TSvCsvSerializer.RebuildColumnIndexCache;
var
  LColumn: Integer;
  LElement: TCsvNamedElement;
begin
  FRoot.FColumnIndex.Clear;
  for LColumn := 0 to FRoot.ColumnCount - 1 do
  begin
    LElement := FRoot.GetElement(0, LColumn) as TCsvNamedElement;
    FRoot.FColumnIndex.Add(FRoot.GetColumnIndexKey(LElement.Name), LColumn);
  end;
end;

procedure TSvCsvSerializer.SetFirstLineIsColumns(const Value: Boolean);
begin
  FRoot.FirstLineIsColumns := Value;
end;

function TSvCsvSerializer.ToString: string;
var
  LRow: Integer;
  LColumn: Integer;
  LBuilder: TStringBuilder;
  LWriteValue: string;
  LNamedElement: TCsvNamedElement;
begin
  Result := '';

  LBuilder := TStringBuilder.Create;
  try
    if (FirstLineIsColumns) and (FRoot.LineCount > 0) then
    begin
      for LColumn := 0 to FRoot.ColumnCount - 1 do
      begin
        if (LColumn <> 0) then
          LBuilder.Append(Delimiter);

        LNamedElement := FRoot.GetElement(0, LColumn) as TCsvNamedElement;
        LWriteValue := LNamedElement.Name;
        if WriterUseQuotes then
        begin
          LWriteValue := AnsiQuotedStr(LWriteValue, QuoteChar);
        end;

        LBuilder.Append(LWriteValue);
      end;
      LBuilder.AppendLine;
    end;

    for LRow := 0 to FRoot.LineCount - 1 do
    begin
      for LColumn := 0 to FRoot.ColumnCount - 1 do
      begin
        if (LColumn <> 0) then
          LBuilder.Append(Delimiter);

        LWriteValue := FRoot.GetElementValue(LRow, LColumn);
        if WriterUseQuotes then
        begin
          LWriteValue := AnsiQuotedStr(LWriteValue, QuoteChar);
        end;

        LBuilder.Append(LWriteValue);
      end;
      //newline
      LBuilder.AppendLine;
    end;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;


{ TCsvNamedElement }

constructor TCsvNamedElement.Create(const AName: string);
begin
  inherited Create();
  FName := AName;
end;

destructor TCsvNamedElement.Destroy;
begin
  inherited Destroy;
end;

function TCsvNamedElement.GetElementValue: TCsvBaseElement;
begin
  Result := TCsvBaseElement(Self);
end;

{ TCsvObjectElement }

procedure TCsvObjectElement.AddObject(const AName, AValue: string);
var
  LElement: TCsvNamedElement;
begin
  LElement := TCsvNamedElement.Create(AName);
  LElement.SetValue(AValue);
  FElements.Add(GetKey(AName), LElement);
end;

function TCsvObjectElement.Count: Integer;
begin
  Result := FElements.Count;
end;

constructor TCsvObjectElement.Create;
begin
  inherited Create;
  FElements := TObjectDictionary<string, TCsvNamedElement>.Create([doOwnsValues]);
end;

destructor TCsvObjectElement.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TCsvObjectElement.GetKey(const AName: string): string;
begin
  Result := UpperCase(AName);
end;

function TCsvObjectElement.GetObjectAt(AIndex: Integer): TCsvNamedElement;
var
  LElement: TPair<string,TCsvNamedElement>;
  LIndex: Integer;
begin
  LIndex := 0;
  for LElement in FElements do
  begin
    if LIndex = AIndex then
    begin
      Exit(LElement.Value);
    end;
    Inc(LIndex);
  end;
  Result := nil;
end;

function TCsvObjectElement.GetObjectByName(const AName: string): TCsvNamedElement;
begin
  if not TryGetObjectByName(AName, Result) then
    Result := nil;
end;

function TCsvObjectElement.TryGetObjectByName(const AName: string;
  out AObject: TCsvNamedElement): Boolean;
begin
  Result := FElements.TryGetValue(GetKey(AName), AObject);
end;

{ TCsvBaseElement }

procedure TCsvBaseElement.SetValue(const Value: string);
begin
  FValue := Value;
end;

end.
