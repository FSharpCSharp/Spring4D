unit TestSvCsvSerializer;

interface

uses
  TestFramework
  ,SvCsvSerializer
  ,SvSerializer
  ;

type
  TTestSvCsvSerializer = class(TTestCase)
  private
    FSerializer: TSvCsvSerializer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Serialize_ToString_Without_Headers_Fail();
    procedure Serialize_ToString_WithQuotes_WithColumns();
  end;

  TTestSvSerializerCsvBackend = class(TTestCase)
  private
    FSerializer: TSvSerializer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SerializeEntity();
    procedure SerializeListOfEntities();
    procedure SerializeListOfEntities_WithoutHeader();
    procedure DeserializeListOfEntities();
    procedure DeserializeEntity();
  end;

implementation

uses
  SysUtils
  ,Generics.Collections
  ;

{ TTestSvCsvSerializer }

const
  CSV_DOC1=  'Test;Foo;Bar' +#13#10;
  CSV_DOC1_QUOTES=  '"Test";"Foo";"Bar"' +#13#10;
  CSV_DOC1_QUOTES_COLUMNS =
    '"Column0";"Column1";"Column2"'+#13#10+
    '"Test";"Foo";"Bar"' +#13#10;

type
  TCsvEntity = class
  private
    FName: string;
    FDescription: string;
    FAmount: Integer;
    FDummy: string;
  public
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Amount: Integer read FAmount write FAmount;
    [SvTransient]
    property Dummy: string read FDummy write FDummy;
  end;



procedure TTestSvCsvSerializer.Serialize_ToString_Without_Headers_Fail;
begin
  FSerializer.FirstLineIsColumns := False;
  try
    FSerializer.ParseCsvString(CSV_DOC1);
  except
    on E:Exception do
    begin
      CheckEquals(E.ClassType, ESvCSVSerializerException);
    end;
  end;
end;

procedure TTestSvCsvSerializer.Serialize_ToString_WithQuotes_WithColumns;
var
  LOutput: string;
begin
  FSerializer.WriterUseQuotes := True;
  FSerializer.FirstLineIsColumns := True;
  FSerializer.ParseCsvString(CSV_DOC1_QUOTES_COLUMNS);
  LOutput := FSerializer.ToString();
  CheckTrue(Length(LOutput) > 0);
  CheckEqualsString(CSV_DOC1_QUOTES_COLUMNS, LOutput);
  CheckEquals(3, FSerializer.Root.ColumnCount);
  CheckEquals(1, FSerializer.Root.LineCount);
end;

procedure TTestSvCsvSerializer.Setup;
begin
  inherited;
  FSerializer := TSvCsvSerializer.Create;
  FSerializer.Delimiter := ';';
end;

procedure TTestSvCsvSerializer.TearDown;
begin
  inherited;
  FSerializer.Free;
end;

{ TTestSvSerializerCsvBackend }

const
  CSV_ENTITY = '"Name";"Description";"Amount"' + #13#10 +
               '"Foo";"Bar";"100"' + #13#10
  ;

  CSV_ENTITIES = '"Name";"Description";"Amount"' + #13#10 +
               '"Foo";"Bar";"100"' + #13#10 +
               '"Second";"Test";"25"' + #13#10
  ;

  CSV_ENTITIES_WITHOUT_HEADER_AND_QUOTES = 'Foo;Bar;100' + #13#10 +
                                'Second;Test;25' + #13#10
  ;


procedure TTestSvSerializerCsvBackend.DeserializeEntity;
var
  LEntity: TCsvEntity;
begin
  LEntity := TCsvEntity.Create;
  try
    FSerializer.CsvDelimiter := ';';
    FSerializer.CsvQuoteChar := '"';
    FSerializer.CsvFirstLineColumns := True;
    FSerializer.CsvWriterUseQuotes := True;

    FSerializer.AddObject('', LEntity);
    FSerializer.DeSerialize(CSV_ENTITY, TEncoding.UTF8);

    CheckEquals('Foo', LEntity.Name);
    CheckEquals('Bar', LEntity.Description);
    CheckEquals(100, LEntity.Amount);
  finally
    LEntity.Free;
  end;
end;

procedure TTestSvSerializerCsvBackend.DeserializeListOfEntities;
var
  LEntities: TObjectList<TCsvEntity>;
  LFirst, LSecond: TCsvEntity;
begin
  LEntities := TObjectList<TCsvEntity>.Create(True);
  try
    FSerializer.CsvDelimiter := ';';
    FSerializer.CsvQuoteChar := '"';
    FSerializer.CsvFirstLineColumns := True;
    FSerializer.CsvWriterUseQuotes := True;

    FSerializer.AddObject('', LEntities);
    CheckEquals(0, LEntities.Count);

    FSerializer.DeSerialize(CSV_ENTITIES, TEncoding.UTF8);
    CheckEquals(2, LEntities.Count);
    LFirst := LEntities.First;
    LSecond := LEntities.Last;

    CheckEquals('Foo', LFirst.Name);
    CheckEquals('Bar', LFirst.Description);
    CheckEquals(100, LFirst.Amount);

    CheckEquals('Second', LSecond.Name);
    CheckEquals('Test', LSecond.Description);
    CheckEquals(25, LSecond.Amount);
  finally
    LEntities.Free;
  end;
end;

procedure TTestSvSerializerCsvBackend.SerializeEntity;
var
  LEntity: TCsvEntity;
  LOutputString: string;
begin
  LEntity := TCsvEntity.Create;
  try
    LEntity.Name := 'Foo';
    LEntity.Description := 'Bar';
    LEntity.Amount := 100;

    FSerializer.CsvDelimiter := ';';
    FSerializer.CsvQuoteChar := '"';
    FSerializer.CsvFirstLineColumns := True;
    FSerializer.CsvWriterUseQuotes := True;

    FSerializer.AddObject('', LEntity);

    FSerializer.Serialize(LOutputString, TEncoding.UTF8);
    CheckEquals(CSV_ENTITY, LOutputString);
  finally
    LEntity.Free;
  end;
end;

procedure TTestSvSerializerCsvBackend.SerializeListOfEntities;
var
  LEntities: TObjectList<TCsvEntity>;
  LEntity: TCsvEntity;
  LOutputString: string;
begin
  LEntities := TObjectList<TCsvEntity>.Create(True);
  try
    LEntity := TCsvEntity.Create;
    LEntity.Name := 'Foo';
    LEntity.Description := 'Bar';
    LEntity.Amount := 100;
    LEntities.Add(LEntity);

    LEntity := TCsvEntity.Create;
    LEntity.Name := 'Second';
    LEntity.Description := 'Test';
    LEntity.Amount := 25;
    LEntities.Add(LEntity);

    FSerializer.CsvDelimiter := ';';
    FSerializer.CsvQuoteChar := '"';
    FSerializer.CsvFirstLineColumns := True;
    FSerializer.CsvWriterUseQuotes := True;

    FSerializer.AddObject('', LEntities);
    FSerializer.Serialize(LOutputString, TEncoding.UTF8);
    CheckEquals(CSV_ENTITIES, LOutputString);
  finally
    LEntities.Free;
  end;
end;

procedure TTestSvSerializerCsvBackend.SerializeListOfEntities_WithoutHeader;
var
  LEntities: TObjectList<TCsvEntity>;
  LEntity: TCsvEntity;
  LOutputString: string;
begin
  LEntities := TObjectList<TCsvEntity>.Create(True);
  try
    LEntity := TCsvEntity.Create;
    LEntity.Name := 'Foo';
    LEntity.Description := 'Bar';
    LEntity.Amount := 100;
    LEntities.Add(LEntity);

    LEntity := TCsvEntity.Create;
    LEntity.Name := 'Second';
    LEntity.Description := 'Test';
    LEntity.Amount := 25;
    LEntities.Add(LEntity);

    FSerializer.CsvDelimiter := ';';
    FSerializer.CsvQuoteChar := '"';
    FSerializer.CsvFirstLineColumns := False;
    FSerializer.CsvWriterUseQuotes := False;

    FSerializer.AddObject('', LEntities);
    FSerializer.Serialize(LOutputString, TEncoding.UTF8);
    CheckEquals(CSV_ENTITIES_WITHOUT_HEADER_AND_QUOTES, LOutputString);
  finally
    LEntities.Free;
  end;
end;

procedure TTestSvSerializerCsvBackend.SetUp;
begin
  inherited;
  FSerializer := TSvSerializer.Create(sstCSV);
end;

procedure TTestSvSerializerCsvBackend.TearDown;
begin
  inherited;
  FSerializer.Free;
end;

initialization
  RegisterTest(TTestSvCsvSerializer.Suite);
  RegisterTest(TTestSvSerializerCsvBackend.Suite);

end.
