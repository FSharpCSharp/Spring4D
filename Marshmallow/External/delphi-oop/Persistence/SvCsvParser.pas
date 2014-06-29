unit SvCsvParser;

interface

uses
  Classes
  ;

type
  TSvCsvParser = class
  private
    FData : array of array of string;
    FDelim : Char;
    FQuote : Char;
    function GetRows : Integer;
    function GetCols : Integer;
    function GetCell( Row, Col : Integer ) : string;
    procedure SetCell( Row, Col : Integer; const Value : string );
  protected
    procedure DoParse( AData: TStrings );
  public
    destructor Destroy; override;

    procedure Parse(const ACsvString: string);
    procedure ParseFromFilename(const AFilename: string);
    procedure ParseFromStream(AStream: TStream);

    property Cell[Row, Col : Integer] : string read GetCell write SetCell;
    property Rows : Integer read GetRows;
    property Cols : Integer read GetCols;
    property Delim : Char read FDelim write FDelim;
    property Quote : Char read FQuote write FQuote;
  end;

implementation

{ TSvCsvParser }

destructor TSvCsvParser.Destroy;
begin
  SetLength( FData, 0, 0 );
  inherited;
end;

function TSvCsvParser.GetCell( Row, Col : Integer ) : string;
begin
  Result := FData[Row, Col];
end;

function TSvCsvParser.GetCols : Integer;
begin
  if Rows > 0
  then
    Result := Length( FData[0] )
  else
    Result := 0;
end;

function TSvCsvParser.GetRows : Integer;
begin
  Result := Length( FData );
end;

procedure TSvCsvParser.DoParse( AData: TStrings );
var
  Val : string;
  MyChar : Char;
  LastChar : Char;
  QuotePart : Boolean;
  Col : Integer;
  Row : Integer;
  MaxCol : Integer;
begin
  LastChar := #0;
  QuotePart := False;
  Val := '';
  MaxCol := 0;
  Col := 0;
  Row := 0;

  for MyChar in AData.Text do
  begin
    if ( MyChar = Quote )  then
    begin
      QuotePart := not QuotePart;

      if QuotePart and ( LastChar = Quote ) then
        Val := Val + Quote;
    end
    else if not QuotePart and ( MyChar = Delim ) then
    begin
      if high( FData ) < Row + 1 then
        SetLength( FData, Row + 10 );

      if high( FData[Row] ) < Col + 1 then
        SetLength( FData[Row], Col + 10 );

      FData[Row, Col] := Val;
      Val := '';
      Inc( Col );
    end
    else if not QuotePart and ( ( MyChar = #13 ) or ( MyChar = #10 ) ) then
    begin
      if ( MyChar = #10 ) and ( LastChar = #13 ) then
      begin
        if high( FData ) < Row + 1 then
          SetLength( FData, Row + 10 );

        SetLength( FData[Row], Col + 1 );

        if Col > MaxCol then
          MaxCol := Col;

        FData[Row, Col] := Val;
        Val := '';
        Inc( Row );
        Col := 0;
      end;
    end
    else
      Val := Val + MyChar;

    LastChar := MyChar;
  end;
  SetLength( FData, Row );

  for Row := low( FData ) to high( FData ) do
    SetLength( FData[Row], MaxCol + 1 );
end;

procedure TSvCsvParser.Parse(const ACsvString: string);
var
  LData: TStrings;
begin
  LData := TStringList.Create;
  try
    LData.Text := ACsvString;
    DoParse(LData);
  finally
    LData.Free;
  end;
end;

procedure TSvCsvParser.ParseFromFilename(const AFilename: string);
var
  LData: TStrings;
begin
  LData := TStringList.Create;
  try
    LData.LoadFromFile(AFilename);
    DoParse(LData);
  finally
    LData.Free;
  end;
end;

procedure TSvCsvParser.ParseFromStream(AStream: TStream);
var
  LData: TStrings;
begin
  LData := TStringList.Create;
  try
    LData.LoadFromStream(AStream);
    DoParse(LData);
  finally
    LData.Free;
  end;
end;

procedure TSvCsvParser.SetCell( Row, Col : Integer; const Value : string );
begin
  FData[Row, Col] := Value;
end;

end.
