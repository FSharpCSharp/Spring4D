unit SvDB;

interface

uses
  Classes
  ,Generics.Collections
  ,SysUtils
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents dynamic SQL Builder.
  ///	</summary>
  ///	<example>
  ///	  <para>
  ///	    Building <b>select</b> statements:
  ///	  </para>
  ///	  <code lang="Delphi">
  ///	LSQLString := TSQLBuilder.Select.
  ///	  .Top(100)
  ///	  .Column('C.FIRSTNAME')
  ///	  .Column('C.LASTNAME')
  ///	  .From('dbo.Customers C')
  ///	  .Join('dbo.Details D on D.ID=C.ID')
  ///	  .ToString;</code>
  ///	  <para>
  ///	    Building <b>update</b> statements:
  ///	  </para>
  ///	  <code lang="Delphi">
  ///	LSQLString := TSQLBuilder.Update
  ///	  .Table('dbo.Customers')
  ///	  .Column('AGE').Values('18')
  ///	  .Column('NAME').Values('Null')
  ///	  .ToString();</code>
  ///	  Building <b>insert</b> statements:
  ///	  <code lang="Delphi">
  ///	LSQLString := TSQLBuilder.Insert
  ///	  .Into('dbo.Customers')
  ///	  .Values('18')
  ///	  .Values('Bob')
  ///	  .ToString();</code>
  ///	  Building <b>delete</b> statements:
  ///	  <code lang="Delphi">
  ///	LSQLString := TSQLBuilder.Delete
  ///	  .From('dbo.Customers c')
  ///	  .ToString();</code>
  ///	</example>
  {$ENDREGION}
  ISQLBuilder = interface(IInvokable)

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Builds sql statement and returns it as string.
    ///	</summary>
    {$ENDREGION}
    function ToString(): string;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <b>select</b> sql statement.
    ///	</summary>
    {$ENDREGION}
    function Select(): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <b>delete</b> sql statement.
    ///	</summary>
    {$ENDREGION}
    function Delete(): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <b>insert</b> sql statement.
    ///	</summary>
    {$ENDREGION}
    function Insert(): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <b>update</b> sql statement.
    ///	</summary>
    {$ENDREGION}
    function Update(): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds <i>Column</i> definition for sql statement. Can be used in
    ///	  <b>select</b>, <b>update</b> and <b>insert</b> statements.
    ///	</summary>
    {$ENDREGION}
    function Column(const AColumnName: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents the main <i>table name</i> which will be used in
    ///	  <b>from</b> part of the statement. Can be used in <b>select</b>,
    ///	  <b>update</b>, <b>delete</b> statements.
    ///	</summary>
    {$ENDREGION}
    function From(const ATableName: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents joined table criteria.
    ///	</summary>
    {$ENDREGION}
    function Join(const AJoinCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents left outer joined table criteria.
    ///	</summary>
    {$ENDREGION}
    function LeftOuterJoin(const AJoinCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents right outer joined table criteria.
    ///	</summary>
    {$ENDREGION}
    function RightOuterJoin(const AJoinCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <b>where</b> criteria. In case it is defined multiple
    ///	  times, all the criterias will be separated by <i>AND</i> identifier.
    ///	</summary>
    {$ENDREGION}
    function Where(const ACriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>group by</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function GroupBy(const AGroupByCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>having</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function Having(const AHavingCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>order by</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function OrderBy(const AOrderByCriteria: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>top</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function Top(ACount: Integer): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>union</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function Union(const AUnionSQL: string): ISQLBuilder; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>union all</i> part of the sql statement.
    ///	</summary>
    {$ENDREGION}
    function UnionAll(const AUnionSQL: string): ISQLBuilder; overload;

    function Union(const AUnionSQL: ISQLBuilder): ISQLBuilder; overload;

    function UnionAll(const AUnionSQL: ISQLBuilder): ISQLBuilder; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>into</i> part of the sql statement. Can be used in <b>insert</b> statements.
    ///	</summary>
    {$ENDREGION}
    function Into(const ATableName: string): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>values</i> part of the sql statement. Can be used in <b>insert</b>, <b>update</b> statements.
    ///	</summary>
    {$ENDREGION}
    function Values(const AValue: string; AIncludeQuotes: Boolean = True): ISQLBuilder;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents <i>table</i> used in <b>update</b> sql statement.
    ///	</summary>
    {$ENDREGION}
    function Table(const ATablename: string): ISQLBuilder;
  end;

  TAnsiSQLBuilder = class;

  TJoinType = (jtNone, jtInner, jtLeftOuter, jtRightOuter);

  TSQLUnionType = (utUnion, utUnionAll);

  TSQLValue = class
  private
    FValue: string;
    FIncludeQuotes: Boolean;
    function GetValue: string;
  public
    constructor Create(const AValue: string; AIncludeQuotes: Boolean = True); virtual;

    property Value: string read GetValue;
  end;

  TSQLTable = class
  private
    FTablename: string;
    FJoinType: TJoinType;
  public
    constructor Create(const ATablename: string; AJoinType: TJoinType = jtNone); virtual;

    function ToString(): string; override;
  end;

  TSQLTop = class
  private
    FEnabled: Boolean;
    FCount: Integer;
  public
    constructor Create(); virtual;
  end;

  TSQLUnion = class
  private
    FUnionType: TSQLUnionType;
    FUnionSQL: string;
  public
    constructor Create(AUnionType: TSQLUnionType; const AUnionSQL: string); virtual;

    function ToString(): string; override;
  end;

  TSQLStatementType = (stSelect, stInsert, stUpdate, stDelete);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base SQL statement type.
  ///	</summary>
  {$ENDREGION}
  TSQLStatement = class
  private
    FOwner: TAnsiSQLBuilder;
  protected
    procedure AppendWhereClause(ABuilder: TStringBuilder); virtual;
    procedure AppendColumns(ABuilder: TStringBuilder); virtual;
    procedure AppendJoinedTables(ABuilder: TStringBuilder); virtual;
    procedure AppendFromClause(ABuilder: TStringBuilder); virtual;
  public
    constructor Create(AOwner: TAnsiSQLBuilder); virtual;

    function ToString(): string; override;

    property Owner: TAnsiSQLBuilder read FOwner;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents select statement.
  ///	</summary>
  {$ENDREGION}
  TSelectStatement = class(TSQLStatement)
  public
    function ToString(): string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents delete statement.
  ///	</summary>
  {$ENDREGION}
  TDeleteStatement = class(TSQLStatement)
  public
    function ToString(): string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents insert statement.
  ///	</summary>
  {$ENDREGION}
  TInsertStatement = class(TSQLStatement)
  protected
    procedure AppendColumns(ABuilder: TStringBuilder); override;
    procedure AppendValues(ABuilder: TStringBuilder); virtual;
  public
    function ToString(): string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents update statement.
  ///	</summary>
  {$ENDREGION}
  TUpdateStatement = class(TSQLStatement)
  protected
    procedure AppendColumns(ABuilder: TStringBuilder); override;
    procedure AppendFromClause(ABuilder: TStringBuilder); override;
  public
    function ToString(): string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base SQL builder.
  ///	</summary>
  {$ENDREGION}
  TAnsiSQLBuilder = class(TInterfacedObject, ISQLBuilder)
  private
    FSQLStmtType: TSQLStatementType;
    FColumns: TStringList;
    FTable: TSQLTable;
    FJoinedTables: TObjectList<TSQLTable>;
    FWhereCriterias: TStringList;
    FGroupByCriterias: TStringList;
    FHavingCriterias: TStringList;
    FOrderByCriterias: TStringList;
    FValues: TObjectList<TSQLValue>;
    FTop: TSQLTop;
    FUnions: TObjectList<TSQLUnion>;
    FFromTable: TSQLTable;
  protected
    function DoBuildSQL(AStatement: TSQLStatement): string; virtual;

    procedure AppendTop(ABuilder: TStringBuilder); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function ToString(): string; override;

    function Select(): ISQLBuilder; virtual;
    function Delete(): ISQLBuilder; virtual;
    function Insert(): ISQLBuilder; virtual;
    function Update(): ISQLBuilder; virtual;
    function Column(const AColumnName: string): ISQLBuilder; virtual;
    function From(const ATableName: string): ISQLBuilder; virtual;
    function Join(const AJoinCriteria: string): ISQLBuilder; virtual;
    function LeftOuterJoin(const AJoinCriteria: string): ISQLBuilder; virtual;
    function RightOuterJoin(const AJoinCriteria: string): ISQLBuilder; virtual;
    function Where(const ACriteria: string): ISQLBuilder; virtual;
    function GroupBy(const AGroupByCriteria: string): ISQLBuilder; virtual;
    function Having(const AHavingCriteria: string): ISQLBuilder; virtual;
    function OrderBy(const AOrderByCriteria: string): ISQLBuilder; virtual;
    function Top(ACount: Integer): ISQLBuilder; virtual;
    function Union(const AUnionSQL: string): ISQLBuilder; overload; virtual;
    function UnionAll(const AUnionSQL: string): ISQLBuilder; overload; virtual;
    function Union(const AUnionSQL: ISQLBuilder): ISQLBuilder; overload; virtual;
    function UnionAll(const AUnionSQL: ISQLBuilder): ISQLBuilder; overload; virtual;
    function Into(const ATableName: string): ISQLBuilder; virtual;
    function Values(const AValue: string; AIncludeQuotes: Boolean = True): ISQLBuilder; virtual;
    function Table(const ATablename: string): ISQLBuilder; virtual;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Transact-SQL (Microsoft SQL Server) builder.
  ///	</summary>
  {$ENDREGION}
  TTransactSQLBuilder = class(TAnsiSQLBuilder)
  protected
    procedure AppendTop(ABuilder: TStringBuilder); override;
  end;


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Constructs Ansi-SQL builder interface.
  ///	</summary>
  {$ENDREGION}
  function AnsiSQLBuilder(): ISQLBuilder;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Constructs Transact-SQL builder interface.
  ///	</summary>
  {$ENDREGION}
  function TSQLBuilder(): ISQLBuilder;


implementation

uses
  TypInfo
  ;

type
  EAnsiSQLBuilderException = class(Exception);


function AnsiSQLBuilder(): ISQLBuilder;
begin
  Result := TAnsiSQLBuilder.Create();
end;

function TSQLBuilder(): ISQLBuilder;
begin
  Result := TTransactSQLBuilder.Create;
end;

function GetSQLString(const AValue: string): string;
begin
  Result := AValue;
  if not SameText(AValue, 'NULL') then
  begin
    Result := QuotedStr(AValue);
  end;
end;


{ TAnsiSQLBuilder }

procedure TAnsiSQLBuilder.AppendTop(ABuilder: TStringBuilder);
begin
  //do nothing
end;

function TAnsiSQLBuilder.Column(const AColumnName: string): ISQLBuilder;
begin
  FColumns.Add(AColumnName);
  Result := Self;
end;

constructor TAnsiSQLBuilder.Create;
begin
  inherited Create;
  FSQLStmtType := stSelect;
  FColumns := TStringList.Create;
  FColumns.Delimiter := ',';
  FColumns.StrictDelimiter := True;
  FTable := TSQLTable.Create('');
  FJoinedTables := TObjectList<TSQLTable>.Create(True);
  FGroupByCriterias := TStringList.Create;
  FGroupByCriterias.Delimiter := ',';
  FGroupByCriterias.StrictDelimiter := True;
  FHavingCriterias := TStringList.Create;
  FWhereCriterias := TStringList.Create;
  FValues := TObjectList<TSQLValue>.Create(True);
  FOrderByCriterias := TStringList.Create;
  FOrderByCriterias.Delimiter := ',';
  FOrderByCriterias.StrictDelimiter := True;
  FTop := TSQLTop.Create;
  FUnions := TObjectList<TSQLUnion>.Create(True);
  FFromTable := TSQLTable.Create('');
end;

function TAnsiSQLBuilder.Delete: ISQLBuilder;
begin
  FSQLStmtType := stDelete;
  Result := Self;
end;

destructor TAnsiSQLBuilder.Destroy;
begin
  FColumns.Free;
  FTable.Free;
  FJoinedTables.Free;
  FGroupByCriterias.Free;
  FHavingCriterias.Free;
  FWhereCriterias.Free;
  FValues.Free;
  FOrderByCriterias.Free;
  FTop.Free;
  FUnions.Free;
  FFromTable.Free;
  inherited Destroy;
end;

function TAnsiSQLBuilder.DoBuildSQL(AStatement: TSQLStatement): string;
begin
  Assert(Assigned(AStatement));
  Result := AStatement.ToString;
end;

function TAnsiSQLBuilder.From(const ATableName: string): ISQLBuilder;
begin
  FFromTable.FTablename := ATableName;
  Result := Self;
end;

function TAnsiSQLBuilder.GroupBy(const AGroupByCriteria: string): ISQLBuilder;
begin
  FGroupByCriterias.Add(AGroupByCriteria);
  Result := Self;
end;

function TAnsiSQLBuilder.Having(const AHavingCriteria: string): ISQLBuilder;
begin
  FHavingCriterias.Add(AHavingCriteria);
  Result := Self;
end;

function TAnsiSQLBuilder.Insert: ISQLBuilder;
begin
  FSQLStmtType := stInsert;
  Result := Self;
end;

function TAnsiSQLBuilder.Into(const ATableName: string): ISQLBuilder;
begin
  Result := Table(ATableName);
end;

function TAnsiSQLBuilder.Join(const AJoinCriteria: string): ISQLBuilder;
begin
  FJoinedTables.Add(TSQLTable.Create(AJoinCriteria, jtInner));
  Result := Self;
end;

function TAnsiSQLBuilder.LeftOuterJoin(const AJoinCriteria: string): ISQLBuilder;
begin
  FJoinedTables.Add(TSQLTable.Create(AJoinCriteria, jtLeftOuter));
  Result := Self;
end;

function TAnsiSQLBuilder.OrderBy(const AOrderByCriteria: string): ISQLBuilder;
begin
  FOrderByCriterias.Add(AOrderByCriteria);
  Result := Self;
end;

function TAnsiSQLBuilder.RightOuterJoin(const AJoinCriteria: string): ISQLBuilder;
begin
  FJoinedTables.Add(TSQLTable.Create(AJoinCriteria, jtRightOuter));
  Result := Self;
end;

function TAnsiSQLBuilder.Select: ISQLBuilder;
begin
  FSQLStmtType := stSelect;
  Result := Self;
end;

function TAnsiSQLBuilder.Table(const ATablename: string): ISQLBuilder;
begin
  FTable.FTablename := ATableName;
  Result := Self;
end;

function TAnsiSQLBuilder.Top(ACount: Integer): ISQLBuilder;
begin
  FTop.FEnabled := True;
  FTop.FCount := ACount;
  Result := Self;
end;

function TAnsiSQLBuilder.ToString: string;
var
  LStatement: TSQLStatement;
begin
  Result := '';

  case FSQLStmtType of
    stSelect: LStatement := TSelectStatement.Create(Self);
    stInsert: LStatement := TInsertStatement.Create(Self);
    stUpdate: LStatement := TUpdateStatement.Create(Self);
    stDelete: LStatement := TDeleteStatement.Create(Self);
    else
    begin
      raise EAnsiSQLBuilderException.CreateFmt('The statement (%S) is not implemented'
        , [GetEnumName(TypeInfo(TSQLStatementType), Ord(FSQLStmtType))]);
    end;
  end;

  try
    Result := DoBuildSQL(LStatement);
  finally
    LStatement.Free;
  end;
end;

function TAnsiSQLBuilder.Union(const AUnionSQL: string): ISQLBuilder;
begin
  FUnions.Add(TSQLUnion.Create(utUnion, AUnionSQL));
  Result := Self;
end;

function TAnsiSQLBuilder.UnionAll(const AUnionSQL: string): ISQLBuilder;
begin
  FUnions.Add(TSQLUnion.Create(utUnionAll, AUnionSQL));
  Result := Self;
end;

function TAnsiSQLBuilder.Where(const ACriteria: string): ISQLBuilder;
begin
  FWhereCriterias.Add(ACriteria);
  Result := Self;
end;

function TAnsiSQLBuilder.Union(const AUnionSQL: ISQLBuilder): ISQLBuilder;
begin
  Result := Union(AUnionSQL.ToString);
end;

function TAnsiSQLBuilder.UnionAll(const AUnionSQL: ISQLBuilder): ISQLBuilder;
begin
  Result := UnionAll(AUnionSQL.ToString);
end;

function TAnsiSQLBuilder.Update: ISQLBuilder;
begin
  FSQLStmtType := stUpdate;
  Result := Self;
end;

function TAnsiSQLBuilder.Values(const AValue: string; AIncludeQuotes: Boolean): ISQLBuilder;
begin
  FValues.Add(TSQLValue.Create(AValue, AIncludeQuotes));
  Result := Self;
end;

{ TSQLStatement }

constructor TSQLStatement.Create(AOwner: TAnsiSQLBuilder);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TSQLStatement.AppendColumns(ABuilder: TStringBuilder);
var
  i: Integer;
begin
  for i := 0 to Owner.FColumns.Count - 1 do
  begin
    if i = 0 then
      ABuilder.AppendLine
    else
      ABuilder.Append(',');

    ABuilder.Append(Owner.FColumns[i]);
  end;
end;

procedure TSQLStatement.AppendFromClause(ABuilder: TStringBuilder);
begin
  ABuilder.AppendLine.Append(' FROM ' + Owner.FFromTable.ToString);
end;

procedure TSQLStatement.AppendJoinedTables(ABuilder: TStringBuilder);
var
  i: Integer;
begin
  for i := 0 to FOwner.FJoinedTables.Count - 1 do
  begin
    ABuilder.AppendLine.Append(' ' + FOwner.FJoinedTables[i].ToString);
  end;
end;

procedure TSQLStatement.AppendWhereClause(ABuilder: TStringBuilder);
var
  i: Integer;
begin
  for i := 0 to FOwner.FWhereCriterias.Count - 1 do
  begin
    if i = 0 then
      ABuilder.AppendLine.Append(' WHERE ')
    else
      ABuilder.Append(' AND ');

    ABuilder.Append('(' + FOwner.FWhereCriterias[i] + ')');
  end;
end;

function TSQLStatement.ToString: string;
begin
  Result := '';
end;

{ TSelectStatement }

function TSelectStatement.ToString: string;
var
  i: Integer;
  LBuilder: TStringBuilder;
begin
  Result := '';
  if (Owner.FColumns.Count < 1) or (Owner.FFromTable.FTablename = '') then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('SELECT ');

    if FOwner.FTop.FEnabled then
      FOwner.AppendTop(LBuilder);

    AppendColumns(LBuilder);

    AppendFromClause(LBuilder);

    AppendJoinedTables(LBuilder);

    AppendWhereClause(LBuilder);

    for i := 0 to Owner.FGroupByCriterias.Count - 1 do
    begin
      if i = 0 then
        LBuilder.AppendLine.Append(' GROUP BY ')
      else
        LBuilder.Append(',');

      LBuilder.Append(Owner.FGroupByCriterias[i]);
    end;

    for i := 0 to FOwner.FHavingCriterias.Count - 1 do
    begin
      if i = 0 then
        LBuilder.AppendLine.Append(' HAVING ')
      else
        LBuilder.Append(' AND ');

      LBuilder.AppendFormat('(%0:S)', [FOwner.FHavingCriterias[i]]);
    end;


    for i := 0 to Owner.FOrderByCriterias.Count - 1 do
    begin
      if i = 0 then
        LBuilder.AppendLine.Append(' ORDER BY ')
      else
        LBuilder.Append(',');

      LBuilder.Append(Owner.FOrderByCriterias[i]);
    end;

    for i := 0 to FOwner.FUnions.Count - 1 do
    begin
      LBuilder.AppendLine.Append(FOwner.FUnions[i].ToString);
    end;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

{ TSQLTable }

constructor TSQLTable.Create(const ATablename: string; AJoinType: TJoinType);
begin
  inherited Create();
  FTablename := ATablename;
  FJoinType := AJoinType;
end;

function TSQLTable.ToString: string;
begin
  Result := FTablename;
  case FJoinType of
    jtNone: Result := FTablename;
    jtInner: Result := 'JOIN ' + FTablename;
    jtLeftOuter: Result := 'LEFT OUTER JOIN ' + FTablename;
    jtRightOuter: Result := 'RIGHT OUTER JOIN ' + FTablename;
  end;
end;

{ TSQLTop }

constructor TSQLTop.Create;
begin
  inherited Create;
  FEnabled := False;
  FCount := 0;
end;

{ TTransactSQLBuilder }

procedure TTransactSQLBuilder.AppendTop(ABuilder: TStringBuilder);
begin
  if FTop.FEnabled then
    ABuilder.Append('TOP ' + IntToStr(FTop.FCount) + ' ');
end;

{ TSQLUnion }

constructor TSQLUnion.Create(AUnionType: TSQLUnionType; const AUnionSQL: string);
begin
  inherited Create;
  FUnionType := AUnionType;
  FUnionSQL := AUnionSQL;
end;

function TSQLUnion.ToString: string;
begin
  case FUnionType of
    utUnion: Result := 'UNION';
    utUnionAll: Result := 'UNION ALL';
  end;

  Result := Result + #13#10 + FUnionSQL;
end;

{ TDeleteStatement }

function TDeleteStatement.ToString: string;
var
  LBuilder: TStringBuilder;
begin
  Result := '';
  if (Owner.FFromTable.FTablename = '') then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('DELETE FROM ').Append(Owner.FFromTable.FTablename);

    AppendWhereClause(LBuilder);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

{ TInsertStatement }

procedure TInsertStatement.AppendColumns(ABuilder: TStringBuilder);
begin
  if Owner.FColumns.Count > 0 then
    ABuilder.AppendLine.Append(' (');

  inherited AppendColumns(ABuilder);

  if Owner.FColumns.Count > 0 then
    ABuilder.AppendLine.Append(' )');
end;

procedure TInsertStatement.AppendValues(ABuilder: TStringBuilder);
var
  i: Integer;
begin
  for i := 0 to Owner.FValues.Count - 1 do
  begin
    if i = 0 then
      ABuilder.AppendLine.Append(' VALUES').AppendLine.Append(' (')
    else
      ABuilder.Append(',');

    ABuilder.Append(Owner.FValues[i].Value);
  end;

  if Owner.FValues.Count > 0 then
    ABuilder.Append(')');
end;

function TInsertStatement.ToString: string;
var
  LBuilder: TStringBuilder;
begin
  Result := '';
  if (Owner.FTable.FTablename = '') then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('INSERT INTO ').Append(Owner.FTable.FTablename);

    AppendColumns(LBuilder);

    AppendValues(LBuilder);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

{ TUpdateStatement }

procedure TUpdateStatement.AppendColumns(ABuilder: TStringBuilder);
var
  i: Integer;
begin
  Assert(Owner.FColumns.Count = Owner.FValues.Count, 'Columns count and Values count must be equal');
  for i := 0 to Owner.FColumns.Count - 1 do
  begin
    if i = 0 then
      ABuilder.AppendLine.Append(' ')
    else
      ABuilder.AppendLine.Append(' ,');

    ABuilder.AppendFormat('%0:S=%1:S',
      [
        Owner.FColumns[i]
        ,Owner.FValues[i].Value
      ]);
  end;
end;

procedure TUpdateStatement.AppendFromClause(ABuilder: TStringBuilder);
begin
  if (Owner.FFromTable.FTablename = '') then
    Exit;

  ABuilder.AppendLine.Append(' FROM ').Append(Owner.FFromTable.FTablename);
end;

function TUpdateStatement.ToString: string;
var
  LBuilder: TStringBuilder;
begin
  Result := '';

  if (Owner.FTable.FTablename = '') then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('UPDATE ').Append(Owner.FTable.FTablename).AppendLine.Append(' SET');

    AppendColumns(LBuilder);

    AppendFromClause(LBuilder);

    AppendJoinedTables(LBuilder);

    AppendWhereClause(LBuilder);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

{ TSQLValue }

constructor TSQLValue.Create(const AValue: string; AIncludeQuotes: Boolean);
begin
  inherited Create();
  FValue := AValue;
  FIncludeQuotes := AIncludeQuotes;
end;

function TSQLValue.GetValue: string;
begin
  Result := FValue;
  if FIncludeQuotes then
    Result := GetSQLString(Result);
end;

end.
