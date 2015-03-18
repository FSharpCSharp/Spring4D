unit TestAnsiSQLGenerator;

interface

uses
  TestFramework, Spring.Persistence.SQL.Generators.Abstract
  , Spring.Persistence.SQL.Generators.Ansi, Spring.Persistence.SQL.Commands
  , Spring.Persistence.SQL.Types, Spring.Collections;

type
  TAnsiSQLGeneratorTest = class(TTestCase)
  private
    FAnsiSQLGenerator: TAnsiSQLGenerator;
  protected
    procedure CheckEqualsSQL(const AExpected, ASQL: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateSelect;
    procedure TestGenerateInsert;
    procedure TestGenerateUpdate;
    procedure TestGenerateDelete;
    procedure TestGenerateCreateTable;
    procedure TestGenerateCreateFK;
    procedure TestGenerateCreateSequence;
    procedure TestGenerateGetNextSequenceValue;
    procedure TestGenerateGetLastInsertId;
    procedure TestGeneratePagedQuery;
    procedure TestGenerateGetQueryCount;
  end;

implementation

uses
  SysUtils,
  StrUtils
  ,TestEntities
  ,Spring.Persistence.Mapping.RttiExplorer
  ,Spring.Persistence.Mapping.Attributes
  ,Generics.Collections
  ;

function CreateTestTable: TSQLTable;
begin
  Result := TSQLTable.Create;
  Result.Schema := 'TEST';
  Result.Name := 'CUSTOMERS';
  Result.Description := 'Customers table';
//  Result.Alias := 'C';
end;

function CreateTestJoinTable: TSQLTable;
begin
  Result := TSQLTable.Create;
  Result.Schema := 'TEST';
  Result.Name := 'PRODUCTS';
  Result.Description := 'Products table';
 // Result.Alias := 'P';
end;

function CreateTestCOUNTRYTable: TSQLTable;
begin
  Result := TSQLTable.Create;
  Result.Schema := 'TEST';
  Result.Name := 'COUNTRIES';
  Result.Description := 'Countries table';
 // Result.Alias := 'P';
end;

procedure TAnsiSQLGeneratorTest.CheckEqualsSQL(const AExpected, ASQL: string);
var
  LExpected, LSQL: string;
begin
  LExpected := ReplaceStr(AExpected, ' ', '');
  LExpected := ReplaceStr(LExpected, sLineBreak, '');

  LSQL := ReplaceStr(ASQL, ' ', '');
  LSQL := ReplaceStr(LSQL, sLineBreak, '');
  CheckEqualsString(LExpected, LSQL, 'SQL not equals');
end;

procedure TAnsiSQLGeneratorTest.SetUp;
begin
  FAnsiSQLGenerator := TAnsiSQLGenerator.Create;
end;

procedure TAnsiSQLGeneratorTest.TearDown;
begin
  FAnsiSQLGenerator.Free;
  FAnsiSQLGenerator := nil;
end;


const
  SQL_SELECT_TEST_SIMPLE = 'SELECT A."NAME",A."AGE",A."HEIGHT"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A;';

  SQL_SELECT_TEST_JOIN = 'SELECT A."NAME",A."AGE",A."HEIGHT"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+
    ';';

  SQL_SELECT_TEST_JOIN_2 = 'SELECT A."NAME",A."AGE",A."HEIGHT",C."COUNTRYNAME"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+sLineBreak+
    '  LEFT OUTER JOIN TEST.COUNTRIES C ON C."ID"=A."COUNTRYID"'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER = 'SELECT A."NAME",A."AGE",A."HEIGHT",C."COUNTRYNAME"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+sLineBreak+
    '  LEFT OUTER JOIN TEST.COUNTRIES C ON C."ID"=A."COUNTRYID"'+sLineBreak+
    '  ORDER BY A."AGE" DESC'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_MULTIPLE = 'SELECT A."NAME",A."AGE",A."HEIGHT",C."COUNTRYNAME"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+sLineBreak+
    '  LEFT OUTER JOIN TEST.COUNTRIES C ON C."ID"=A."COUNTRYID"'+sLineBreak+
    '  ORDER BY A."AGE" DESC,C."COUNTRYNAME" ASC'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_GROUP = 'SELECT A."NAME",A."AGE",A."HEIGHT",C."COUNTRYNAME"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+sLineBreak+
    '  LEFT OUTER JOIN TEST.COUNTRIES C ON C."ID"=A."COUNTRYID"'+sLineBreak+
    '  GROUP BY A."HEIGHT",A."NAME",A."AGE",C."COUNTRYNAME"'+sLineBreak+
    '  ORDER BY A."AGE" DESC,C."COUNTRYNAME" ASC'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_GROUP_WHERE = 'SELECT A."NAME",A."AGE",A."HEIGHT",C."COUNTRYNAME"'+ sLineBreak +
    ' FROM TEST.CUSTOMERS A' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS B ON B."ID"=A."PRODID"'+sLineBreak+
    '  LEFT OUTER JOIN TEST.COUNTRIES C ON C."ID"=A."COUNTRYID"'+sLineBreak+
    '  WHERE A."NAME" = :NAME'+sLineBreak+
    '  GROUP BY A."HEIGHT",A."NAME",A."AGE",C."COUNTRYNAME"'+sLineBreak+
    '  ORDER BY A."AGE" DESC,C."COUNTRYNAME" ASC'+
    ';';

procedure TAnsiSQLGeneratorTest.TestGenerateSelect;
var
  sSql: string;
  LCommand: TSelectCommand;
  LTable, LJoinTable, LCountriesTable: TSQLTable;
  LJoin: TSQLJoin;
begin
  LTable := CreateTestTable;
  LTable.Alias := 'A';
  LJoinTable := CreateTestJoinTable;
  LJoinTable.Alias := 'B';
  LCountriesTable := CreateTestCOUNTRYTable;
  LCountriesTable.Alias := 'C';
  LCommand := TSelectCommand.Create(LTable);
  try
    LCommand.SelectFields.Add(TSQLSelectField.Create('NAME', LTable));
    LCommand.SelectFields.Add(TSQLSelectField.Create('AGE', LTable));
    LCommand.SelectFields.Add(TSQLSelectField.Create('HEIGHT', LTable));

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_SIMPLE, sSql);

    LJoin := TSQLJoin.Create(jtInner);
    LJoin.Segments.Add
    (
      TSQLJoinSegment.Create
      (
        TSQLField.Create('ID', LJoinTable)
        ,TSQLField.Create('PRODID', LTable)
      )
    );
    LCommand.Joins.Add(LJoin);

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsSQL(SQL_SELECT_TEST_JOIN, sSql);

    LCommand.SelectFields.Add(TSQLSelectField.Create('COUNTRYNAME', LCountriesTable));
    LJoin := TSQLJoin.Create(jtLeft);
    LJoin.Segments.Add
    (
      TSQLJoinSegment.Create
      (
        TSQLField.Create('ID', LCountriesTable)
        ,TSQLField.Create('COUNTRYID', LTable)
      )
    );
    LCommand.Joins.Add(LJoin);

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsSQL(SQL_SELECT_TEST_JOIN_2, sSql);

    LCommand.OrderByFields.Add(TSQLOrderByField.Create('AGE', LTable));
    LCommand.OrderByFields[0].SortingDirection := stDescending;

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER, sSql);

    LCommand.OrderByFields.Add(TSQLOrderByField.Create('COUNTRYNAME', LCountriesTable));
    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER_MULTIPLE, sSql);

    LCommand.GroupByFields.Add(TSQLGroupByField.Create('HEIGHT', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('NAME', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('AGE', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('COUNTRYNAME', LCountriesTable));

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER_GROUP, sSql);

    LCommand.WhereFields.Add(TSQLWhereField.Create('NAME', LTable));

    sSql := FAnsiSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsSQL(SQL_SELECT_TEST_JOIN_2_ORDER_GROUP_WHERE, sSql);

  finally
    LTable.Free;
    LJoinTable.Free;
    LCountriesTable.Free;
    LCommand.Free;
  end;
end;

const
  SQL_INSERT_TEST = 'INSERT INTO TEST.CUSTOMERS ('+ sLineBreak +
    '  "NAME","AGE","HEIGHT")'+ sLineBreak +
    '  VALUES ('+ sLineBreak +
    ':NAME1,:AGE1,:HEIGHT1);';

  SQL_INSERT_TEST_WITHOUT_SCHEMA = 'INSERT INTO CUSTOMERS ('+ sLineBreak +
    '  "NAME","AGE","HEIGHT")'+ sLineBreak +
    '  VALUES ('+ sLineBreak +
    ':NAME1,:AGE1,:HEIGHT1);';

procedure TAnsiSQLGeneratorTest.TestGenerateInsert;
var
  ReturnValue: string;
  LCommand: TInsertCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TInsertCommand.Create(LTable);
  try
    LCommand.InsertFields.Add(TSQLInsertField.Create('NAME', LTable, nil, ':NAME1'));
    LCommand.InsertFields.Add(TSQLInsertField.Create('AGE', LTable, nil, ':AGE1'));
    LCommand.InsertFields.Add(TSQLInsertField.Create('HEIGHT', LTable, nil, ':HEIGHT1'));

    ReturnValue := FAnsiSQLGenerator.GenerateInsert(LCommand);
    CheckEqualsString(SQL_INSERT_TEST, ReturnValue);

    LTable.Schema := '';
    ReturnValue := FAnsiSQLGenerator.GenerateInsert(LCommand);
    CheckEqualsString(SQL_INSERT_TEST_WITHOUT_SCHEMA, ReturnValue);

  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

const
  SQL_PAGED_TEST = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1;';
  SQL_PAGED = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1 LIMIT 1,10 ;';

procedure TAnsiSQLGeneratorTest.TestGeneratePagedQuery;
var
  LSQL: string;
begin
  LSQL := FAnsiSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 1);
  CheckEqualsString(SQL_PAGED, LSQL);
end;

const
  SQL_UPDATE_TEST = 'UPDATE TEST.CUSTOMERS SET ' + sLineBreak
  + '"NAME"=:NAME1,"AGE"=:AGE1,"HEIGHT"=:HEIGHT1' + sLineBreak + ' WHERE "ID"=:ID1;';

procedure TAnsiSQLGeneratorTest.TestGenerateUpdate;
var
  ReturnValue: string;
  LCommand: TUpdateCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TUpdateCommand.Create(LTable);
  try
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('NAME', LTable, nil,':NAME1'));
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('AGE', LTable, nil, ':AGE1'));
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('HEIGHT', LTable, nil, ':HEIGHT1'));
    LCommand.WhereFields.Add(TSQLWhereField.Create('ID', LTable, nil, ':ID1'));

    ReturnValue := FAnsiSQLGenerator.GenerateUpdate(LCommand);
    CheckEqualsString(SQL_UPDATE_TEST, ReturnValue);
  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

const
  SQL_DELETE_TEST = 'DELETE FROM TEST.CUSTOMERS' + sLineBreak
  + ' WHERE "ID"=:ID1;';

procedure TAnsiSQLGeneratorTest.TestGenerateDelete;
var
  ReturnValue: string;
  LCommand: TDeleteCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TDeleteCommand.Create(LTable);
  try
    LCommand.WhereFields.Add(TSQLWhereField.Create('ID', LTable, nil, ':ID1'));

    ReturnValue := FAnsiSQLGenerator.GenerateDelete(LCommand);
    CheckEqualsString(SQL_DELETE_TEST, ReturnValue);
  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateTable;
var
  ReturnValue: IList<string>;
  LCommand: TCreateTableCommand;
  LTable: TSQLTable;
  LCols: IList<ColumnAttribute>;
begin
  LTable := CreateTestTable;
  LCommand := TCreateTableCommand.Create(LTable);
  try
    LCols := TRttiExplorer.GetColumns(TCustomer);
    LCommand.SetCommandFieldsFromColumns(LCols);

    ReturnValue := FAnsiSQLGenerator.GenerateCreateTable(LCommand);
    CheckTrue(ReturnValue.Any);
  finally
    LTable.Free;
    LCommand.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateFK;
var
  LSQL: IList<string>;
  LCommand: TCreateForeignKeyCommand;
  LTable: TSQLTable;
  LCols: IList<ColumnAttribute>;
begin
  LTable := CreateTestTable;
  LCommand := TCreateForeignKeyCommand.Create(LTable);
  try
    LCols := TRttiExplorer.GetColumns(TCustomer);
    LCommand.SetCommandFieldsFromColumns(LCols);
    LCommand.ForeignKeys.Add(
      TSQLForeignKeyField.Create('FKColumn', LTable, 'RefColumn', 'RefTable', [fsOnDeleteCascade, fsOnUpdateCascade]
      )
    );

    LSQL := FAnsiSQLGenerator.GenerateCreateForeignKey(LCommand);
    CheckTrue(LSQL.Any);
  finally
    LTable.Free;
    LCommand.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateSequence;
var
  ReturnValue: string;
begin
  ReturnValue := FAnsiSQLGenerator.GenerateCreateSequence(nil);
  CheckEqualsString('', ReturnValue);
end;

procedure TAnsiSQLGeneratorTest.TestGenerateGetNextSequenceValue;
var
  ReturnValue: string;
begin
  ReturnValue := FAnsiSQLGenerator.GenerateGetNextSequenceValue(nil);
  CheckEqualsString('', ReturnValue);
end;

const
  SQL_COUNT_TEST = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1;';
  SQL_COUNT = 'SELECT COUNT(*) FROM (' + sLineBreak +
    'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1' + sLineBreak +
    ') AS ORM_GET_QUERY_COUNT;';

procedure TAnsiSQLGeneratorTest.TestGenerateGetQueryCount;
var
  LSQL: string;
begin
  LSQL := FAnsiSQLGenerator.GenerateGetQueryCount(SQL_COUNT_TEST);
  CheckEqualsString(SQL_COUNT, LSQL);
end;

procedure TAnsiSQLGeneratorTest.TestGenerateGetLastInsertId;
var
  ReturnValue: string;
begin
  ReturnValue := FAnsiSQLGenerator.GenerateGetLastInsertId(nil);
  CheckEquals('', ReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TAnsiSQLGeneratorTest.Suite);
end.

