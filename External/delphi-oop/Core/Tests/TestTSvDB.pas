unit TestTSvDB;

interface

uses
  TestFramework, SvDB, SvTesting.DUnit
  ;

type
  TSvAnsiSQLBuilderTests = class(TSvTestCase)
  private
    FSQLBuilder: ISQLBuilder;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Select();
    procedure Select_Union();
    procedure Delete_From_Where();
    procedure Delete_From();
    procedure Insert_Into_Column_Values();
    procedure Insert_Into_Values();
    procedure Update_Set_Where();
    procedure Update_SetNull();
    procedure Update_Set_Where_Dequoted();
  end;

  TSvTransactSQLBuilderTests = class(TSvTestCase)
  private
    FSQLBuilder: ISQLBuilder;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SelectTop();
    procedure Update_Join();
  end;

implementation


{ TSvSQLBuilderTests }

procedure TSvAnsiSQLBuilderTests.Delete_From;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Delete
    .From('dbo.Customers c')
    .ToString();

  CheckEquals('DELETE FROM dbo.Customers c', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Delete_From_Where;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Delete
    .From('dbo.Customers c')
    .Where('c.AGE < 18')
    .ToString();

  CheckEquals('DELETE FROM dbo.Customers c'+ #13#10 + ' WHERE (c.AGE < 18)', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Insert_Into_Column_Values;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Insert
    .Into('dbo.Customers')
    .Column('AGE').Values('18')
    .Column('NAME').Values('Bob')
    .ToString();

  CheckEquals('INSERT INTO dbo.Customers'+ #13#10 + ' ('+#13#10+'AGE,NAME'+#13#10+' )'+#13#10+' VALUES'+#13#10+' (''18'',''Bob'')', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Insert_Into_Values;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Insert
    .Into('dbo.Customers')
    .Values('18')
    .Values('Bob')
    .ToString();

  CheckEquals('INSERT INTO dbo.Customers'+ #13#10 +' VALUES'+#13#10+' (''18'',''Bob'')', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Select;
var
  LSQL, LPreviousSQL: string;
begin
  CheckEquals('', FSQLBuilder.ToString);

  LSQL := FSQLBuilder
    .Select.Column('C.FIRSTNAME').Column('C.LASTNAME').ToString;
  CheckEquals('', LSQL);

  LSQL := FSQLBuilder
    .From('dbo.Customers C')
    .Join('dbo.Details D on D.ID=C.ID').ToString;
  CheckEquals('SELECT '+ #13#10 +'C.FIRSTNAME,C.LASTNAME'+ #13#10 + ' FROM dbo.Customers C'+ #13#10 +' JOIN dbo.Details D on D.ID=C.ID', LSQL);

  LSQL := FSQLBuilder
    .Where('C.CUSTNAME = ''Foobar''')
    .Where('D.CUSTORDER = 1')
    .ToString;
  CheckEquals('SELECT '+ #13#10 +'C.FIRSTNAME,C.LASTNAME'+ #13#10 + ' FROM dbo.Customers C'+ #13#10 +' JOIN dbo.Details D on D.ID=C.ID'+#13#10+
    ' WHERE (C.CUSTNAME = ''Foobar'') AND (D.CUSTORDER = 1)', LSQL);

  LSQL := FSQLBuilder.Select.Column('SUM(D.CUSTCOUNT)')
    .GroupBy('C.FIRSTNAME')
    .GroupBy('C.LASTNAME')
    .ToString;
  CheckEquals('SELECT '+ #13#10 +'C.FIRSTNAME,C.LASTNAME,SUM(D.CUSTCOUNT)'+ #13#10 + ' FROM dbo.Customers C'+ #13#10 +' JOIN dbo.Details D on D.ID=C.ID'+#13#10+
    ' WHERE (C.CUSTNAME = ''Foobar'') AND (D.CUSTORDER = 1)'+ #13#10+
    ' GROUP BY C.FIRSTNAME,C.LASTNAME', LSQL);
  LPreviousSQL := LSQL;

  LSQL := FSQLBuilder.Having('C.LASTNAME <> ''''').ToString;
  CheckEquals(LPreviousSQL + #13#10 + ' HAVING (C.LASTNAME <> '''')', LSQL);
  LPreviousSQL := LSQL;
  LSQL := FSQLBuilder
    .OrderBy('1 ASC')
    .OrderBy('C.LASTNAME').ToString;

  CheckEquals(LPreviousSQL + #13#10 + ' ORDER BY 1 ASC,C.LASTNAME', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Select_Union;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder
  .Select
    .From('dbo.Customers C')
    .Join('dbo.Details D on D.ID=C.ID')
    .Column('C.FIRSTNAME').Column('C.LASTNAME')
  .Union('SELECT '+ #13#10 +'NULL, NULL')
  .ToString;
  CheckEquals('SELECT '+ #13#10 +'C.FIRSTNAME,C.LASTNAME'+ #13#10 + ' FROM dbo.Customers C'+ #13#10 +' JOIN dbo.Details D on D.ID=C.ID'+ #13#10 +
    'UNION' + #13#10 +
    'SELECT '+ #13#10 +'NULL, NULL', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.SetUp;
begin
  inherited;
  FSQLBuilder := TAnsiSQLBuilder.Create();
end;

procedure TSvAnsiSQLBuilderTests.TearDown;
begin
  inherited;
end;

procedure TSvAnsiSQLBuilderTests.Update_SetNull;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Update
    .Table('dbo.Customers')
    .Column('AGE').Values('18')
    .Column('NAME').Values('Null')
    .ToString;

  CheckEquals('UPDATE dbo.Customers' + #13#10 + ' SET' + #13#10 + ' AGE=''18'''+#13#10+' ,NAME=Null'
    , LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Update_Set_Where;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Update
    .Table('dbo.Customers')
    .Column('AGE').Values('18')
    .Column('NAME').Values('Bob')
    .Where('AGE > 18')
    .ToString;

  CheckEquals('UPDATE dbo.Customers' + #13#10 + ' SET' + #13#10 + ' AGE=''18'''+#13#10+' ,NAME=''Bob''' + #13#10 +
    ' WHERE (AGE > 18)', LSQL);
end;

procedure TSvAnsiSQLBuilderTests.Update_Set_Where_Dequoted;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Update
    .Table('dbo.Customers')
    .Column('AGE').Values('18', False)
    .Column('NAME').Values('Bob')
    .Where('AGE > 18')
    .ToString;

  CheckEquals('UPDATE dbo.Customers' + #13#10 + ' SET' + #13#10 + ' AGE=18'+#13#10+' ,NAME=''Bob''' + #13#10 +
    ' WHERE (AGE > 18)', LSQL);
end;

{ TSvTransactSQLBuilderTests }

procedure TSvTransactSQLBuilderTests.SelectTop;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder
    .Select.Top(100).Column('C.FIRSTNAME').Column('C.LASTNAME').ToString;
  CheckEquals('', LSQL);

  LSQL := FSQLBuilder
    .From('dbo.Customers C')
    .Join('dbo.Details D on D.ID=C.ID').ToString;
  CheckEquals('SELECT TOP 100 ' +#13#10 + 'C.FIRSTNAME,C.LASTNAME'+ #13#10 + ' FROM dbo.Customers C'+ #13#10 +' JOIN dbo.Details D on D.ID=C.ID', LSQL);
end;

procedure TSvTransactSQLBuilderTests.Update_Join;
var
  LSQL: string;
begin
  LSQL := FSQLBuilder.Update
    .Table('C')
    .Column('C.AGE').Values('18')
    .Column('C.NAME').Values('Null')
    .From('dbo.Customers C')
    .Join('dbo.Orders o on o.CUSTID=c.CUSTID')
    .ToString;

  CheckEquals('UPDATE C' + #13#10 + ' SET' + #13#10 + ' C.AGE=''18'''+#13#10+' ,C.NAME=Null' + #13#10 +
    ' FROM dbo.Customers C' + #13#10 + ' JOIN dbo.Orders o on o.CUSTID=c.CUSTID'
    , LSQL);
end;

procedure TSvTransactSQLBuilderTests.SetUp;
begin
  inherited;
  FSQLBuilder := TTransactSQLBuilder.Create();
end;

procedure TSvTransactSQLBuilderTests.TearDown;
begin
  inherited;
end;

initialization
  RegisterTest(TSvAnsiSQLBuilderTests.Suite);
  RegisterTest(TSvTransactSQLBuilderTests.Suite);

end.
