unit TestAdaptersOracle;

interface

uses
  TestFramework, Spring.Persistence.Adapters.Oracle, SysUtils, Spring.Persistence.Adapters.ADO
  , ADODB, Spring.Persistence.Core.Interfaces, TestEntities
  ,Generics.Collections, Spring.Persistence.Core.Session, Spring.Persistence.SQL.Generators.Oracle;

type
  TestOracleConnectionAdapter = class(TTestCase)
  private
    FConnectionAdapter: TOracleConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GetDriverName;
  end;

  TestOracleSession = class(TTestCase)
  private
    FConnection: IDBConnection;
    FManager: TSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure First();
    procedure Save_Delete();
    procedure Page();
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.Core.DatabaseManager
  ,Spring.Collections
  ,Spring.Persistence.Criteria.Properties
  ,Spring.Persistence.SQL.Params
  ,Variants
  ;

const
  TBL_COMPANY = 'Vikarina.IMONES';

procedure CreateTestTables(AConnection: IDBConnection);
var
  LDBManager: TDatabaseManager;
begin
  LDBManager := TDatabaseManager.Create(AConnection);
  try
    LDBManager.ClearEntities;
    LDBManager.RegisterEntity(TCompany);
    LDBManager.BuildDatabase;
  finally
    LDBManager.Free;
  end;
end;

function CreateTestConnection: TADOConnection;
begin
  Result := TADOConnection.Create(nil);
  Result.LoginPrompt := False;
  //Provider=OraOLEDB.Oracle;Data Source=SERVER1;User ID=SYSTEM;Password=master
  Result.ConnectionString := Format('Provider=OraOLEDB.Oracle;Data Source=%0:S;Password=%1:S;User ID=%2:S'
    , ['SERVER1', 'master', 'SYSTEM']);
  Result.Open();
end;

procedure DropTestTables();
var
  LConn: TADOConnection;
begin
  LConn := CreateTestConnection;
  try
    LConn.Execute('DROP TABLE ' + TBL_COMPANY);
  finally
    LConn.Free;
  end;
end;

procedure InsertCompany(AID: Integer; const ACompName: string);
var
  LConn: TADOConnection;
begin
  LConn := CreateTestConnection;
  try
    LConn.Execute(Format('INSERT INTO '+ TBL_COMPANY + ' (IMONE, IMPAV) VALUES (%0:D, %1:S)'
      ,
        [
          AID
          ,QuotedStr(ACompName)
        ]
      ));
  finally
    LConn.Free;
  end;
end;

function GetTableCount(const ATablename: string): Variant;
var
  LConn: TADOConnection;
  LResults: _Recordset;
begin
  LConn := CreateTestConnection;
  try
    LResults := LConn.Execute(Format('SELECT COUNT(*) FROM %0:S'
      ,
        [
          ATablename
        ]
      ));

    if LResults.RecordCount > 0 then
    begin
      Result := LResults.Fields.Item[0].Value;
    end;
  finally
    LConn.Free;
  end;

end;


{ TestOracleConnectionAdapter }

procedure TestOracleConnectionAdapter.GetDriverName;
begin
  CheckEquals('Oracle', FConnectionAdapter.GetDriverName);
end;

procedure TestOracleConnectionAdapter.SetUp;
begin
  inherited;
  FConnectionAdapter := TOracleConnectionAdapter.Create(CreateTestConnection);
  FConnectionAdapter.AutoFreeConnection := True;
end;

procedure TestOracleConnectionAdapter.TearDown;
begin
  FConnectionAdapter.Free;
  inherited;
end;

{ TestOracleSession }

procedure TestOracleSession.First;
var
  LCompany: TCompany;
begin
  //insert company
  InsertCompany(1, 'Oracle Company');
  LCompany := FManager.FindOne<TCompany>(1);
  try
    CheckTrue(Assigned(LCompany));
    CheckEquals(1, LCompany.ID);
    CheckEquals('Oracle Company', LCompany.Name);
  finally
    LCompany.Free;
  end;
end;

procedure TestOracleSession.Page;
var
  LCriteria: ICriteria<TCompany>;
  LItems: IList<TCompany>;
  Imone: IProperty;
  i: Integer;
begin
  for i := 1 to 20 do
  begin
    InsertCompany(i, Format('%D Company', [i]));
  end;
  Imone := TProperty<TCompany>.ForName('IMONE');

  LCriteria := FManager.CreateCriteria<TCompany>;
  LCriteria.Add(Imone.GEq(1));
  LItems := LCriteria.Page(1, 10).Items;
  CheckEquals(10, LItems.Count);

  LItems := LCriteria.Page(2, 10).Items;
  CheckEquals(10, LItems.Count);
end;

procedure TestOracleSession.Save_Delete;
var
  LCompany: TCompany;
begin
  LCompany := TCompany.Create;
  try
    InsertCompany(1, 'Oracle Company');
    LCompany.Name := 'Inserted Company';
    LCompany.ID := 2;
    LCompany.Logo.LoadFromFile(PictureFilename);
    FManager.Save(LCompany);
    CheckEquals(2, GetTableCount(TBL_COMPANY));

    FManager.Delete(LCompany);
    CheckEquals(1, GetTableCount(TBL_COMPANY));
  finally
    LCompany.Free;
  end;
end;

procedure TestOracleSession.SetUp;
begin
  inherited;
  FConnection := TConnectionFactory.GetInstance(dtOracle, CreateTestConnection);
  FConnection.AutoFreeConnection := True;
  FManager := TSession.Create(FConnection);

  FConnection.AddExecutionListener(
    procedure(const ACommand: string; const AParams: IList<TDBParam>)
    var
      i: Integer;
    begin
      Status(ACommand);
      for i := 0 to AParams.Count - 1 do
      begin
        Status(Format('%2:D Param %0:S = %1:S', [AParams[i].Name, VarToStrDef(AParams[i].Value, 'NULL'), i]));
      end;
      Status('-----');
    end);

  CreateTestTables(FConnection);
end;

procedure TestOracleSession.TearDown;
begin
  DropTestTables;
  FManager.Free;
  FConnection := nil;
  inherited;
end;

initialization
  if FileExists('D:\Oracle\oraociei11.dll') then
  begin
    RegisterTest(TestOracleConnectionAdapter.Suite);
    RegisterTest(TestOracleSession.Suite);
  end;


end.
