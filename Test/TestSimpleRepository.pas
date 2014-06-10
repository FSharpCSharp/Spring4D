unit TestSimpleRepository;

{$I sv.inc}

interface

uses
  TestFramework, Windows, Forms, Dialogs, Controls, Classes, SysUtils,
  Variants, Graphics, Messages, StdCtrls, Core.Session, Core.Interfaces
  ,uModels, Rtti, SQLiteTable3;

type
  TSimpleRepositoryTests = class(TTestCase)
  private
    FConnection: IDBConnection;
    FSession: TSession;
    FRepository: IPagedRepository<TCustomer,Integer>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FindOne();
    procedure Count();
  end;


implementation

uses
  Adapters.SQLite
  ,Core.ConnectionFactory
  ,SQL.Register
  ,SQL.Params
  ,SvDesignPatterns
  ,SvRttiUtils
  ,Spring.Collections
  ,Core.Reflection
  ,TestConsts
  ,TestSession
  ,Core.Criteria.Properties
  ,Diagnostics
  ,Core.Repository.Simple
  ,Generics.Collections
  ;


{ TestAbstractRepository }

procedure TSimpleRepositoryTests.Count;
begin
  InsertCustomer();
  InsertCustomer();
  CheckEquals(2, FRepository.Count);
end;

procedure TSimpleRepositoryTests.FindOne;
var
  LCustomer: TCustomer;
  RowID: Integer;
begin
  InsertCustomer();
  RowID := TestDB.GetLastInsertRowID;
  LCustomer := FRepository.FindOne(RowID);
  CheckEquals(RowID, LCustomer.ID);
  LCustomer.Free;
end;

procedure TSimpleRepositoryTests.SetUp;
begin
  inherited;
  FConnection := TConnectionFactory.GetInstance(dtSQLite, TestDB);
  FSession := TSession.Create(FConnection);
  FRepository := TSimpleRepository<TCustomer, Integer>.Create(FSession);

  FConnection.AddExecutionListener(
    procedure(const ACommand: string; const AParams: TObjectList<TDBParam>)
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
end;

procedure TSimpleRepositoryTests.TearDown;
begin
  inherited;
  ClearTable(TBL_PEOPLE);
  ClearTable(TBL_ORDERS);
  ClearTable(TBL_PRODUCTS);
  FSession.Free;
end;

initialization
  RegisterTest(TSimpleRepositoryTests.Suite);

end.
