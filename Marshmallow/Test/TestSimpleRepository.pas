unit TestSimpleRepository;

{$I sv.inc}

interface

uses
  TestFramework, Classes, SysUtils,
  Variants, Spring.Persistence.Core.Graphics,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  TestEntities, Rtti, SQLiteTable3,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.Core.Repository.Proxy;

type
  ICustomerRepository = interface(IPagedRepository<TCustomer, Integer>)
    ['{955BF130-3E2F-45E2-A9E9-79647CA3F33B}']

    [Query('SELECT * FROM CUSTOMERS WHERE CUSTNAME = :0')]
    function FindByName(const AName: string): TCustomer;

    [Query('SELECT * FROM CUSTOMERS WHERE CUSTNAME = :0')]
    function FindByNamePaged(const AName: string; APage: Integer; AItemsPerPage: Integer): IDBPage<TCustomer>;
  end;

  TSimpleRepositoryTests = class(TTestCase)
  private
    FConnection: IDBConnection;
    FSession: TSession;
    FRepository: IPagedRepository<TCustomer,Integer>;
  protected
    function CreateRepository: IInterface; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    property Session: TSession read FSession write FSession;
    property Connection: IDBConnection read FConnection write FConnection;
    property Repository: IPagedRepository<TCustomer, Integer> read FRepository write FRepository;
  published
    procedure FindOne;
    procedure Count;
  end;

  TCustomRepositoryTests = class(TSimpleRepositoryTests)
  private
    FCustomerRepository: ICustomerRepository;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FindByName;
    procedure FindByName_Paged;
    procedure FindWhere_ToList;
    procedure FindWhere_UsingCriterion_ToList;
  end;


implementation

uses
  Spring.Persistence.Adapters.SQLite
  ,Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.SQL.Register
  ,Spring.Persistence.SQL.Params
  ,Spring.Collections
  ,Spring.Persistence.Core.Reflection
  ,TestConsts
  ,TestSession
  ,Spring.Persistence.Criteria.Properties
  ,Diagnostics
  ,Spring.Persistence.Core.Repository.Simple
  ,Generics.Collections
  ;


{ TestAbstractRepository }

procedure TSimpleRepositoryTests.Count;
begin
  InsertCustomer;
  InsertCustomer;
  CheckEquals(2, FRepository.Count);
end;

function TSimpleRepositoryTests.CreateRepository: IInterface;
begin
  Result := TSimpleRepository<TCustomer, Integer>.Create(FSession);
end;

procedure TSimpleRepositoryTests.FindOne;
var
  LCustomer: TCustomer;
  RowID: Integer;
begin
  InsertCustomer;
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
  FRepository := CreateRepository as IPagedRepository<TCustomer,Integer>;

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
end;

procedure TSimpleRepositoryTests.TearDown;
begin
  inherited;
  ClearTable(TBL_PEOPLE);
  ClearTable(TBL_ORDERS);
  ClearTable(TBL_PRODUCTS);
  FSession.Free;
end;

{ TCustomRepositoryTests }

procedure TCustomRepositoryTests.FindByName;
var
  LCustomer: TCustomer;
begin
  InsertCustomer(10, 'Foo');
  LCustomer := FCustomerRepository.FindByName('Foo');
  CheckEquals('Foo', LCustomer.Name);
  LCustomer.Free;
end;

procedure TCustomRepositoryTests.FindByName_Paged;
var
  LCustomerPage: IDBPage<TCustomer>;
begin
  InsertCustomer(10, 'Foo', 1);
  InsertCustomer(15, 'Bar', 2);
  LCustomerPage := FCustomerRepository.FindByNamePaged('Foo', 0, 10);
  CheckEquals(1, LCustomerPage.GetTotalItems);
  CheckEquals('Foo', LCustomerPage.Items.First.Name);
end;

procedure TCustomRepositoryTests.FindWhere_ToList;
begin
  InsertCustomer(10, 'Foo');

  CheckEquals('Foo', FCustomerRepository
    .FindWhere
    .ToList
    .First.Name);
end;

procedure TCustomRepositoryTests.FindWhere_UsingCriterion_ToList;
begin
  InsertCustomer(10, 'Foo');

  CheckEquals('Foo', FCustomerRepository
    .FindWhere(GetProp(CUSTNAME) = 'Foo')
    .ToList
    .First.Name);
end;

procedure TCustomRepositoryTests.SetUp;
begin
  inherited;
  FCustomerRepository := TProxyRepository<TCustomer, Integer>.Create(
    FSession, TypeInfo(ICustomerRepository)) as ICustomerRepository;
end;

procedure TCustomRepositoryTests.TearDown;
begin
  FCustomerRepository := nil;
  inherited;
end;

initialization
  RegisterTest(TSimpleRepositoryTests.Suite);
  RegisterTest(TCustomRepositoryTests.Suite);

end.
