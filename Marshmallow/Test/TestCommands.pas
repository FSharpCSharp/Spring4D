unit TestCommands;

interface

uses
  TestFramework, SQL.Commands.TableCreator, SQL.AbstractCommandExecutor, SQL.Commands, SQL.Types
  ,Core.Interfaces
  ;

type
  TTableCreatorTest = class(TTestCase)
  private
    FCommand: TTableCreateExecutor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TableExists();
  end;

implementation

uses
  TestSession
  ,Core.ConnectionFactory
  ,uModels
  ,TestConsts
  ;

{ TTableCreatorTest }

procedure TTableCreatorTest.SetUp;
begin
  inherited;
  FCommand := TTableCreateExecutor.Create;
  FCommand.Connection := TConnectionFactory.GetInstance(dtSQLite, TestDB);
end;

procedure TTableCreatorTest.TableExists;
begin
  FCommand.Build(TCustomer);
  CheckTrue( FCommand.TableExists(TBL_PEOPLE));
  CheckFalse(FCommand.TableExists('FOOBAR'));
end;

procedure TTableCreatorTest.TearDown;
begin
  FCommand.Free;
  inherited;
end;

initialization
  RegisterTest(TTableCreatorTest.Suite);

end.
