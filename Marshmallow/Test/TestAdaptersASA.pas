unit TestAdaptersASA;

interface

uses
  TestFramework, Spring.Persistence.Adapters.ASA
  , ADODB, Spring.Persistence.SQL.Generators.ASA;

type
  TestTASAConnectionAdapter = class(TTestCase)
  strict private
    FASAConnectionAdapter: TASAConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetDriverName;
  end;

  TestTASASQLGenerator = class(TTestCase)
  strict private
    FASASQLGenerator: TASASQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateGetLastInsertId;
  end;

implementation

procedure TestTASAConnectionAdapter.SetUp;
begin
  FASAConnectionAdapter := TASAConnectionAdapter.Create(TADOConnection.Create(nil));
  FASAConnectionAdapter.AutoFreeConnection := True;
end;

procedure TestTASAConnectionAdapter.TearDown;
begin
  FASAConnectionAdapter.Free;
end;

procedure TestTASAConnectionAdapter.TestGetDriverName;
var
  ReturnValue: string;
begin
  ReturnValue := FASAConnectionAdapter.GetDriverName;
  CheckEqualsString('ASA', ReturnValue);
end;

procedure TestTASASQLGenerator.SetUp;
begin
  FASASQLGenerator := TASASQLGenerator.Create();
end;

procedure TestTASASQLGenerator.TearDown;
begin
  FASASQLGenerator.Free;
end;

procedure TestTASASQLGenerator.TestGenerateGetLastInsertId;
var
  ReturnValue: string;
begin
  ReturnValue := FASASQLGenerator.GenerateGetLastInsertId(nil);
  CheckEqualsString('SELECT @@IDENTITY;', ReturnValue);
end;

initialization
  RegisterTest(TestTASASQLGenerator.Suite);
  RegisterTest(TestTASAConnectionAdapter.Suite);


end.

