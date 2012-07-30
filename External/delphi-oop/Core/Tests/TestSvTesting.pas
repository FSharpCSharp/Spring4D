unit TestSvTesting;

interface

uses
  SvTesting.DUnit, Classes;

type
  TNotExists = class(TCustomAttribute)

  end;

  TTestClass = class
  private
    FName: string;
    FSecret: string;
    FValues: TStrings;

    property Secret: string read FSecret write FSecret;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SomeLengthyOperation();

    property TestName: string read FName write FName;
    property Values: TStrings read FValues write FValues;
  end;

  TestTSvTestingDUnit = class(TSvTestCase)
  private
    FObject: TTestClass;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    [Timeout(200)]
    procedure TestTimeout();
    [Timeout(10)]
    procedure TestTimeoutFail();
  end;

implementation

uses
  TestFramework,
  SysUtils;

{ TTestClass }

constructor TTestClass.Create;
begin
  inherited Create;
  FValues := TStringList.Create;
end;

destructor TTestClass.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TTestClass.SomeLengthyOperation;
begin
  Sleep(100);
end;

{ TestTSvTestingDUnit }

procedure TestTSvTestingDUnit.SetUp;
begin
  inherited;
  FObject := TTestClass.Create;
end;

procedure TestTSvTestingDUnit.TearDown;
begin
  FObject.Free;
  inherited;
end;

procedure TestTSvTestingDUnit.TestTimeout;
begin
  Sleep(10);
end;

procedure TestTSvTestingDUnit.TestTimeoutFail;
begin
  Sleep(50);
  ExpectedException := ETestTimeoutException;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSvTestingDUnit.Suite);

end.
