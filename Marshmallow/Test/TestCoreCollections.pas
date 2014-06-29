unit TestCoreCollections;

interface

uses
  TestFramework, Rtti, Core.Collections, uModels;

type
  // Test methods for class ICollectionAdapter
  TBaseCollectionTestCase = class(TTestCase)
  private
    FCollection: TValue;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    property Collection: TValue read FCollection write FCollection;
  end;

  TBaseSpringCollectionTestCase = class(TTestCase)
  private
    FCollection: TValue;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    property Collection: TValue read FCollection write FCollection;
  end;


  // Test methods for class TCollectionAdapter

  TestTCollectionAdapter = class(TBaseCollectionTestCase)
  private
    FCollectionAdapter: TCollectionAdapter<TCustomer>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestCount;
    procedure TestGetEnumerator;
    procedure TestIsSupported;
    procedure Performance();
  end;

  TestTSpringCollectionAdapter = class(TBaseSpringCollectionTestCase)
  private
    FCollectionAdapter: TCollectionAdapter<TCustomer>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestCount;
    procedure TestGetEnumerator;
    procedure TestIsSupported;
  end;

implementation

uses
  Generics.Collections
  ,Spring.Collections
  ,Diagnostics
  ,SysUtils
  ;


procedure TestTCollectionAdapter.Performance;
var
  i, iterations: Integer;
  sw, sw2: TStopwatch;
  LDefault: TObjectList<TCustomer>;
begin
  iterations := 10000;
  LDefault := TObjectList<TCustomer>.Create(True);
  try
    sw := TStopwatch.StartNew;
    for i := 1 to iterations do
    begin
      FCollectionAdapter.Add(TCustomer.Create);
    end;
    sw.Stop;

    sw2 := TStopwatch.StartNew;
    for i := 1 to iterations do
    begin
      LDefault.Add(TCustomer.Create);
    end;
    sw2.Stop;

    Status(Format('Adapter in %D ms. Default TObjectList in %D ms',
      [sw.ElapsedMilliseconds, sw2.ElapsedMilliseconds]));

  finally
    LDefault.Free;
  end;
end;

procedure TestTCollectionAdapter.SetUp;
begin
  inherited;
  FCollectionAdapter := TCollectionAdapter<TCustomer>.Create(Collection);
end;

procedure TestTCollectionAdapter.TearDown;
begin
  FCollectionAdapter.Free;
  FCollectionAdapter := nil;
  inherited;
end;

procedure TestTCollectionAdapter.TestAdd;
var
  AEntity: TCustomer;
begin
  AEntity := TCustomer.Create;
  AEntity.Name := 'Bob';
  FCollectionAdapter.Add(AEntity);
  CheckEquals(1, FCollectionAdapter.Count);
end;

procedure TestTCollectionAdapter.TestClear;
begin
  FCollectionAdapter.Add(TCustomer.Create);
  CheckEquals(1, FCollectionAdapter.Count);
  FCollectionAdapter.Clear;
  CheckEquals(0, FCollectionAdapter.Count);
end;

procedure TestTCollectionAdapter.TestCount;
begin
  CheckEquals(0, FCollectionAdapter.Count);
  FCollectionAdapter.Add(TCustomer.Create);
  CheckEquals(1, FCollectionAdapter.Count);
end;

procedure TestTCollectionAdapter.TestGetEnumerator;
var
  LCustomer: TCustomer;
  i: Integer;
begin
  i := 0;
  FCollectionAdapter.Add(TCustomer.Create);
  FCollectionAdapter.Add(TCustomer.Create);
  for LCustomer in FCollectionAdapter do
  begin
    CheckTrue(Assigned(LCustomer));
    Inc(i);
  end;
  CheckEquals(2, i);
end;

procedure TestTCollectionAdapter.TestIsSupported;
begin
  CheckTrue(FCollectionAdapter.IsAddSupported);
end;

{ TBaseCollectionTestCase }

procedure TBaseCollectionTestCase.SetUp;
begin
  FCollection := TObjectList<TCustomer>.Create(True);
  inherited;
end;

procedure TBaseCollectionTestCase.TearDown;
begin
  FCollection.AsObject.Free;
  inherited;
end;

{ TBaseSpringCollectionTestCase }

procedure TBaseSpringCollectionTestCase.SetUp;
var
  LList: IList<TCustomer>;
begin
  inherited;
  LList := TCollections.CreateObjectList<TCustomer>();
  FCollection := TValue.From<IList<TCustomer>>(LList);
end;

procedure TBaseSpringCollectionTestCase.TearDown;
begin
  inherited;

end;

{ TestTSpringCollectionAdapter }

procedure TestTSpringCollectionAdapter.SetUp;
begin
  inherited;
  FCollectionAdapter := TCollectionAdapter<TCustomer>.Create(Collection);
end;

procedure TestTSpringCollectionAdapter.TearDown;
begin
  inherited;
  FCollectionAdapter.Free;
end;

procedure TestTSpringCollectionAdapter.TestAdd;
var
  AEntity: TCustomer;
begin
  AEntity := TCustomer.Create;
  AEntity.Name := 'Bob';
  FCollectionAdapter.Add(AEntity);
  CheckEquals(1, FCollectionAdapter.Count);
end;

procedure TestTSpringCollectionAdapter.TestClear;
begin
  FCollectionAdapter.Add(TCustomer.Create);
  CheckEquals(1, FCollectionAdapter.Count);
  FCollectionAdapter.Clear;
  CheckEquals(0, FCollectionAdapter.Count);
end;

procedure TestTSpringCollectionAdapter.TestCount;
begin
  CheckEquals(0, FCollectionAdapter.Count);
  FCollectionAdapter.Add(TCustomer.Create);
  CheckEquals(1, FCollectionAdapter.Count);
end;

procedure TestTSpringCollectionAdapter.TestGetEnumerator;
var
  LCustomer: TCustomer;
  i: Integer;
begin
  i := 0;
  FCollectionAdapter.Add(TCustomer.Create);
  FCollectionAdapter.Add(TCustomer.Create);
  for LCustomer in FCollectionAdapter do
  begin
    CheckTrue(Assigned(LCustomer));
    Inc(i);
  end;
  CheckEquals(2, i);
end;

procedure TestTSpringCollectionAdapter.TestIsSupported;
begin
  CheckTrue(FCollectionAdapter.IsAddSupported);
end;

initialization
  RegisterTest(TestTCollectionAdapter.Suite);
  RegisterTest(TestTSpringCollectionAdapter.Suite);
end.

