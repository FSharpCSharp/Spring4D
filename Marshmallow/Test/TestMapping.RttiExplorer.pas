unit TestMapping.RttiExplorer;

interface

uses
  TestFramework, Spring.Persistence.Mapping.Attributes, Generics.Collections
  , Spring.Persistence.Mapping.RttiExplorer,
  Rtti, TestEntities;

type
  TForeignCustomer = class(TCustomer)
  end;

  TRttiExplorerTest = class(TTestCase)
  private
    FCustomer: TCustomer;
    FProduct: TProduct;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetClassMembers;
    procedure TestGetTable;
    procedure TestGetUniqueConstraints;
    procedure TestGetColumns;
    procedure TestGetSequence;
    procedure TestHasSequence;
    procedure TestGetMemberValue;
    procedure TestCopyFieldValues;
    procedure TestClone;
    {$IFDEF PERFORMANCE_TESTS}
    procedure TestCloneSpeed;
    {$ENDIF}
    procedure GetPrimaryKey;
    procedure TestGetEntities;
  end;

implementation

uses
  Classes,
  DateUtils,
  Diagnostics,
  Math,
  SysUtils,
  Spring.Collections;

procedure TRttiExplorerTest.GetPrimaryKey;
var
  LColumn: ColumnAttribute;
begin
  LColumn := TRttiExplorer.GetPrimaryKeyColumn(FCustomer.ClassType);
  CheckTrue(Assigned(LColumn));

  CheckEqualsString('CUSTID', LColumn.ColumnName);
  CheckEqualsString('CUSTID',TRttiExplorer.GetPrimaryKeyColumn(FCustomer.ClassType).ColumnName);
  CheckEqualsString('FId',TRttiExplorer.GetPrimaryKeyColumn(FCustomer.ClassType).MemberName);
end;

procedure TRttiExplorerTest.SetUp;
begin
  FCustomer := TCustomer.Create;
  FProduct := TProduct.Create;
end;

procedure TRttiExplorerTest.TearDown;
begin
  FCustomer.Free;
  FProduct.Free;
end;

procedure TRttiExplorerTest.TestGetClassMembers;
var
  ReturnValue: IList<EntityAttribute>;
  LColumns: IList<ColumnAttribute>;
  AClassInfo: Pointer;
begin
  AClassInfo := FProduct.ClassType;
  ReturnValue := TRttiExplorer.GetClassMembers<EntityAttribute>(AClassInfo);
  CheckEquals(0, ReturnValue.Count);

  LColumns := TRttiExplorer.GetClassMembers<ColumnAttribute>(AClassInfo);
  CheckEquals(4, LColumns.Count);
end;

procedure TRttiExplorerTest.TestGetTable;
var
  ReturnValue: TableAttribute;
  AClass: TClass;
begin
  AClass := FProduct.ClassType;
  ReturnValue := TRttiExplorer.GetTable(AClass);
  CheckEqualsString('Products', ReturnValue.TableName);

  AClass := FCustomer.ClassType;
  ReturnValue := TRttiExplorer.GetTable(AClass);
  CheckEqualsString('Customers', ReturnValue.TableName);
end;

procedure TRttiExplorerTest.TestGetUniqueConstraints;
var
  ReturnValue: IList<UniqueConstraint>;
  AClass: TClass;
begin
  AClass := FCustomer.ClassType;

  ReturnValue := TRttiExplorer.GetUniqueConstraints(AClass);
  CheckEquals(1, ReturnValue.Count);
  CheckEqualsString('FId', ReturnValue.First.MemberName);
end;

procedure TRttiExplorerTest.TestGetColumns;
var
  ReturnValue: IList<ColumnAttribute>;
  AClass: TClass;
begin
  AClass := FCustomer.ClassType;

  ReturnValue := TRttiExplorer.GetColumns(AClass);
  CheckEquals(CustomerColumnCount, ReturnValue.Count);
end;

procedure TRttiExplorerTest.TestGetEntities;
var
  LEntities: IList<TClass>;
begin
  LEntities := TRttiExplorer.GetEntities;
  CheckEquals(6, LEntities.Count);
end;

procedure TRttiExplorerTest.TestGetSequence;
var
  LSequence: SequenceAttribute;
  AClass: TClass;
  LForeigner: TForeignCustomer;
begin
  AClass := FCustomer.ClassType;

  LSequence := TRttiExplorer.GetSequence(AClass);

  CheckTrue(Assigned(LSequence));
  CheckEquals(1, LSequence.Increment);

  LForeigner := TForeignCustomer.Create;
  try
    AClass := LForeigner.ClassType;
    LSequence := TRttiExplorer.GetSequence(AClass);
    CheckTrue(Assigned(LSequence));
  finally
    LForeigner.Free;
  end;
end;

procedure TRttiExplorerTest.TestHasSequence;
var
  ReturnValue: Boolean;
  AClass: TClass;
  AClas2: TClass;
begin
  AClass := FCustomer.ClassType;
  AClas2 := FProduct.ClassType;

  ReturnValue := TRttiExplorer.HasSequence(AClass);
  CheckTrue(ReturnValue);

  ReturnValue := TRttiExplorer.HasSequence(AClas2);
  CheckFalse(ReturnValue);
end;

procedure TRttiExplorerTest.TestGetMemberValue;
var
  ReturnValue: TValue;
  AMemberName: string;
begin
  AMemberName := 'Name';
  FCustomer.Name := 'Test';
  ReturnValue := TRttiExplorer.GetMemberValue(FCustomer, AMemberName);
  CheckEqualsString('Test', ReturnValue.AsString);

  AMemberName := 'LastEdited';
  FCustomer.LastEdited := EncodeDate(2009, 11, 15);
  ReturnValue := TRttiExplorer.GetMemberValue(FCustomer, AMemberName);
  CheckTrue(SameDate(EncodeDate(2009, 11, 15), ReturnValue.AsExtended));
end;

procedure TRttiExplorerTest.TestCopyFieldValues;
var
  AEntityTo: TCustomer;
  AEntityFrom: TCustomer;
begin
  AEntityTo := TCustomer.Create;
  AEntityFrom := TCustomer.Create;
  try
    AEntityFrom.Name := 'From';
    AEntityFrom.Age := 15;
    AEntityFrom.Height := 1.111;
    AEntityFrom.EMail := 'test@gmail.com';
    AEntityFrom.LastEdited := Tomorrow;

    TRttiExplorer.CopyFieldValues(AEntityFrom, AEntityTo);

    CheckEqualsString('From', AEntityTo.Name);
    CheckEquals(15, AEntityTo.Age);
    CheckTrue(SameValue(1.111, AEntityTo.Height));
    CheckEqualsString('test@gmail.com', AEntityTo.EMail);
    CheckTrue(SameDate(Tomorrow, AEntityTo.LastEdited));
  finally
    AEntityTo.Free;
    AEntityFrom.Free;
  end;
end;

procedure TRttiExplorerTest.TestClone;
var
  clonedCustomer: TCustomer;
  customer: TCustomer;
  stream: TMemoryStream;
  order: TCustomer_Orders;
begin
  customer := TCustomer.Create;
  stream := TMemoryStream.Create;
  try
    customer.Name := 'Clone';
    customer.Age := 4589;
    customer.LastEdited := EncodeDate(2011,1,1);
    customer.Height := 1.1234;
    customer.CustomerType := ctBusinessClass;
    customer.MiddleName := 'Bob';
    customer.StreamLazy := stream;

    order := TCustomer_Orders.Create;
    order.Customer_Payment_Method_Id := 15;
    customer.OrdersIntf := TCollections.CreateObjectList<TCustomer_Orders>;
    customer.OrdersIntf.Add(order);

    clonedCustomer := TRttiExplorer.Clone(customer) as TCustomer;
    try
      CheckEqualsString('Clone', clonedCustomer.Name);
      CheckEquals(customer.Age, clonedCustomer.Age);
      CheckTrue(SameDate(customer.LastEdited, clonedCustomer.LastEdited));
      CheckEquals(customer.Height, clonedCustomer.Height);
      CheckEquals(Ord(customer.CustomerType), Ord(clonedCustomer.CustomerType));
      CheckEquals(customer.MiddleName.Value, clonedCustomer.MiddleName.Value);
      CheckTrue(Assigned(clonedCustomer.CustStream));
      CheckEquals(1, clonedCustomer.OrdersIntf.Count);
      CheckEquals(15, clonedCustomer.OrdersIntf[0].Customer_Payment_Method_Id.Value);
    finally
      clonedCustomer.Free;
    end;
  finally
    customer.Free;
    stream.Free;
  end;
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TRttiExplorerTest.TestCloneSpeed;
var
  LCustomer, LCloned: TCustomer;
  i, iMax: Integer;
  sw: TStopwatch;
  LCustomers: TObjectList<TCustomer>;
  LClonedCustomers: TObjectList<TCustomer>;
  LWorker, LClonedWorker: TWorker;
  LWorkers, LClonedWorkers: TObjectList<TWorker>;
begin
  iMax := 100000;
  LCustomers := TObjectList<TCustomer>.Create(True);
  LClonedCustomers := TObjectList<TCustomer>.Create(True);
  try
    for i := 1 to iMax do
    begin
      LCustomer := TCustomer.Create;
      LCustomer.Age := i;

      LCustomers.Add(LCustomer);
    end;


    sw := TStopwatch.StartNew;

    for i := 0 to LCustomers.Count - 1 do
    begin
      LCustomer := LCustomers[i];

      LCloned := TRttiExplorer.Clone(LCustomer) as TCustomer;
      LClonedCustomers.Add(LCloned);
    end;

    sw.Stop;
  finally
    LCustomers.Free;
    LClonedCustomers.Free;
  end;

  Status(Format('Cloned %D complex objects in %D ms.',
    [iMax, sw.ElapsedMilliseconds]));

  //start cloning simple objects - models without another instances declared in their fields or properties. In this case much faster clone should be used
  LWorkers := TObjectList<TWorker>.Create(True);
  LClonedWorkers := TObjectList<TWorker>.Create(True);
  try
    for i := 1 to iMax do
    begin
      LWorker := TWorker.Create;
      LWorker.TabNr := i;

      LWorkers.Add(LWorker);
    end;

    sw := TStopwatch.StartNew;

    for i := 0 to LWorkers.Count - 1 do
    begin
      LWorker := LWorkers[i];

      LClonedWorker := TRttiExplorer.Clone(LWorker) as TWorker;
      LClonedWorkers.Add(LClonedWorker);
    end;

    sw.Stop;

  finally
    LWorkers.Free;
    LClonedWorkers.Free;
  end;

  Status(Format('Cloned %D simple objects in %D ms.',
    [iMax, sw.ElapsedMilliseconds]));
end;
{$ENDIF}

initialization
  // Register any test cases with the test runner
  RegisterTest(TRttiExplorerTest.Suite);
end.

