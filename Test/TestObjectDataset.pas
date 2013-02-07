unit TestObjectDataset;

{$I sv.inc}

interface

uses
  TestFramework, Adapters.ObjectDataset, Spring.Collections, uModels;

type
  TestTObjectDataset = class(TTestCase)
  private
    FDataset: TObjectDataset;
  protected
    function CreateCustomersList(ASize: Integer = 10): IList<TCustomer>; virtual;
    function CreateCustomersOrdersList(ASize: Integer = 10): IList<TCustomer_Orders>; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Open();
    procedure Open_Orders();
    procedure Edit();
    procedure Insert();
    procedure Delete();
    procedure Sort();
    procedure Locate();
    procedure Filter();
    procedure TestGUI;
  end;


implementation

uses
  ViewTestObjectDataset
  ,Forms
  ,DateUtils
  ,DB
  ;


{ TestTObjectDataset }

function TestTObjectDataset.CreateCustomersList(ASize: Integer): IList<TCustomer>;
var
  LCustomer: TCustomer;
  i: Integer;
begin
  Result := TCollections.CreateObjectList<TCustomer>(True);
  for i := 1 to ASize do
  begin
    LCustomer := TCustomer.Create;
    LCustomer.Name := 'FirstName';
    LCustomer.Age := i;
    LCustomer.EMail := 'aaa@aaa.com';
    LCustomer.Height := 100.5;


    Result.Add(LCustomer);
  end;
end;

function TestTObjectDataset.CreateCustomersOrdersList(ASize: Integer): IList<TCustomer_Orders>;
var
  LOrder: TCustomer_Orders;
  i: Integer;
begin
  Result := TCollections.CreateObjectList<TCustomer_Orders>(True);
  for i := 1 to ASize do
  begin
    LOrder := TCustomer_Orders.Create;
    LOrder.Order_Status_Code := 150;
    LOrder.Date_Order_Placed := Today;
    LOrder.Customer_ID := i;

    Result.Add(LOrder);
  end;
end;

procedure TestTObjectDataset.Delete;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);
  LCustomers.Last.Name := 'Foo';
  FDataset.Last;
  FDataset.Prior;
  FDataset.Delete;
  CheckEquals(9, LCustomers.Count);
  CheckEquals(9, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals('Foo', LCustomers.Last.Name);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);

  FDataset.Delete;
  CheckEquals(8, LCustomers.Count);
  CheckEquals(8, FDataset.RecordCount);
end;

procedure TestTObjectDataset.Edit;
var
  LCustomers: IList<TCustomer>;
  LDate: TDateTime;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);
  FDataset.First;
  FDataset.Edit;
  FDataset.FieldByName('Age').AsInteger := 999;
  FDataset.FieldByName('MiddleName').AsString := 'Middle';
  LDate := Today;
  FDataset.FieldByName('LastEdited').AsDateTime := LDate;
  FDataset.Post;
  CheckEquals(999, LCustomers[0].Age);
  CheckEquals('Middle', LCustomers[0].MiddleName);
  CheckEquals(LDate, LCustomers[0].LastEdited);
end;

procedure TestTObjectDataset.Insert;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);

  FDataset.Append;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.FieldByName('Age').AsInteger := 999;
  FDataset.FieldByName('MiddleName').AsString := 'Middle';
  FDataset.Post;

  CheckEquals(11, LCustomers.Count);
  CheckEquals(999, LCustomers.Last.Age);
  CheckEquals('Middle', LCustomers.Last.MiddleName);
  CheckEquals('Foo', LCustomers.Last.Name);
end;

procedure TestTObjectDataset.Locate;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  CheckTrue( FDataset.Locate('Age', 5, []) );
  CheckEquals(5, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(5, LCustomers[FDataset.Index].Age);

  CheckFalse( FDataset.Locate('Age', 50, []) );
end;

procedure TestTObjectDataset.Open;
var
  LCustomers: IList<TCustomer>;
  LNewCustomer: TCustomer;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);
  CheckFalse(FDataset.IsEmpty);
  CheckTrue(FDataset.Active);
  CheckEquals(1, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckEquals(100.5, FDataset.FieldByName('Height').AsFloat, 0.001);
  CheckEquals('aaa@aaa.com', FDataset.FieldByName('EMail').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);

  FDataset.Next;
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);

  LNewCustomer := TCustomer.Create;
  LNewCustomer.Name := 'New';
  LNewCustomer.MiddleName := 'Customer';
  LNewCustomer.Age := 58;
  LCustomers.Add(LNewCustomer);

  CheckEquals(11, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals(58, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('New', FDataset.FieldByName('Name').AsString);
  CheckEquals('Customer', FDataset.FieldByName('MiddleName').AsString);
end;

procedure TestTObjectDataset.Open_Orders;
var
  LOrders: IList<TCustomer_Orders>;
begin
  LOrders := CreateCustomersOrdersList(10);
  FDataset.SetDataList<TCustomer_Orders>(LOrders);
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);
  CheckFalse(FDataset.IsEmpty);
  CheckTrue(FDataset.Active);

  CheckEquals(1, LOrders.First.Customer_ID);
  CheckEquals(Today, LOrders.First.Date_Order_Placed);
  CheckEquals(150, LOrders.First.Order_Status_Code);
  CheckTrue(LOrders.First.Total_Order_Price.IsNull);
end;

procedure TestTObjectDataset.SetUp;
begin
  inherited;
  FDataset := TObjectDataset.Create(nil);
end;

procedure TestTObjectDataset.Sort;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);

  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle';

  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.Filter := 'Age > 1';
  FDataset.Filtered := True;

  FDataset.Sort := 'Age Desc, Name, MIDDLENAME';

//  CheckEquals(10, LCustomers.First.Age);
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);

//  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckEquals('FirstName', LCustomers.Last.Name);
  CheckEquals('Bob', LCustomers[8].Name);
  CheckEquals(9, FDataset.RecordCount);

  FDataset.Filtered := False;

  FDataset.Sort := 'Age Desc, MIDDLENAME, Name';
  CheckEquals('Bob', LCustomers[8].Name);
  CheckEquals('Middle', LCustomers[8].MiddleName);
end;

procedure TestTObjectDataset.TearDown;
begin
  inherited;
  FDataset.Free;
end;

procedure TestTObjectDataset.Filter;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);

 { LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle'; }
  FDataset.Filtered := True;
 // FDataset.FilterOptions := [foCaseInsensitive];
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;


  FDataset.Filter := '(Age = 2)';
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(1, FDataset.RecordCount);
end;

procedure TestTObjectDataset.TestGUI;
var
  LCustomers: IList<TCustomer>;
  LView: TfrmObjectDatasetTest;
begin
  LCustomers := CreateCustomersList(150);
  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  LView := TfrmObjectDatasetTest.Create(nil);
  try
    LView.dsList.DataSet := FDataset;
    FDataset.Sort := 'Age Desc, NAME';
    FDataset.Filtered := True;

    LView.ShowModal;
  finally
    LView.Free;
  end;
end;

initialization
  RegisterTest(TestTObjectDataset.Suite);

end.
