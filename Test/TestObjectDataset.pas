unit TestObjectDataset;

{$I sv.inc}

interface

uses
  TestFramework, Adapters.ObjectDataset, Spring.Collections, uModels;

type
  TMockObjectDataset = class(TObjectDataset)

  end;

  TestTObjectDataset = class(TTestCase)
  private
    FDataset: TMockObjectDataset;
  protected
    function CreateCustomersList(ASize: Integer = 10): IList<TCustomer>; virtual;
    function CreateCustomersOrdersList(ASize: Integer = 10): IList<TCustomer_Orders>; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddRecord();
    procedure Append();
    procedure Append_Filtered();
    procedure Bookmark_Filtered();
    procedure Bookmark_Filtered_2();
    procedure Bookmark_Filtered_Fail();
    procedure Bookmark_Simple();
    procedure Bookmark_Sorted();
    procedure ClearField_Nullable();
    procedure ClearField_SimpleType();
    procedure Delete();
    procedure Delete_Filtered();
    procedure Delete_Sorted();
    procedure Delete_Last();
    procedure Edit();
    procedure Eof_AfterLast();
    procedure Eof_AfterNext();
    procedure Filter();
    procedure Filter_DateTime();
    procedure Filter_Null();
    procedure Filter_Performance_Test();
    procedure Insert_Simple();
    procedure Iterating();
    procedure Iterating_Empty();
    procedure Locate();
    procedure MergeSort_Try();
    procedure Open();
    procedure Open_Orders();
    procedure QuickSortTest();
    procedure SimpleSort();
    procedure Sort();
    procedure Sort_Regression();
    procedure TestGUI;
  end;


implementation

uses
  ViewTestObjectDataset
  ,Forms
  ,DateUtils
  ,DB
  ,SysUtils
  ,Generics.Collections
  ,Generics.Defaults
  ,Diagnostics
  ;


{ TestTObjectDataset }

procedure TestTObjectDataset.AddRecord;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.AppendRecord(['Insert', 59]);
  CheckEquals(11, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals('Insert', FDataset.Fields[0].AsString);
  CheckEquals(59, FDataset.Fields[1].AsInteger);
end;

procedure TestTObjectDataset.Append_Filtered;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := 'Age = 1';
  FDataset.Filtered := True;

  CheckEquals(1, FDataset.RecordCount);
  FDataset.Append;
  FDataset.FieldByName('AGE').AsInteger := 1;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.Post;
  CheckEquals(2, FDataset.RecordCount);

  FDataset.Append;
  FDataset.FieldByName('AGE').AsInteger := 2;
  FDataset.FieldByName('Name').AsString := 'Bar';
  FDataset.Post;
  CheckEquals(2, FDataset.RecordCount);
end;

procedure TestTObjectDataset.Bookmark_Filtered;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := '(AGE <= 3)';
  FDataset.Filtered := True;
  CheckEquals(3, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '(AGE >= 3)';
  FDataset.Last;
  CheckEquals(10, FDataset.FieldByName('AGE').AsInteger);
  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TestTObjectDataset.Bookmark_Filtered_2;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := '(AGE <= 3)';
  FDataset.Filtered := True;
  CheckEquals(3, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '';
  CheckEquals(10, FDataset.RecordCount);
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);

  FDataset.Filter := '(AGE > 3)';
  CheckEquals(7, FDataset.RecordCount);
  FDataset.Filter := '';
  CheckEquals(10, FDataset.RecordCount);
  FDataset.First;
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TestTObjectDataset.Bookmark_Filtered_Fail;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '(AGE = 3)';
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount);
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  CheckFalse(FDataset.BookmarkValid(LBookmark));
end;

procedure TestTObjectDataset.Bookmark_Simple;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.Last;
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo);
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;
  FDataset.First;

  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(9, FDataset.RecNo);
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TestTObjectDataset.Bookmark_Sorted;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Last;
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo);
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Sort := 'Age Desc';
  FDataset.First;
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);

  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TestTObjectDataset.ClearField_Nullable;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  LCustomers.First.MiddleName := 'Foo';
  CheckFalse(LCustomers.First.MiddleName.IsNull);

  FDataset.Edit;
  FDataset.FieldByName('MiddleName').Clear;
  FDataset.Post;
  CheckTrue(LCustomers.First.MiddleName.IsNull);
end;

procedure TestTObjectDataset.ClearField_SimpleType;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  LCustomers.First.Age := 1;
  //clear simple type
  CheckEquals(1, LCustomers.First.Age);
  FDataset.Edit;
  FDataset.FieldByName('Age').Clear;
  FDataset.Post;
  CheckEquals(0, LCustomers.First.Age);
  //clear nullable type
end;

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
 // CheckEquals('Foo', LCustomers.Last.Name);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);

  FDataset.Delete;
  CheckEquals(8, LCustomers.Count);
  CheckEquals(8, FDataset.RecordCount);
end;


procedure TestTObjectDataset.Delete_Filtered;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Filtered := True;
  FDataset.Filter := 'Age < 3';
  FDataset.Open;

  CheckEquals(2, FDataset.RecordCount);
  FDataset.Delete;
  CheckEquals(1, FDataset.RecordCount);
  FDataset.Delete;
  CheckEquals(0, FDataset.RecordCount);
  CheckEquals(8, LCustomers.Count);
end;

procedure TestTObjectDataset.Delete_Last;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Last;

  FDataset.Delete;
  CheckEquals(9, FDataset.RecordCount);
  CheckEquals(9, FDataset.RecNo);
  CheckEquals(9, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);

  FDataset.Delete;
  CheckEquals(8, FDataset.RecordCount);
  CheckEquals(8, FDataset.RecNo);
  CheckEquals(8, FDataset.FieldByName('Age').AsInteger);
end;

procedure TestTObjectDataset.Delete_Sorted;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Sort := 'AGE DESC';
  FDataset.RecNo := 5;
  FDataset.Insert;
  FDataset.FieldByName('age').AsInteger := 0;
  FDataset.Post;
  FDataset.Last;
  CheckEquals(0, FDataset.FieldByName('age').AsInteger);
  FDataset.Delete;
  CheckEquals(10, FDataset.RecordCount);
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
  CheckEquals(1, FDataset.RecNo);
  CheckEquals(999, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(999, LCustomers[0].Age);
  CheckEquals('Middle', LCustomers[0].MiddleName);
  CheckEquals(LDate, LCustomers[0].LastEdited);
end;

procedure TestTObjectDataset.Eof_AfterLast;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  CheckFalse(FDataset.Eof);
  CheckTrue(FDataset.Bof);
  FDataset.Last;
  CheckTrue(FDataset.Eof);
  CheckFalse(FDataset.Bof);

end;

procedure TestTObjectDataset.Eof_AfterNext;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Last;
  FDataset.Prior;
  CheckFalse(FDataset.Eof);
  FDataset.Next;
  CheckFalse(FDataset.Eof);
  FDataset.Next;
  CheckTrue(FDataset.Eof);
end;

procedure TestTObjectDataset.Append;
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
  CheckEquals(11, FDataset.RecordCount);
  CheckEquals(11, FDataset.RecNo);
  CheckEquals(999, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);

  CheckEquals(999, LCustomers.Last.Age);
  CheckEquals('Middle', LCustomers.Last.MiddleName);
  CheckEquals('Foo', LCustomers.Last.Name);

  FDataset.Append;
  FDataset.FieldByName('Name').AsString := 'Bar';
  FDataset.FieldByName('Age').AsInteger := 111;
  FDataset.FieldByName('MiddleName').AsString := 'Marley';
  FDataset.Post;
  CheckEquals(12, LCustomers.Count);
  CheckEquals(12, FDataset.RecordCount);
  CheckEquals(12, FDataset.RecNo);
  CheckEquals(111, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Marley', FDataset.FieldByName('MiddleName').AsString);
  CheckEquals('Bar', FDataset.FieldByName('Name').AsString);
end;

procedure TestTObjectDataset.Insert_Simple;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Insert;
  FDataset.FieldByName('Age').AsInteger := 6;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.Post;
  CheckEquals(6, FDataset.RecordCount);
  CheckEquals(1, FDataset.RecNo);
  CheckEquals(6, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);
end;

procedure TestTObjectDataset.Iterating;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  i := 0;

  while not FDataset.Eof do
  begin
    Inc(i);
    CheckEquals(i, FDataset.FieldByName('Age').AsInteger);
    FDataset.Next;
  end;

  CheckEquals(10, i);
end;

procedure TestTObjectDataset.Iterating_Empty;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
begin
  LCustomers := CreateCustomersList(0);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  i := 0;
  while not FDataset.Eof do
  begin
    FDataset.Next;
    Inc(i);
  end;
  CheckEquals(0, i);
end;

procedure TestTObjectDataset.Locate;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  CheckTrue( FDataset.Locate('Age', 5, []) );
  CheckEquals(5, FDataset.RecNo);
  CheckEquals(5, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(5, LCustomers[FDataset.Index].Age);

  CheckFalse( FDataset.Locate('Age', 50, []) );
end;

procedure TestTObjectDataset.MergeSort_Try;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

 // FDataset.MergeSort(0, LCustomers.Count - 1, FDataset.CompareRecords, );

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
  FDataset.IndexList.AddModel(LNewCustomer);
 // LCustomers.Add(LNewCustomer);

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

procedure TestTObjectDataset.QuickSortTest;
var
  LArray: TArray<Integer>;
begin
  LArray := TArray<Integer>.Create(2, 2, 10, 9, 8, 7, 6, 5, 4, 3);
  TArray.Sort<Integer>(LArray, TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      Result := Right - Left;
    end
    )
  );
end;

procedure TestTObjectDataset.SetUp;
begin
  inherited;
  FDataset := TMockObjectDataset.Create(nil);
end;

procedure TestTObjectDataset.SimpleSort;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(3);
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle';

  LCustomers.Last.Name := 'Michael';
  LCustomers.Last.MiddleName := 'Jordan';

  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.Filtered := False;     //2,0,1

  FDataset.Sort := 'Age Desc, MIDDLENAME, Name';
  CheckEquals(3, FDataset.RecordCount);
  CheckEquals(1, FDataset.RecNo);
  CheckEquals('Michael', FDataset.FieldByName('Name').AsString);
  CheckEquals('Jordan', FDataset.FieldByName('MiddleName').AsString);

  FDataset.Next;
  CheckEquals(2, FDataset.RecNo);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);

  FDataset.Sort := 'Age Asc, MIDDLENAME, Name';
  CheckEquals(2, FDataset.RecNo);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);
  FDataset.First;
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);
end;

procedure TestTObjectDataset.Sort;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
  LMsg: string;
begin
  LCustomers := CreateCustomersList(10);

  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle';

  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.Filtered := True;
  FDataset.Filter := 'Age > 2';

  CheckEquals(1, FDataset.RecNo);
  FDataset.Last;
  CheckEquals(8, FDataset.RecNo);

  FDataset.Sort := 'Age Desc, Name, MIDDLENAME';
  FDataset.First;
  CheckEquals(1, FDataset.RecNo);
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', LCustomers.Last.Name);
  CheckEquals(8, FDataset.RecordCount);


  LMsg := '';
  Status(Format('Filter: %S. Sort: %S', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
  begin
    Status(LMsg + Format('%D %S %D', [i, LCustomers[i].Name, LCustomers[i].Age]));
  end;

  FDataset.Filtered := False;

  LMsg := '';
  Status(Format('Filtered false. Sort: %S', [FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
  begin
    Status(LMsg + Format('%D %S %D', [i, LCustomers[i].Name, LCustomers[i].Age]));
  end;

  CheckEquals(10, FDataset.RecordCount);
  FDataset.Sort := 'Age Desc, MiddleName, Name';

  LMsg := '';
  Status(Format('Filter: %S. Sort: %S', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
  begin
    Status(LMsg + Format('%D %S %D %S', [i, LCustomers[i].Name, LCustomers[i].Age, LCustomers[i].MiddleName.ToString]));
  end;

  FDataset.Last;
  CheckEquals(10, FDataset.RecNo);
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo);
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);

  CheckEquals('Bob', LCustomers[8].Name);
  CheckEquals('Middle', LCustomers[8].MiddleName);

  FDataset.Append;
  FDataset.FieldByName('name').AsString := 'aaa';
  FDataset.FieldByName('Age').AsInteger := 15;
  FDataset.Post;

  FDataset.Sort := 'name asc';

  LMsg := '';
  Status(Format('Filter: %S. Sort: %S', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
  begin
    Status(LMsg + Format('%D %S %D %S', [i, LCustomers[i].Name, LCustomers[i].Age, LCustomers[i].MiddleName.ToString]));
  end;

  CheckEquals(11, FDataset.RecNo);
  FDataset.First;
  CheckEquals('aaa', FDataset.FieldByName('Name').AsString);
  FDataset.Next;
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
end;

procedure TestTObjectDataset.Sort_Regression;
var
  LCustomers: IList<TCustomer>;
  LCust: TCustomer;
begin
  LCustomers := TCollections.CreateObjectList<TCustomer>(True);
  {
    0 Bob 2
    1 FirstName 2
    2 FirstName 10
    3 FirstName 9
    4 FirstName 8
    5 FirstName 7
    6 FirstName 6
    7 FirstName 5
    8 FirstName 4
    9 FirstName 3
  }
  //0
  LCust := TCustomer.Create();
  LCust.Name := 'Bob';
  LCust.Age := 2;
  LCustomers.Add(LCust);
  //1
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 2;
  LCustomers.Add(LCust);
  //2
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 10;
  LCustomers.Add(LCust);
  //3
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 9;
  LCustomers.Add(LCust);
  //4
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 8;
  LCustomers.Add(LCust);
  //5
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 7;
  LCustomers.Add(LCust);
  //6
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 6;
  LCustomers.Add(LCust);
  //7
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 5;
  LCustomers.Add(LCust);
  //8
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 4;
  LCustomers.Add(LCust);
  //9
  LCust := TCustomer.Create();
  LCust.Name := 'FirstName';
  LCust.Age := 3;
  LCustomers.Add(LCust);

  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;

  FDataset.Sort := 'Age Desc';

  CheckEquals(2, LCustomers.Last.Age);
  CheckEquals(2, LCustomers[8].Age);

  FDataset.Last;
  CheckEquals(10, FDataset.RecNo);
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo);
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);



 // CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
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
  FDataset.Filtered := True;
  FDataset.FilterOptions := [foCaseInsensitive];
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;


  FDataset.Filter := '(Age = 2)';
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(1, FDataset.RecordCount);

  FDataset.Filter := '(Age > 2)';
  CheckEquals(8, FDataset.RecordCount);
end;

procedure TestTObjectDataset.Filter_DateTime;
var
  LCustomers: IList<TCustomer>;
  LDate: TDate;
begin
  LCustomers := CreateCustomersList(10);
  LDate := EncodeDate(2013,1,1);
  LCustomers.Last.LastEdited := LDate;

  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := '(LastEdited = ''2013-01-01'')';
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount);
end;

procedure TestTObjectDataset.Filter_Null;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  LCustomers.Last.MiddleName := 'Foo';
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := '(MiddleName <> Null)';
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount);
  FDataset.Filter := '(MiddleName = Null)';
  CheckEquals(4, FDataset.RecordCount);
end;

procedure TestTObjectDataset.Filter_Performance_Test;
var
  LCustomers: IList<TCustomer>;
  sw: TStopwatch;
begin
  LCustomers := CreateCustomersList(50000);
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  FDataset.Filter := '((AGE = 2)) OR ((AGE = 3)) OR ((AGE = 4)) OR ((AGE = 1)) OR ((AGE = 100)) OR ((AGE = 1000)) OR ((AGE = 999)) OR ((AGE = -1))';
  sw := TStopwatch.StartNew;
  FDataset.Filtered := True;
  sw.Stop;
  CheckEquals(7, FDataset.RecordCount);
  Status(Format('%D records in %D ms', [LCustomers.Count, sw.ElapsedMilliseconds]));
end;

procedure TestTObjectDataset.TestGUI;
var
  LCustomers: IList<TCustomer>;
  LView: TfrmObjectDatasetTest;
  sw: TStopwatch;
begin
  LCustomers := CreateCustomersList(10);
  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  FDataset.SetDataList<TCustomer>(LCustomers);
  FDataset.Open;
  LView := TfrmObjectDatasetTest.Create(nil);
  try
    LView.Dataset := FDataset;
    LView.dsList.DataSet := FDataset;
    sw := TStopwatch.StartNew;
  //  FDataset.Sort := 'Age Desc, NAME';
    FDataset.Filtered := True;
    sw.Stop;
    Status(Format('Elapsed time: %D ms', [sw.ElapsedMilliseconds]));
    LView.ShowModal;
  finally
    LView.Free;
  end;
end;

initialization
  RegisterTest(TestTObjectDataset.Suite);

end.
