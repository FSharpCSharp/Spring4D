unit TestEntityWrapper;

interface

uses
  TestFramework,
  TestEntities,
  Rtti,
  Spring.TestUtils,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Collections
  ;

type
  TPair = class
  strict private
    fName: string;
    fValue: Variant;
  public
    constructor Create(const name: string; const value: Variant);

    property Name: string read FName;
    property Value: Variant read FValue;
  end;

  TResultSetListAdapter = class(TInterfacedObject, IDBResultset)
  private
    fValues: IList<TPair>;
    fCurrent: Integer;
  protected
    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldNameExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldname: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  public
    constructor Create(const values: IList<TPair>);
  end;

  TTestEntityWrapper = class(TTestCase)
  private
    fCustomer: TCustomer;
    fSut: TEntityWrapper;
  protected
    function GetCustomerResultSet(id: Variant; const name: String; age: Integer; middleName: string = ''): IDBResultSet;
    function GetEmptyResultSet: IDBResultSet;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IsAssigned;
    procedure When_ResultSet_HasNoPrimaryKey_GetPrimaryKey_ThrowsException;
    procedure When_ResultSet_Has_PrimaryKey_GetPrimaryKey;
    procedure When_ResultSet_HasNoColumns_GetColumnValue_ThrowsException;
    procedure When_ResultSet_HasColumns_GetColumnValue_Returns_CorrectValue;
    procedure SetPrimaryKeyToEntity_Successfully;
    procedure SetPrimaryKeyToEntity_Exception;
    procedure SetMemberValue_Name;
  end;


implementation

uses
  SysUtils,
  Variants,
  TestConsts,
  Spring.Persistence.Core.Exceptions
  ;

{ TTestEntityWrapper }

function TTestEntityWrapper.GetCustomerResultSet(id: Variant;
  const name: String; age: Integer; middleName: string): IDBResultSet;
var
  values: IList<TPair>;
begin
  values := TCollections.CreateObjectList<TPair>;
  if not VarIsNull(id) then
    values.Add(TPair.Create(CUSTID, id));
  values.Add(TPair.Create(CUSTNAME, name));
  values.Add(TPair.Create(CUSTAGE, age));
  values.Add(TPair.Create(CUST_MIDDLENAME, middleName));
  Result := TResultSetListAdapter.Create(values);
end;

function TTestEntityWrapper.GetEmptyResultSet: IDBResultSet;
begin
  Result := TResultSetListAdapter.Create(TCollections.CreateObjectList<TPair>);
end;

procedure TTestEntityWrapper.IsAssigned;
begin
  CheckTrue(Assigned(fSut));
end;

procedure TTestEntityWrapper.SetMemberValue_Name;
begin
  fSut.SetMemberValue('Name', 'Foo');
  CheckEquals('Foo', fCustomer.Name);
end;

procedure TTestEntityWrapper.SetPrimaryKeyToEntity_Exception;
begin
  CheckException(EInvalidCast, procedure begin fSut.SetPrimaryKeyValue(1.01); end);
end;

procedure TTestEntityWrapper.SetPrimaryKeyToEntity_Successfully;
begin
  fSut.SetPrimaryKeyValue(1);
  CheckEquals(1, fCustomer.ID);
end;

procedure TTestEntityWrapper.SetUp;
begin
  inherited;
  fCustomer := TCustomer.Create;
  fSut := TEntityWrapper.Create(fCustomer);
end;

procedure TTestEntityWrapper.TearDown;
begin
  inherited;
  fCustomer.Free;
  fSut.Free;
end;

procedure TTestEntityWrapper.When_ResultSet_HasColumns_GetColumnValue_Returns_CorrectValue;
begin
  CheckEquals('Foo', fSut.GetColumnValueFrom(GetCustomerResultSet(1, 'Foo', 10), CUSTNAME).AsString, 'Name should be Foo');
  CheckEquals(10, fSut.GetColumnValueFrom(GetCustomerResultSet(1, 'Foo', 10), CUSTAGE).AsInteger, 'Age should be 10');
end;

procedure TTestEntityWrapper.When_ResultSet_HasNoColumns_GetColumnValue_ThrowsException;
begin
  CheckException(EORMColumnNotFound,
    procedure
    begin
      fSut.GetColumnValueFrom(GetEmptyResultSet, CUSTNAME);
    end);
end;

procedure TTestEntityWrapper.When_ResultSet_HasNoPrimaryKey_GetPrimaryKey_ThrowsException;
begin
  CheckException(EORMPrimaryKeyColumnNotFound,
    procedure
    begin
      fSut.GetPrimaryKeyValueFrom(GetCustomerResultSet(Null, 'Foo', 10));
    end);
end;

procedure TTestEntityWrapper.When_ResultSet_Has_PrimaryKey_GetPrimaryKey;
begin
  CheckEquals(1, fSut.GetPrimaryKeyValueFrom(GetCustomerResultSet(1, 'Foo', 10)).AsInteger);
end;

{ TMockResultSet }

constructor TResultSetListAdapter.Create(const values: IList<TPair>);
begin
  inherited Create;
  fValues := values;
  fCurrent := 0;
end;

function TResultSetListAdapter.FieldNameExists(const fieldName: string): Boolean;
var
  pair: TPair;
begin
  for pair in fValues do
  begin
    if SameText(fieldname, pair.Name) then
      Exit(True);
  end;
  Result := False;
end;

function TResultSetListAdapter.GetFieldCount: Integer;
begin
  Result := fValues.Count;
end;

function TResultSetListAdapter.GetFieldName(index: Integer): string;
begin
  Result := fValues[index].Name;
end;

function TResultSetListAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := fValues[index].Value;
end;

function TResultSetListAdapter.GetFieldValue(const fieldname: string): Variant;
var
  pair: TPair;
begin
  for pair in fValues do
  begin
    if SameText(fieldname, pair.Name) then
      Exit(pair.Value);
  end;
  raise EORMColumnNotFound.CreateFmt('Column %s not found in the resultSet', [fieldname]);
end;

function TResultSetListAdapter.IsEmpty: Boolean;
begin
  Result := fCurrent > 0;
  Inc(fCurrent);
end;

function TResultSetListAdapter.Next: Boolean;
begin
  Result := IsEmpty;
end;

{ TPair }

constructor TPair.Create(const name: string; const value: Variant);
begin
  fName := name;
  fValue := value;
end;

initialization
  RegisterTest(TTestEntityWrapper.Suite);

end.
