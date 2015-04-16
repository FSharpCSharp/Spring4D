unit TestMapping.RttiExplorer;

interface

uses
  TestFramework,
  Rtti,
  TestEntities,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.Mapping.RttiExplorer;

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
    procedure GetPrimaryKey;
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
  CheckEqualsString('CUSTID', TRttiExplorer.GetPrimaryKeyColumn(FCustomer.ClassType).ColumnName);
  CheckEqualsString('FId', TRttiExplorer.GetPrimaryKeyColumn(FCustomer.ClassType).Member.Name);
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
  CheckEqualsString('FId', ReturnValue.First.Member.Name);
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

initialization
  RegisterTest(TRttiExplorerTest.Suite);
end.

