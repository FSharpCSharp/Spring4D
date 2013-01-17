unit TestPersistence;

interface

uses
  TestFramework, Core.Interfaces, Core.Types, uModels, SvSerializer;

type
  PersistenceTests = class(TTestCase)
  private
    FSerializer: TSvSerializer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Serialize_Nullable();
    procedure Serialize_ObjectList();
    procedure TestInterface_Rtti();
  end;

implementation

uses
  SvSerializerSuperJson
  ,SysUtils
  ,Rtti
  ,TypInfo
  ;

{ PersistenceTests }

procedure PersistenceTests.Serialize_Nullable;
var
  LCustomer: TCustomer;
  LOutput: string;
begin
  LCustomer := TCustomer.Create;
  try
    LCustomer.MiddleName.Value := 'Nullable';
    FSerializer.AddObject('', LCustomer);
    FSerializer.Serialize(LOutput, TEncoding.UTF8);

    LCustomer.Free;
    FSerializer.ClearObjects;
    LCustomer := TCustomer.Create;
    FSerializer.AddObject('', LCustomer);
    FSerializer.DeSerialize(LOutput, TEncoding.UTF8);

    CheckFalse(LCustomer.MiddleName.IsNull);
    CheckEquals('Nullable', LCustomer.MiddleName.Value);
    CheckTrue(LCustomer.AvatarLazy.IsNull);
  finally
    LCustomer.Free;
  end;
end;

procedure PersistenceTests.Serialize_ObjectList;
var
  LCustomer: TCustomer;
  LProduct: TProduct;
  LOrder: TCustomer_Orders;
  LOutput: string;
begin
  LCustomer := TCustomer.Create;
  try
    LProduct := TProduct.Create;
    LProduct.Name := 'Sofa';
    LCustomer.Products.Add(LProduct);

    LOrder := TCustomer_Orders.Create;
    LOrder.ORDER_ID := 25;
    LCustomer.OrdersIntf.Add(LOrder);

    FSerializer.AddObject('', LCustomer);
    FSerializer.Serialize(LOutput, TEncoding.UTF8);

    LCustomer.Free;
    FSerializer.ClearObjects;
    LCustomer := TCustomer.Create;
    FSerializer.AddObject('', LCustomer);
    FSerializer.DeSerialize(LOutput, TEncoding.UTF8);

    CheckEquals(0, FSerializer.ErrorCount);

    CheckEquals(1, LCustomer.Products.Count);
    CheckEquals('Sofa', LCustomer.Products[0].Name);
    CheckEquals(1, LCustomer.OrdersIntf.Count);
    CheckEquals(25, LCustomer.OrdersIntf[0].ORDER_ID);
  finally
    LCustomer.Free;
  end;
end;

procedure PersistenceTests.SetUp;
begin
  inherited;
  FSerializer := TSvSerializer.Create(sstSuperJson);
end;

procedure PersistenceTests.TearDown;
begin
  FSerializer.Free;
  inherited;
end;

type
  ITest = interface(IInvokable)
    ['{5EB34AA7-6C35-4030-AC63-4AF9306BE59C}']
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

  TTestas = class(TInterfacedObject, ITest)
  private
    FCount: Integer;
  public
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

  TBean = class
  private
    FTest: ITest;
  protected

  public
    property Test: ITest read FTest write FTest;
  end;

procedure PersistenceTests.TestInterface_Rtti;
var
  LBeanType: TRttiType;
  LBean: TBean;
  LProp: TRttiProperty;
  LValue: TValue;
  LCountMethod: TRttiMethod;
  Lintf: IInterface;
  LInterfaceType: TRttiInterfaceType;
begin
  LBean := TBean.Create;
  try
    LBean.Test := TTestas.Create;
    LBean.Test.Count := 3;
    LBeanType := TRttiContext.Create.GetType(LBean.ClassType);
    LProp := LBeanType.GetProperty('Test');

    LValue := LProp.GetValue(LBean);
    LCountMethod := LProp.PropertyType.GetMethod('GetCount');
    CheckTrue(Assigned(LCountMethod));

    if LProp.PropertyType is TRttiInterfaceType then
    begin
      LInterfaceType := TRttiInterfaceType(LProp.PropertyType);
      if Supports(LValue.AsInterface, LInterfaceType.GUID, Lintf) then
      begin
        LCountMethod := LInterfaceType.GetMethod('SetCount');
        CheckTrue(Assigned(LCountMethod));
        TValue.Make(@Lintf, LInterfaceType.Handle, LValue);
        LCountMethod.Invoke(LValue, [10]);
        LProp.SetValue(LBean, LValue);
        CheckEquals(10, LBean.Test.Count);
      end;
    end;
  finally
    LBean.Free;
  end;
end;

{ TTestas }

function TTestas.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TTestas.SetCount(AValue: Integer);
begin
  FCount := AValue;
end;

initialization
  RegisterTest(PersistenceTests.Suite);

end.
