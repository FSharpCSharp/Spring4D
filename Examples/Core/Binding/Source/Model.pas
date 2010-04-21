unit Model;

interface

uses
  Spring.System,
  Spring.Notifications,
  Spring.Collections,
  Generics.Collections;

type
  TCustomer = class;
  TAddress = class;
  TCategory = class;
  TProduct = class;
  TOrder = class;
  TOrderLineItem = class;

  {
     Entity Objects: TCategory, TProduct, TCustomer, TOrder
     Value Objects: TOrderLineItem, TAddress (?)

     TCustomer -> (1) TAddress
     TCategory (1) - (*) TProduct
     TCustomer (1) - (*) TOrder (1) - (*) TOrderLineItem -> (1) TProduct

  }

  TDomainObject = TNotifiableObject;

  TCustomer = class(TDomainObject)
  private
    fName: string;
    fBirthDate: TNullableDateTime;
    fAddress: TAddress;
    function GetAge: TNullableInteger;
    procedure SetName(const value: string);
    procedure SetBirthDate(const Value: TNullableDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read fName write SetName;
    property Age: TNullableInteger read GetAge;
    property BirthDate: TNullableDateTime read fBirthDate write SetBirthDate;
    property Address: TAddress read fAddress;
  end;

  TAddress = class(TDomainObject)
  private
    fStreet: TNullableString;
    fCity: TNullableString;
    fPostalCode: TNullableString;
    procedure SetCity(const value: TNullableString);
    procedure SetPostalCode(const value: TNullableString);
    procedure SetStreet(const value: TNullableString);
  public
    property Street: TNullableString read fStreet write SetStreet;
    property City: TNullableString read fCity write SetCity;
    property PostalCode: TNullableString read fPostalCode write SetPostalCode;
  end;

  TCategory = class(TDomainObject)
  private
    fName: string;
    procedure SetName(const value: string);
  protected
//    procedure AddProduct(product: TProduct);
//    procedure RemoveProduct(product: TProduct);
  public
    property Name: string read fName write SetName;
//    property Products: IList<TProduct>;
//    class property Uncategorized: TCategory read fUncategorized;
  end;

  TProduct = class(TDomainObject)
  private
    fName: string;
    fCategory: TCategory;
    fUnitPrice: Currency;
    procedure SetName(const value: string);
    procedure SetCategory(const value: TCategory);
    procedure SetUnitPrice(const value: Currency);
  public
    property Name: string read fName write SetName;
    property Category: TCategory read fCategory write SetCategory;
    property UnitPrice: Currency read fUnitPrice write SetUnitPrice;
  end;

  TOrderLineItem = class(TDomainObject)
  private
    fProduct: TProduct;
    fQuantity: Integer;
    fUnitPrice: Currency;
    function GetSubTotal: Currency;
  public
    constructor Create(product: TProduct; quantity: Integer; price: Currency);
    property Product: TProduct read fProduct;
    property Quantity: Integer read fQuantity;
    property UnitPrice: Currency read fUnitPrice;
    property SubTotal: Currency read GetSubTotal;
  end;

  TOrder = class(TDomainObject)
  private
    fCustomer: TCustomer;
    fOrderNo: string;
    fOrderDate: TNullableDateTime;
    fLineItems: IList<TOrderLineItem>;
    // fTotal: TNullable<Currency>;
    procedure SetOrderNo(const value: string);
    procedure SetOrderDate(const value: TNullableDateTime);
    procedure SetCustomer(const value: TCustomer);
  protected
    function GetLineItems: IList<TOrderLineItem>; virtual;
    function GetTotal: Currency; virtual;
  public
    function AddLineItem(product: TProduct; quantity: Integer): TOrderLineItem; overload;
    function AddLineItem(product: TProduct; quantity: Integer; unitPrice: Currency): TOrderLineItem; overload;
    procedure RemoveLineItem(lineItem: TOrderLineItem);
    property Customer: TCustomer read fCustomer write SetCustomer;
    property OrderNo: string read fOrderNo write SetOrderNo;
    property OrderDate: TNullableDateTime read fOrderDate write SetOrderDate;
    property LineItems: IList<TOrderLineItem> read GetLineItems;
    property Total: Currency read GetTotal; // SetTotal
  end;

implementation

uses
  DateUtils;

{ TCustomer }

constructor TCustomer.Create;
begin
  inherited Create;
  fAddress := TAddress.Create;
end;

destructor TCustomer.Destroy;
begin
  fAddress.Free;
  inherited;
end;

function TCustomer.GetAge: TNullableInteger;
begin
  if BirthDate.HasValue then
    Result := TNullableInteger.Create(YearOf(Today) - YearOf(BirthDate))
  else
    Result := nil;
end;

procedure TCustomer.SetName(const value: string);
begin
  SetProperty<string>('Name', fName, value);
end;

procedure TCustomer.SetBirthDate(const Value: TNullableDateTime);
begin
  SetProperty<TNullableDateTime>('BirthDate', fBirthDate, value);
end;

{ TAddress }

procedure TAddress.SetCity(const value: TNullableString);
begin
  SetProperty<TNullableString>('City', fCity, value);
end;

procedure TAddress.SetPostalCode(const value: TNullableString);
begin
  SetProperty<TNullableString>('PostalCode', fPostalCode, value);
end;

procedure TAddress.SetStreet(const value: TNullableString);
begin
  SetProperty<TNullableString>('Street', fStreet, value);
end;

{ TCategory }

procedure TCategory.SetName(const value: string);
begin
  SetProperty<string>('Name', fName, value);
end;

{ TProduct }

procedure TProduct.SetCategory(const value: TCategory);
begin
  SetProperty<TCategory>('Category', fCategory, value);
end;

procedure TProduct.SetName(const value: string);
begin
  SetProperty<string>('Name', fName, value);
end;

procedure TProduct.SetUnitPrice(const value: Currency);
begin
  SetProperty<Currency>('UnitPrice', fUnitPrice, value);
end;

{ TOrder }

function TOrder.AddLineItem(product: TProduct;
  quantity: Integer): TOrderLineItem;
begin
  TArgument.CheckNotNull(product, 'product');
  Result := AddLineItem(product, quantity, product.UnitPrice);
end;

function TOrder.AddLineItem(product: TProduct; quantity: Integer;
  unitPrice: Currency): TOrderLineItem;
begin
  TArgument.CheckNotNull(product, 'product');
  Result := TOrderLineItem.Create(product, quantity, unitPrice);
  fLineItems.Add(Result);
end;

procedure TOrder.RemoveLineItem(lineItem: TOrderLineItem);
begin
  TArgument.CheckNotNull(lineItem, 'lineItem');
  LineItems.Remove(lineItem);
end;

function TOrder.GetLineItems: IList<TOrderLineItem>;
begin
  if fLineItems = nil then
  begin
    fLineItems := TObjectNotifiableCollection<TOrderLineItem>.Create;
  end;
  Result := fLineItems;
end;

function TOrder.GetTotal: Currency;
var
  item: TOrderLineItem;
begin
  if fLineItems = nil then
  begin
    Exit(0);
  end;
  Result := 0;
  for item in fLineItems do
  begin
    Result := Result + item.SubTotal;
  end;
end;

procedure TOrder.SetOrderNo(const value: string);
begin
  SetProperty<string>('OrderNo', fOrderNo, value);
end;

procedure TOrder.SetOrderDate(const value: TNullableDateTime);
begin
  SetProperty<TNullableDateTime>('OrderDate', fOrderDate, value);
end;

procedure TOrder.SetCustomer(const value: TCustomer);
begin
  SetProperty<TCustomer>('Customer', fCustomer, value);
end;

{ TOrderLineItem }

constructor TOrderLineItem.Create(
  product: TProduct; quantity: Integer; price: Currency);
begin
  inherited Create;
  fProduct := product;
  fQuantity := quantity;
  fUnitPrice := price;
end;    

function TOrderLineItem.GetSubTotal: Currency;
begin
  Result := UnitPrice * Quantity;
end;

end.
