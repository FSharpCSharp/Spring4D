unit CustomersViewModel;

interface

uses
  Customer,
  DSharp.Core.PropertyChangedBase,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  TCustomersViewModel = class(TPropertyChangedBase)
  private
    fCustomers: IObjectList;
    fCustomerId: string;
    fCustomerRepository: IPagedRepository<TCustomer,string>;
  public
    constructor Create(const customerRespository: IPagedRepository<TCustomer,string>);

    procedure LoadCustomers(Sender: TObject);
    property Customers: IObjectList read fCustomers;
    property CustomerId: string read fCustomerId write fCustomerId;
  end;

implementation

{ TCustomersViewModel }

constructor TCustomersViewModel.Create(
  const customerRespository: IPagedRepository<TCustomer, string>);
begin
  inherited Create;
  fCustomerRepository := customerRespository;
end;

procedure TCustomersViewModel.LoadCustomers(Sender: TObject);
begin
  // load customers
  if fCustomerId = '' then
    fCustomers := fCustomerRepository.FindAll as IObjectList
  else
  begin
    fCustomers := TCollections.CreateList<TCustomer>(True) as IObjectList;
    fCustomers.Add(fCustomerRepository.FindOne(fCustomerId));
  end;

  NotifyOfPropertyChange('Customers');
end;

end.
