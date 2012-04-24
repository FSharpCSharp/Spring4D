unit uSortCustomers;

interface

procedure UnsortedCustomers;
procedure SortCustomersBySalary;
procedure SortCustomerByLastName;

implementation

uses
       uCustomer
     , uCustomerListFiller
     , uCustomerToConsole
     , uSalaryComparer
     , uLastNameComparer
     , Spring.Collections
     , Spring.Collections.Lists
     , System.Generics.Defaults
     ;

procedure UnsortedCustomers;
var
  Customers: IList<TCustomer>;
begin
  Customers := TCollections.CreateObjectList<TCustomer>(True);
  FillListWithCustomers(Customers);
  WriteCustomersToConsole(Customers);
end;

procedure SortCustomersBySalary;
var
  Customers: IList<TCustomer>;
  SalaryComparer: IComparer<TCustomer>;
begin
  SalaryComparer := TSalaryComparer.Create;
  Customers := TCollections.CreateObjectList<TCustomer>(True, SalaryComparer);
  FillListWithCustomers(Customers);
  Customers.Sort;
  WriteCustomersToConsole(Customers);
end;

procedure SortCustomerByLastName;
var
  Customers: IList<TCustomer>;
  LastNameComparer: IComparer<TCustomer>;
begin
  LastNameComparer := TLastNameComparer.Create;
  Customers := TCollections.CreateObjectList<TCustomer>(True, LastNameComparer);
  FillListWithCustomers(Customers);
  Customers.Sort;
  WriteCustomersToConsole(Customers);
end;


end.
