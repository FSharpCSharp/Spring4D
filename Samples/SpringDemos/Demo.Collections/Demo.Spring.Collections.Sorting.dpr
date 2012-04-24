program Demo.Spring.Collections.Sorting;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uCustomer in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uCustomer.pas',
  uCustomerListFiller in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uCustomerListFiller.pas',
  uSortCustomers in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uSortCustomers.pas',
  uCustomerToConsole in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uCustomerToConsole.pas',
  uSalaryComparer in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uSalaryComparer.pas',
  uLastNameComparer in 'C:\Users\nhodges\Documents\RAD Studio\Projects\uLastNameComparer.pas';

begin
  try
    Writeln('Unsorted Customers:');
    UnsortedCustomers;
    WriteLn('--------------------------------');
    WriteLn('Sorting by Salary:');
    SortCustomersBySalary;
    WriteLn('--------------------------------');
    WriteLn('Sorting by Last Name:');
    SortCustomerByLastName
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
