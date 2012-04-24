program Demo.Spring.Collections.Sorting;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND},
  uCustomer in 'uCustomer.pas',
  uCustomerListFiller in 'uCustomerListFiller.pas',
  uSortCustomers in 'uSortCustomers.pas',
  uCustomerToConsole in 'uCustomerToConsole.pas',
  uSalaryComparer in 'uSalaryComparer.pas',
  uLastNameComparer in 'uLastNameComparer.pas';

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
