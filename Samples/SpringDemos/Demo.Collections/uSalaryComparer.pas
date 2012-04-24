unit uSalaryComparer;

interface

uses
      uCustomer
    , System.Generics.Defaults
    ;

type
  TSalaryComparer = class(TInterfacedObject, IComparer<TCustomer>)
    function Compare(const Left, Right: TCustomer): Integer;
  end;


implementation

{ TSalaryComparer }

function TSalaryComparer.Compare(const Left, Right: TCustomer): Integer;
begin
  if Left.Salary < Right.Salary then
  begin
    Result := -1
  end else
  begin
     if Left.Salary > Right.Salary then
     begin
       Result := 1;
     end else
     begin
       Result := 0;
     end;
  end;
end;

end.
