unit uLastNameComparer;

interface

uses
      uCustomer
    , System.Generics.Defaults
    ;

type
  TLastNameComparer = class(TInterfacedObject, IComparer<TCustomer>)
    function Compare(const Left, Right: TCustomer): Integer;
  end;

implementation

uses
     SysUtils
   ;

{ TLastNameComparer }

function TLastNameComparer.Compare(const Left, Right: TCustomer): Integer;
begin
  Result := CompareStr(Left.LastName, Right.LastName);
end;

end.
