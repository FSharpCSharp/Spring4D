unit uLastNameComparer;

interface

uses
      uCustomer
    , {$IF CompilerVersion >= 23.0}System.Generics.Defaults{$ELSE}Generics.Defaults{$IFEND}
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
