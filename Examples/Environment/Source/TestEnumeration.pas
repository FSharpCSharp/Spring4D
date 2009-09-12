unit TestEnumeration;

interface

uses
  Gestures;

type
  TEnumerable<T> = class(TInterfacedObject, IEnumerable<T>, IEnumerable, IInterface)
  public
    function GenericGetEnumerator: IEnumerator<T>;  // Generic
    function IEnumerable<T>.GetEnumerator = GenericGetEnumerator;
  public
    function NonGenericGetEnumerator: IEnumerator;            // NonGeneric
    function IEnumerable.GetEnumerator = NonGenericGetEnumerator;
    function GetEnumerator: IEnumerator<T>;
  end;

var
  s: TEnumerable<string>;

implementation

{ TEnumerable<T> }

function TEnumerable<T>.GenericGetEnumerator: IEnumerator<T>;
begin

end;

function TEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin

end;

function TEnumerable<T>.NonGenericGetEnumerator: IEnumerator;
begin

end;

end.
