unit HasCount;

interface

type
  IHasCount = interface
    ['{E76AAB8E-B78E-4AA6-96D4-01F92FBE6268}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

implementation

end.
