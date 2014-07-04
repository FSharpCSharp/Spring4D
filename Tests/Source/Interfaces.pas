unit Interfaces;

interface

uses
  Generics.Defaults;

type
  IAnsweringEngine = interface(IInvokable)
    ['{D3B8E1FF-9B51-43FA-BF70-3AAC9B742E26}']
    function GetAnswer(s: string): Integer;
  end;

  IDeepThought = interface
    ['{FD310D32-A8FB-429E-A753-E7C0824AF777}']
    procedure SetAnsweringEngine(const answeringEngine: IAnsweringEngine);
  end;

  {$M+}
  IEqualityComparer<T> = interface//(Generics.Defaults.IEqualityComparer<T>)
    // DO NOT ADD ANY METHODS HERE!!!
    function Equals(const Left, Right: T): Boolean;
    function GetHashCode(const Value: T): Integer;
  end;
  {$M-}

implementation

end.
