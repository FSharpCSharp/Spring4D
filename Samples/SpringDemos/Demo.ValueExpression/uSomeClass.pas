unit uSomeClass;

interface

type
   TSomeClass = class
   private
     FPrivateInt: integer;
    procedure SetPrivateInt(const Value: integer);
   public
     PublicString: string;
     property PrivateInt: integer read FPrivateInt write SetPrivateInt;
   end;


implementation

{ TSomeClass }

procedure TSomeClass.SetPrivateInt(const Value: integer);
begin
  FPrivateInt := Value;
end;

end.
