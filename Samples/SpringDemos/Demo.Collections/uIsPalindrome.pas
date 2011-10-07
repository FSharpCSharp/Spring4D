unit uIsPalindrome;

interface

function IsPalindrome(const aString: string): Boolean;

implementation

uses
        Spring.Collections
      , {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND}
      ;


function CleanString(const aString: string): string;
var
  C: char;
begin
  // Remove all non-alpha chars and make all lower case
  // Spaces don't matter, so let's count only letters
  Result := '';
  for C in LowerCase(aString) do
  begin
    if CharInSet(C, ['a'..'z', 'A'..'Z']) then
    begin
      Result := Result + C;
    end;
  end;
end;

function IsPalindrome(const aString: string): Boolean;
var
  Stack: IStack<Char>;
  C: Char;
  NoSpaces: string;
  Temp: string;
begin
  NoSpaces :=  CleanString(aString);

  Stack := TCollections.CreateStack<Char>;
  for C in NoSpaces do
  begin
    Stack.Push(C);
  end;
  Temp := '';
  repeat
    Temp := Temp + Stack.Pop;
  until Stack.Count = 0;

  Result := Temp = NoSpaces;

end;

end.
