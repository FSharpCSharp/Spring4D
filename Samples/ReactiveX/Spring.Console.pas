unit Spring.Console;

interface

type
  Console = record
    class procedure WriteLine(const value: string); static;
  end;

implementation

{ Console }

class procedure Console.WriteLine(const value: string);
begin
  Writeln(value);
end;

end.