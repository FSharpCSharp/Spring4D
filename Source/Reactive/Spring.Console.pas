unit Spring.Console;

interface

type
  Console = record
    class procedure WriteLine(const value: string); overload; static;
    class procedure WriteLine<T>(const value: T); overload; static;
  end;

procedure WritelnFmt(const text: string; const args: array of const);

implementation

uses
  Rtti,
  SysUtils;


{$REGION 'Console'}

class procedure Console.WriteLine(const value: string);
begin
  Writeln(value);
end;

class procedure Console.WriteLine<T>(const value: T);
begin
  Writeln(TValue.From<T>(value).ToString);
end;

procedure WritelnFmt(const text: string; const args: array of const);
begin
  Writeln(Format(text, args));
end;

{$ENDREGION}


end.
