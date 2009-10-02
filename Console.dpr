program Console;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  Classes,
  Windows,
  IOUtils,
  SysUtils,
  Generics.Collections,
  Spring.System,
  Spring.DesignPatterns;

begin
  try

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
