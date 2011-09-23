program Demo.Spring.Enum;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uEnumDemo in 'uEnumDemo.pas';

begin
  try
    DoEnumDemo;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
