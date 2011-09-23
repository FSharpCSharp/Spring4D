program Demo.Spring.ObjectContainer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Spring.Services,
  uYeller in 'uYeller.pas';

var
  Yeller: TYeller;
begin
  try
    Yeller := ServiceLocator.GetService<TYeller>;
    Yeller.Yell;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
