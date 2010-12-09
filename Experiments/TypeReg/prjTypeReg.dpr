program prjTypeReg;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uTypeReg in 'uTypeReg.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
