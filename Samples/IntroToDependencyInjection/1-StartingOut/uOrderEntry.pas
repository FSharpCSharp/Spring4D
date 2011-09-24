unit uOrderEntry;

interface

uses
    uOrder;

type
  TOrderEntry = class
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderEntry }

function TOrderEntry.EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
begin
  Result := aOrder <> nil;
  {$IFDEF CONSOLEAPP}
    WriteLn('Entering order into the database....');
  {$ENDIF}
end;

end.
