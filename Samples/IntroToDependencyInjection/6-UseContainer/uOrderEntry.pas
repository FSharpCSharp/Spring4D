unit uOrderEntry;

interface

implementation

uses
       uOrder
     , uOrderInterfaces
     , Spring.Container
     ;

type

  TOrderEntry = class(TInterfacedObject, IOrderEntry)
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

{ TOrderEntry }

function TOrderEntry.EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
begin
  Result := aOrder <> nil;
  {$IFDEF CONSOLEAPP}
    WriteLn('Entering order into the database....');
  {$ENDIF}
end;

initialization
  GlobalContainer.RegisterType<TOrderEntry>.Implements<IOrderEntry>;

end.
