unit uOrderValidator;

interface

uses
  uOrder, uOrderInterfaces;

type

  TOrderValidator = class(TInterfacedObject, IOrderValidator)
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderValidator }

function TOrderValidator.ValidateOrder(aOrder: TOrder): Boolean;
begin
  Result := aOrder <> nil;
  {$IFDEF CONSOLEAPP}
    WriteLn('Validating Order....');
  {$ENDIF}
end;

end.
