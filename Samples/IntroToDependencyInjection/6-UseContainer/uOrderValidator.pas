unit uOrderValidator;

interface

implementation

uses
     uOrder
   , uOrderInterfaces
   , Spring.Container
   ;

type

  TOrderValidator = class(TInterfacedObject, IOrderValidator)
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;


{ TOrderValidator }

function TOrderValidator.ValidateOrder(aOrder: TOrder): Boolean;
begin
  Result := aOrder <> nil;
  {$IFDEF CONSOLEAPP}
    WriteLn('Validating Order....');
  {$ENDIF}
end;

initialization
  GlobalContainer.RegisterType<TOrderValidator>.Implements<IOrderValidator>;


end.
