unit uOrderProcessor;

interface

implementation

uses
       uOrder
     , uOrderInterfaces
     , Spring.Container
     , Spring.Services
     ;

type
  TOrderProcessor = class(TInterfacedObject, IOrderProcessor)
  private
    [Inject]
    FOrderValidator: IOrderValidator;
    FOrderEntry: IOrderEntry;
  public
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

{ TOrderProcessor }

function TOrderProcessor.ProcessOrder(aOrder: TOrder): Boolean;
var
  OrderIsValid: Boolean;
begin
  Result := False;
  OrderIsValid := FOrderValidator.ValidateOrder(aOrder);
  if OrderIsValid then
  begin
    Result := FOrderEntry.EnterOrderIntoDatabase(aOrder);
  end;

  {$IFDEF CONSOLEAPP}
    WriteLn('Order has been processed....');
  {$ENDIF}
end;

initialization
  GlobalContainer.RegisterComponent<TOrderProcessor>.Implements<IOrderProcessor>.InjectField('FOrderEntry');

end.
