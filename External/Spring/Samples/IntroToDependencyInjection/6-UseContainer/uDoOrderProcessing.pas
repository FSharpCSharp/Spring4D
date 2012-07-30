unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
        uOrder
      , Spring.Services
      , Spring.Container
      , uOrderInterfaces
   //   , uOrderValidator
   //   , uOrderEntry
      , uOrderProcessor
      ;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;

  OrderValidator: IOrderValidator;
  OrderEntry: IOrderEntry;
begin
  GlobalContainer.Build;
  Order := TOrder.Create;
  try
    OrderValidator := ServiceLocator.GetService<IOrderValidator>;
    OrderEntry := ServiceLocator.GetService<IOrderEntry>;
    OrderProcessor := TOrderProcessor.Create(OrderValidator, OrderEntry);
    if OrderProcessor.ProcessOrder(Order) then
    begin
      WriteLn('Order successfully processed....');
    end;
  finally
    Order.Free;
  end;
end;

end.
