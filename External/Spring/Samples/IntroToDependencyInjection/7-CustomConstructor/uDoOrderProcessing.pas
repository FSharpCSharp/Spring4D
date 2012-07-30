unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
        uOrder
      , uOrderInterfaces
   //   , uOrderValidator
   //   , uOrderEntry
    //  , uOrderProcessor
      , Spring.Services
      , Spring.Container
      ;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  GlobalContainer.Build;
  Order := TOrder.Create;
  try
    OrderProcessor := ServiceLocator.GetService<IOrderProcessor>;
    if OrderProcessor.ProcessOrder(Order) then
    begin
      WriteLn('Order successfully processed....');
    end;
  finally
    Order.Free;
  end;
end;

end.
