unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
     uOrder
     , uOrderInterfaces
     , uOrderValidator
     , uOrderEntry
     , uOrderProcessor;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  Order := TOrder.Create;
//  try
    OrderProcessor := TOrderProcessor.Create(TOrderValidator.Create, TOrderEntry.Create);
    try
      if OrderProcessor.ProcessOrder(Order) then
      begin
        WriteLn('Order successfully processed....');
      end;
//    finally
//      OrderProcessor.Free;
//    end;
  finally
    Order.Free;
  end;
end;

end.
