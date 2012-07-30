unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
   uOrder, uOrderValidator, uOrderEntry, uOrderProcessor;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  Order := TOrder.Create;
  OrderProcessor := TOrderProcessor.Create;
  try
    if OrderProcessor.ProcessOrder(Order) then
    begin
      WriteLn('Order successfully processed....');
    end;
  finally
    Order.Free;
  end;
end;

end.
