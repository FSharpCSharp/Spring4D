unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
  uOrder,
  uOrderProcessor,
  uOrderValidator,
  uOrderEntry;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: TOrderProcessor;
begin
  Order := TOrder.Create;
  try
    OrderProcessor := TOrderProcessor.Create(TOrderValidator.Create, TOrderEntry.Create);
    try
      if OrderProcessor.ProcessOrder(Order) then
      begin
        {$IFDEF CONSOLEAPP}
        Writeln('Order successfully processed....');
        {$ENDIF}
      end;
    finally
      OrderProcessor.Free;
    end;
  finally
    Order.Free;
  end;
end;

end.
