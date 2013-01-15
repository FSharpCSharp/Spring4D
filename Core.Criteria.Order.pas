unit Core.Criteria.Order;

interface

uses
  Core.Interfaces
  ,SQL.Types
  ;

type
  TOrder = class(TInterfacedObject, IOrder)
  private
    FOrderType: TOrderType;
    FPropertyName: string;
  protected
    constructor Create(const APropertyName: string; AOrderType: TOrderType); virtual;

    function GetPropertyName(): string; virtual;
    function GetOrderType(): TOrderType; virtual;
  public
    class function Asc(const APropertyName: string): IOrder;
    class function Desc(const APropertyName: string): IOrder;
  end;

implementation

uses
  SysUtils
  ;

{ TOrder }

class function TOrder.Asc(const APropertyName: string): IOrder;
begin
  Result := TOrder.Create(APropertyName, otAscending);
end;

constructor TOrder.Create(const APropertyName: string; AOrderType: TOrderType);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FOrderType := AOrderType;
end;

class function TOrder.Desc(const APropertyName: string): IOrder;
begin
  Result := TOrder.Create(APropertyName, otDescending);
end;

function TOrder.GetOrderType: TOrderType;
begin
  Result := FOrderType;
end;

function TOrder.GetPropertyName: string;
begin
  Result := UpperCase(FPropertyName);
end;

end.
