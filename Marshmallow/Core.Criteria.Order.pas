unit Core.Criteria.Order;

interface

uses
  Core.Interfaces
  ,SQL.Types
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Implementation of IOrder interface.
  ///	</summary>
  {$ENDREGION}
  TOrder = class(TInterfacedObject, IOrder)
  private
    FOrderType: TOrderType;
    FPropertyName: string;
    FEntityClass: TClass;
  protected
    constructor Create(const APropertyName: string; AOrderType: TOrderType); virtual;

    function GetPropertyName(): string; virtual;
    function GetOrderType(): TOrderType; virtual;

    function GetEntityClass(): TClass; virtual;
    procedure SetEntityClass(AClass: TClass); virtual;
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
  FEntityClass := nil;
end;

class function TOrder.Desc(const APropertyName: string): IOrder;
begin
  Result := TOrder.Create(APropertyName, otDescending);
end;

function TOrder.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TOrder.GetOrderType: TOrderType;
begin
  Result := FOrderType;
end;

function TOrder.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

procedure TOrder.SetEntityClass(AClass: TClass);
begin
  FEntityClass := AClass;
end;

end.


