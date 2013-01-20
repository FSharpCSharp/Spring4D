unit Core.Criteria.Properties;

interface

uses
  Core.Interfaces
  ,Rtti
  ,SQL.Types
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  ///	<remarks>
  ///	  For detailed methods documentation look in
  ///	  <see cref="Core.Criteria.Restrictions|TRestrictions" />.
  ///	</remarks>
  {$ENDREGION}
  IProperty = interface(IInvokable)
    ['{2F58C81C-4817-43E7-BA3F-7570FE2A6823}']
    function Eq(const AValue: TValue): ICriterion;
    function NotEq(const AValue: TValue): ICriterion;
    function GEq(const AValue: TValue): ICriterion;
    function Gt(const AValue: TValue): ICriterion;
    function IsNull(): ICriterion;
    function IsNotNull(): ICriterion;
    function Like(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;
    function NotLike(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;
    function LEq(const AValue: TValue): ICriterion;
    function Lt(const AValue: TValue): ICriterion;
    function &InStr(const AValues: TArray<string>): ICriterion;
    function NotInStr(const AValues: TArray<string>): ICriterion;
    function &InInt(const AValues: TArray<Integer>): ICriterion;
    function NotInInt(const AValues: TArray<Integer>): ICriterion;

    function Asc(): IOrder;
    function Desc(): IOrder;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  {$ENDREGION}
  TProperty = class(TInterfacedObject, IProperty)
  private
    FPropertyName: string;
  protected
    constructor Create(); virtual;
  protected
    function Eq(const AValue: TValue): ICriterion; virtual;
    function NotEq(const AValue: TValue): ICriterion; virtual;
    function GEq(const AValue: TValue): ICriterion; virtual;
    function Gt(const AValue: TValue): ICriterion; virtual;
    function IsNull(): ICriterion; virtual;
    function IsNotNull(): ICriterion; virtual;
    function Like(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion; virtual;
    function NotLike(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion; virtual;
    function LEq(const AValue: TValue): ICriterion; virtual;
    function Lt(const AValue: TValue): ICriterion; virtual;
    function &In<T>(const AValues: TArray<T>): ICriterion;
    function NotIn<T>(const AValues: TArray<T>): ICriterion;
    function &InStr(const AValues: TArray<string>): ICriterion; virtual;
    function NotInStr(const AValues: TArray<string>): ICriterion; virtual;
    function &InInt(const AValues: TArray<Integer>): ICriterion; virtual;
    function NotInInt(const AValues: TArray<Integer>): ICriterion; virtual;
    function Asc(): IOrder; virtual;
    function Desc(): IOrder; virtual;
  public
    class function ForName(const APropertyName: string): TProperty;

    property PropertyName: string read FPropertyName write FPropertyName;
  end;

implementation

uses
  Core.Criteria.Restrictions
  ,Core.Criteria.Order
  ;

{ TProperty }

function TProperty.Asc: IOrder;
begin
  Result := TOrder.Asc(FPropertyName);
end;

constructor TProperty.Create;
begin
  inherited Create();
end;

function TProperty.Desc: IOrder;
begin
  Result := TOrder.Desc(FPropertyName);
end;

function TProperty.Eq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Eq(FPropertyName, AValue);
end;

class function TProperty.ForName(const APropertyName: string): TProperty;
begin
  Result := TProperty.Create();
  Result.FPropertyName := APropertyName;
end;

function TProperty.GEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.GEq(FPropertyName, AValue);
end;

function TProperty.Gt(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Gt(FPropertyName, AValue);
end;

function TProperty.&In<T>(const AValues: TArray<T>): ICriterion;
begin
  Result := TRestrictions.&In<T>(FPropertyName, AValues);
end;

function TProperty.InInt(const AValues: TArray<Integer>): ICriterion;
begin
  Result := &In<Integer>(AValues);
end;

function TProperty.InStr(const AValues: TArray<string>): ICriterion;
begin
  Result := &In<string>(AValues);
end;

function TProperty.IsNotNull: ICriterion;
begin
  Result := TRestrictions.IsNotNull(FPropertyName);
end;

function TProperty.IsNull: ICriterion;
begin
  Result := TRestrictions.IsNull(FPropertyName);
end;

function TProperty.LEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.LEq(FPropertyName, AValue);
end;

function TProperty.Like(const AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.Like(FPropertyName, AValue, AMatchMode);
end;

function TProperty.Lt(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Lt(FPropertyName, AValue);
end;

function TProperty.NotEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.NotEq(FPropertyName, AValue);
end;

function TProperty.NotIn<T>(const AValues: TArray<T>): ICriterion;
begin
  Result := TRestrictions.NotIn<T>(FPropertyName, AValues);
end;

function TProperty.NotInInt(const AValues: TArray<Integer>): ICriterion;
begin
  Result := NotIn<Integer>(AValues);
end;

function TProperty.NotInStr(const AValues: TArray<string>): ICriterion;
begin
  Result := NotIn<string>(AValues);
end;

function TProperty.NotLike(const AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.NotLike(FPropertyName, AValue, AMatchMode);
end;

end.
