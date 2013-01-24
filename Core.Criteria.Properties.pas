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

    function EqProperty(AOther: IProperty): ICriterion; overload;
    function EqProperty(const AOtherPropertyName: string): ICriterion; overload;
    function NeProperty(AOther: IProperty): ICriterion; overload;
    function NeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function GeProperty(AOther: IProperty): ICriterion; overload;
    function GeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function GtProperty(AOther: IProperty): ICriterion; overload;
    function GtProperty(const AOtherPropertyName: string): ICriterion; overload;
    function LeProperty(AOther: IProperty): ICriterion; overload;
    function LeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function LtProperty(AOther: IProperty): ICriterion; overload;
    function LtProperty(const AOtherPropertyName: string): ICriterion; overload;

    function GetPropertyName(): string;
    function GetEntityClass(): TClass;
    procedure SetEntityClass(AClass: TClass);
    procedure SetPropertyName(const Value: string);

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
    FEntityClass: TClass;
  protected
    constructor Create(); virtual;

    class function CreateSQLTable(AEntityClass: TClass): TSQLTable;
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

    function EqProperty(AOther: IProperty): ICriterion; overload; virtual;
    function EqProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function NeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function NeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function GeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function GeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function GtProperty(AOther: IProperty): ICriterion; overload; virtual;
    function GtProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function LeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function LeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function LtProperty(AOther: IProperty): ICriterion; overload; virtual;
    function LtProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;

    function GetEntityClass(): TClass; virtual;
    function GetPropertyName(): string; virtual;
    procedure SetEntityClass(AClass: TClass); virtual;
    procedure SetPropertyName(const Value: string); virtual;
  public
    class function ForName(const APropertyName: string): TProperty;

    property PropertyName: string read GetPropertyName write SetPropertyName;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  {$ENDREGION}
  TProperty<T: class> = class(TProperty)
  public
    class function ForName(const APropertyName: string): TProperty;
  end;

implementation

uses
  Core.Criteria.Restrictions
  ,Core.Criteria.Order
  ,Core.Criteria.Criterion.PropertyExpression
  ,Core.EntityCache
  ,Core.Exceptions
  ;

{ TProperty }

function TProperty.Asc: IOrder;
begin
  Result := TOrder.Asc(FPropertyName);
end;

constructor TProperty.Create;
begin
  inherited Create();
  FEntityClass := nil;
end;

class function TProperty.CreateSQLTable(AEntityClass: TClass): TSQLTable;
var
  LEntityData: TEntityData;
begin
  if AEntityClass = nil then
    Exit(nil);

  LEntityData := TEntityCache.Get(AEntityClass);
  if not LEntityData.IsTableEntity then
    raise ETableNotSpecified.CreateFmt('Entity ("%S") is not a table', [AEntityClass.ClassName]);

  Result := TSQLTable.Create;
  Result.SetFromAttribute(LEntityData.EntityTable);
end;

function TProperty.Desc: IOrder;
begin
  Result := TOrder.Desc(FPropertyName);
end;

function TProperty.Eq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Eq(FPropertyName, AValue);
end;

function TProperty.EqProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woEqual
    , CreateSQLTable(FEntityClass), nil);
end;

function TProperty.EqProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woEqual
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

class function TProperty.ForName(const APropertyName: string): TProperty;
begin
  Result := TProperty.Create();
  Result.FPropertyName := APropertyName;
end;

function TProperty.GeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woMoreOrEqual
    , CreateSQLTable(FEntityClass), nil);
end;

function TProperty.GeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woMoreOrEqual
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

function TProperty.GEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.GEq(FPropertyName, AValue);
end;

function TProperty.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TProperty.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

function TProperty.Gt(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Gt(FPropertyName, AValue);
end;

function TProperty.GtProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woMore
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

function TProperty.GtProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woMore
    , CreateSQLTable(FEntityClass), nil);
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

function TProperty.LeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woLessOrEqual
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

function TProperty.LeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woLessOrEqual
    , CreateSQLTable(FEntityClass), nil);
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

function TProperty.LtProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woLess
    , CreateSQLTable(FEntityClass), nil);
end;

function TProperty.LtProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woLess
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

function TProperty.NeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woNotEqual
    , CreateSQLTable(FEntityClass), CreateSQLTable(AOther.GetEntityClass));
end;

function TProperty.NeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woNotEqual
    , CreateSQLTable(FEntityClass), nil);
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

procedure TProperty.SetEntityClass(AClass: TClass);
begin
  FEntityClass := AClass;
end;

procedure TProperty.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

{ TGenericProperty<T> }

class function TProperty<T>.ForName(const APropertyName: string): TProperty;
begin
  Result := TProperty.ForName(APropertyName);
  Result.FEntityClass := T;
end;

end.
