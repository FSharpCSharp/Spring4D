unit Core.Criteria.Criterion.SimpleExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,Rtti
  ,SQL.Types
  ,SQL.Params
  ,Generics.Collections
  ;

type
  TSimpleExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FValue: TValue;
    FOperator: TWhereOperator;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: TObjectList<TDBParam>): string; override;
    function GetWhereOperator(): TWhereOperator; override;
  end;

implementation

{ TSimpleExpression }

constructor TSimpleExpression.Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FValue := AValue;
  FOperator := AOperator;
end;

function TSimpleExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TSimpleExpression.ToSqlString(AParams: TObjectList<TDBParam>): string;
var
  LParam: TDBParam;
begin
  {TODO -oLinas -cGeneral : generate simple expression sql string}
  Result := FPropertyName;
  LParam := TDBParam.Create();
  LParam.SetFromTValue(FValue);
  LParam.Name := ':' + FPropertyName;
  AParams.Add(LParam);
end;

end.
