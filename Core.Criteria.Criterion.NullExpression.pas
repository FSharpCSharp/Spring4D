unit Core.Criteria.Criterion.NullExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,SQL.Types
  ,SQL.Params
  ,Generics.Collections
  ;

type
  TNullExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FOperator: TWhereOperator;
  public
    constructor Create(const APropertyName: string; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: TObjectList<TDBParam>): string; override;
    function GetWhereOperator(): TWhereOperator; override;
  end;

implementation

{ TNullExpression }

constructor TNullExpression.Create(const APropertyName: string; const AOperator: TWhereOperator);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FOperator := AOperator;
end;

function TNullExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TNullExpression.ToSqlString(AParams: TObjectList<TDBParam>): string;
begin
  Result := FPropertyName;
end;

end.
