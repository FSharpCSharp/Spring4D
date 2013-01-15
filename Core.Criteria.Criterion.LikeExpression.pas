unit Core.Criteria.Criterion.LikeExpression;

interface

uses
  Core.Criteria.Criterion.SimpleExpression
  ,Core.Interfaces
  ,SQL.Types
  ,Rtti
  ,SQL.Params
  ,Generics.Collections
  ;

type
  TLikeExpression = class(TSimpleExpression)
  private
    FMatchMode: TMatchMode;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode); reintroduce; overload;

    function ToSqlString(AParams: TObjectList<TDBParam>): string; override;
  end;

implementation

uses
  SysUtils
  ;

{ TLikeExpression }

constructor TLikeExpression.Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode);
begin
  inherited Create(APropertyName, AValue, AOperator);
  FMatchMode := AMatchMode;
end;

function TLikeExpression.ToSqlString(AParams: TObjectList<TDBParam>): string;
begin
  Result := Format('%S %S %S', [UpperCase(PropertyName), WhereOpNames[GetWhereOperator], GetMatchModeString(FMatchMode, Value.AsString)]);
end;

end.
