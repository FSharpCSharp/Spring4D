unit Core.Criteria.Criterion.SimpleExpression;

interface

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,Rtti
  ;

type
  TSimpleExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FValue: TValue;
  public
    constructor Create(const APropertyName: string; const AValue: TValue); virtual;
  public
    function ToSqlString(): string; override;
  end;

implementation

{ TSimpleExpression }

constructor TSimpleExpression.Create(const APropertyName: string; const AValue: TValue);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FValue := AValue;
end;

function TSimpleExpression.ToSqlString: string;
begin
  {TODO -oLinas -cGeneral : generate simple expression sql string}
  Result := '';
end;

end.
