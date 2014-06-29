unit Core.Criteria.Criterion.Conjunction;

interface

uses
  Core.Criteria.Criterion.Junction
  ,SQL.Types
  ;

type
  TConjunction = class(TJunction)
  protected
    function GetWhereOperator(): TWhereOperator; override;
  end;

implementation

{ TConjunction }

function TConjunction.GetWhereOperator: TWhereOperator;
begin
  Result := woAnd;
end;

end.
