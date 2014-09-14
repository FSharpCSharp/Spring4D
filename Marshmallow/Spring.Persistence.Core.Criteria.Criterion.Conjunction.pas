unit Spring.Persistence.Core.Criteria.Criterion.Conjunction;

interface

uses
  Spring.Persistence.Core.Criteria.Criterion.Junction
  ,Spring.Persistence.SQL.Types
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
