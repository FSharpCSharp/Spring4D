unit Core.Criteria.Criterion.Disjunction;

interface

uses
  Core.Criteria.Criterion.Junction
  ,SQL.Types
  ;

type
  TDisjunction = class(TJunction)
  protected
    function GetWhereOperator(): TWhereOperator; override;
  end;

implementation

{ TDisjunction }

function TDisjunction.GetWhereOperator: TWhereOperator;
begin
  Result := woOr;
end;

end.
