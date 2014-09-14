unit Spring.Persistence.Core.Criteria.Criterion.Disjunction;

interface

uses
  Spring.Persistence.Core.Criteria.Criterion.Junction
  ,Spring.Persistence.SQL.Types
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
