unit Spring.Persistence.Core.Criteria.Criterion.LogicalExpression;

interface

{$I sv.inc}

uses
  Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.Core.Criteria.AbstractCriterion
  ,Rtti
  ,Spring.Persistence.SQL.Types
  ,Spring.Persistence.SQL.Params
  ,Spring.Persistence.SQL.Commands
  ,Spring.Persistence.SQL.Interfaces
  ,Spring.Collections
  ;

type
  TLogicalExpression = class(TAbstractCriterion)
  private
    FOperator: TWhereOperator;
    FLeft: ICriterion;
    FRight: ICriterion;
  public
    constructor Create(ALeft, ARight: ICriterion; const AOperator: TWhereOperator); virtual;
  public
    function GetWhereOperator(): TWhereOperator; override;
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

uses
  SysUtils
  ;

{ TLogicalExpression }

constructor TLogicalExpression.Create(ALeft, ARight: ICriterion; const AOperator: TWhereOperator);
begin
  inherited Create();
  FLeft := ALeft;
  FRight := ARight;
  FOperator := AOperator;
end;

function TLogicalExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TLogicalExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere, LEndOp: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LWhere := TSQLWhereField.Create('', '');
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere);
  LWhere.LeftSQL := FLeft.ToSqlString(AParams, ACommand, AGenerator, AAddToCommand);
  if Assigned(FRight) then
    LWhere.RightSQL := FRight.ToSqlString(AParams, ACommand, AGenerator, AAddToCommand);

  LEndOp := TSQLWhereField.Create('', '');
  LEndOp.MatchMode := GetMatchMode;
  LEndOp.WhereOperator := GetEndOperator(FOperator);
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LEndOp);

  Result := LWhere.ToSQLString(AGenerator.GetEscapeFieldnameChar);

  if not AAddToCommand then
  begin
    LWhere.Free;
    LEndOp.Free;
  end;
end;

end.
