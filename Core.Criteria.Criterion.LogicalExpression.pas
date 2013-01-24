unit Core.Criteria.Criterion.LogicalExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,Rtti
  ,SQL.Types
  ,SQL.Params
  ,SQL.Commands
  ,Generics.Collections
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
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string; override;
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

function TLogicalExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string;
var
  LWhere, LOrEnd: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);

  LWhere := TSQLWhereField.Create('', '');
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);
  LWhere.LeftSQL := FLeft.ToSqlString(AParams, ACommand);
  LWhere.RightSQL := FRight.ToSqlString(AParams, ACommand);

  LOrEnd := TSQLWhereField.Create('', '');
  LOrEnd.MatchMode := GetMatchMode;
  LOrEnd.WhereOperator := woOrEnd;
  TWhereCommand(ACommand).WhereFields.Add(LOrEnd);

  Result := LWhere.ToSQLString;
 // Result := Format('%0:S %1:S %2:S', [FLeft.ToSqlString(AParams, ACommand), LogicalOpNames[FOperator], FRight.ToSqlString(AParams, ACommand)]);
end;

end.
