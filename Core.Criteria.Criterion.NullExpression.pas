unit Core.Criteria.Criterion.NullExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,SQL.Types
  ,SQL.Params
  ,SQl.Commands
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
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string; override;
    function GetWhereOperator(): TWhereOperator; override;
  end;

implementation

uses
  SysUtils
  ;

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

function TNullExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);

  LWhere := TSQLWhereField.Create(FPropertyName, ACommand.Table);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);

  Result := LWhere.ToSQLString;
end;

end.
