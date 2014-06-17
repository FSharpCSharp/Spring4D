unit Core.Criteria.Criterion.NullExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,SQL.Types
  ,SQL.Params
  ,SQl.Commands
  ,SQL.Interfaces
  ,Spring.Collections
  ;

type
  TNullExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FOperator: TWhereOperator;
  public
    constructor Create(const APropertyName: string; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
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

function TNullExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LWhere := TSQLWhereField.Create(FPropertyName, GetCriterionTable(ACommand) );
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  Result := LWhere.ToSQLString(Generator.GetEscapeFieldnameChar);

  if AAddToCommand then
  begin
    TWhereCommand(ACommand).WhereFields.Add(LWhere);
  end
  else
  begin
    LWhere.Free;
  end;
end;

end.
