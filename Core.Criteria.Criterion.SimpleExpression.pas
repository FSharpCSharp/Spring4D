unit Core.Criteria.Criterion.SimpleExpression;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.AbstractCriterion
  ,Rtti
  ,SQL.Types
  ,SQL.Params
  ,SQL.Commands
  ,SQL.Interfaces
  ,Generics.Collections
  ;

type
  TSimpleExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FValue: TValue;
    FOperator: TWhereOperator;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator): string; override;
    function GetWhereOperator(): TWhereOperator; override;

    property PropertyName: string read FPropertyName;
    property Value: TValue read FValue;
  end;

implementation

uses
  SysUtils
  ;

{ TSimpleExpression }

constructor TSimpleExpression.Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FValue := AValue;
  FOperator := AOperator;
end;

function TSimpleExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TSimpleExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator): string;
var
  LParam: TDBParam;
  LWhere: TSQLWhereField;
  LParamName: string;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LParamName := ACommand.GetAndIncParameterName(FPropertyName);
  LWhere := TSQLWhereField.Create(UpperCase(FPropertyName), GetCriterionTable(ACommand) {ACommand.Table});
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  LWhere.ParamName := LParamName;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);

  Result := LWhere.ToSQLString(Generator.GetEscapeFieldnameChar); {TODO -oLinas -cGeneral : fix escape fields}
  LParam := TDBParam.Create();
  LParam.SetFromTValue(FValue);
  LParam.Name := LParamName;
  AParams.Add(LParam);
end;

end.
