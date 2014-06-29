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
  ,Spring.Collections
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
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
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

function TSimpleExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LParam: TDBParam;
  LWhere: TSQLWhereField;
  LParamName: string;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LParamName := ACommand.GetAndIncParameterName(FPropertyName);

  LWhere := TSQLWhereField.Create(FPropertyName, GetCriterionTable(ACommand) {ACommand.Table});
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  LWhere.ParamName := LParamName;

  Result := LWhere.ToSQLString(Generator.GetEscapeFieldnameChar); {TODO -oLinas -cGeneral : fix escape fields}
  LParam := TDBParam.Create();
  LParam.SetFromTValue(FValue);
  LParam.Name := LParamName;
  AParams.Add(LParam);

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
