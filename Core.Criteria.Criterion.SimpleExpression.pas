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
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string; override;
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

function TSimpleExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string;
var
  LParam: TDBParam;
  LWhere: TSQLWhereField;
  LParamName: string;
begin
  Assert(ACommand is TWhereCommand);
  LParamName := ACommand.GetAndIncParameterName(FPropertyName);
  LWhere := TSQLWhereField.Create(UpperCase(FPropertyName), ACommand.Table);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  LWhere.ParamName := LParamName;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);

  {TODO -oLinas -cGeneral : generate simple expression sql string}
  Result := LWhere.ToSQLString;
  LParam := TDBParam.Create();
  LParam.SetFromTValue(FValue);
  LParam.Name := LParamName;   {TODO -oLinas -cGeneral : what if there are more than 1 parameter with the same fieldname????}
  AParams.Add(LParam);
end;

end.
