unit Core.Criteria.Criterion.LikeExpression;

interface

uses
  Core.Criteria.Criterion.SimpleExpression
  ,Core.Interfaces
  ,SQL.Types
  ,Rtti
  ,SQL.Params
  ,SQL.Commands
  ,Generics.Collections
  ;

type
  TLikeExpression = class(TSimpleExpression)
  private
    FMatchMode: TMatchMode;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode); reintroduce; overload;

    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string; override;
  end;

implementation

uses
  SysUtils
  ;

{ TLikeExpression }

constructor TLikeExpression.Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode);
begin
  inherited Create(APropertyName, AValue, AOperator);
  FMatchMode := AMatchMode;
end;

function TLikeExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);
  Result := Format('%S %S %S', [UpperCase(PropertyName), WhereOpNames[GetWhereOperator], GetMatchModeString(FMatchMode, Value.AsString)]);

  LWhere := TSQLWhereField.Create(Result, ACommand.Table);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);
end;

end.
