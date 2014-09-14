unit Spring.Persistence.Core.Criteria.Criterion.InExpression;

interface

uses
  Spring.Persistence.Core.Criteria.Criterion.SimpleExpression
  ,Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.SQL.Types
  ,Rtti
  ,Spring.Persistence.SQL.Params
  ,Spring.Persistence.SQL.Commands
  ,Spring.Persistence.SQL.Interfaces
  ,Spring.Collections
  ;

type
  TInExpression<T> = class(TSimpleExpression)
  private
    FValues: TArray<T>;
  protected
    function ValuesToSeparatedString(): string;
  public
    constructor Create(const APropertyName: string; const AValues: TArray<T>; AOperator: TWhereOperator); reintroduce; overload;

    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

uses
  SysUtils
  ,TypInfo
  ;

{ TInExpression<T> }

constructor TInExpression<T>.Create(const APropertyName: string; const AValues: TArray<T>;
  AOperator: TWhereOperator);
begin
  inherited Create(APropertyName, TValue.Empty, AOperator);
  FValues := AValues;
end;

function TInExpression<T>.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);

  Result := Format('%S %S (%S)',
    [PropertyName, WhereOpNames[GetWhereOperator], ValuesToSeparatedString()]);

  LWhere := TSQLWhereField.Create(Result, GetCriterionTable(ACommand));
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;

  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

function TInExpression<T>.ValuesToSeparatedString: string;
var
  LCurrent: T;
  i: Integer;
  LValue: TValue;
  LStringValue: string;
begin
  Result := 'NULL';
  i := 0;
  for LCurrent in FValues do
  begin
    if i = 0 then
      Result := ''
    else
      Result := Result + ',';

    LValue := TValue.From<T>(LCurrent);
    case LValue.Kind of
      tkChar, tkWChar, tkLString, tkWString, tkUString, tkString:
        LStringValue := QuotedStr(LValue.AsString)
      else
        LStringValue := LValue.ToString;
    end;
    Result := Result + LStringValue;
    Inc(i);
  end;
end;

end.
