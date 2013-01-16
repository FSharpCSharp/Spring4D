unit Core.Criteria.Criterion.InExpression;

interface

uses
  Core.Criteria.Criterion.SimpleExpression
  ,Core.Interfaces
  ,SQL.Types
  ,Rtti
  ,SQL.Params
  ,Generics.Collections
  ;

type
  TInExpression<T> = class(TSimpleExpression)
  private
    FValues: TArray<T>;
  protected
    function ValuesToSeparatedString(): string;
  public
    constructor Create(const APropertyName: string; const AValues: TArray<T>; AOperator: TWhereOperator); reintroduce; overload;

    function ToSqlString(AParams: TObjectList<TDBParam>): string; override;
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

function TInExpression<T>.ToSqlString(AParams: TObjectList<TDBParam>): string;
begin
  Result := Format('%S %S (%S)',
    [UpperCase(PropertyName), WhereOpNames[GetWhereOperator], ValuesToSeparatedString()]);
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
