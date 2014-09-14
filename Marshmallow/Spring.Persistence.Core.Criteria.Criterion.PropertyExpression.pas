unit Spring.Persistence.Core.Criteria.Criterion.PropertyExpression;

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
  TPropertyExpression = class(TAbstractCriterion)
  private
    FOperator: TWhereOperator;
    FPropertyName: string;
    FOtherPropertyName: string;
    FTable: TSQLTable;
    FOtherTable: TSQLTable;
  public
    constructor Create(const APropertyName, AOtherPropertyName: string; AOperator: TWhereOperator
      ; ATable: TSQLTable = nil; AOtherTable: TSQLTable = nil); virtual;
    destructor Destroy; override;
  public
    function GetWhereOperator(): TWhereOperator; override;
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;

  end;

implementation

uses
  SysUtils
  ;

{ TPropertyExpression }

constructor TPropertyExpression.Create(const APropertyName, AOtherPropertyName: string; AOperator: TWhereOperator; ATable, AOtherTable: TSQLTable);
begin
  inherited Create();
  FPropertyName := APropertyName;
  FOtherPropertyName := AOtherPropertyName;
  FOperator := AOperator;
  FTable := ATable;
  FOtherTable := AOtherTable;
end;

destructor TPropertyExpression.Destroy;
begin
  if Assigned(FTable) then
    FTable.Free;
  if Assigned(FOtherTable) then
    FOtherTable.Free;
  inherited Destroy;
end;

function TPropertyExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TPropertyExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWherePropertyField;
  LTable, LOtherTable: TSQLTable;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LTable := FTable;
  LOtherTable := FOtherTable;

  LTable := GetCriterionTable(ACommand, LTable);
  LOtherTable := GetCriterionTable(ACommand, LOtherTable);

  if not Assigned(LTable) then
    LTable := ACommand.Table;

  if not Assigned(LOtherTable) then
    LOtherTable := ACommand.Table;

  LWhere := TSQLWherePropertyField.Create(UpperCase(FPropertyName), UpperCase(FOtherPropertyName)
    , LTable, LOtherTable);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

end.
