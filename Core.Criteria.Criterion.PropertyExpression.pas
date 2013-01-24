unit Core.Criteria.Criterion.PropertyExpression;

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
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string; override;

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

function TPropertyExpression.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand): string;
var
  LWhere: TSQLWherePropertyField;
  LTable, LOtherTable: TSQLTable;
begin
  Assert(ACommand is TWhereCommand);
  LTable := FTable;
  LOtherTable := FOtherTable;

  if not Assigned(LTable) then
    LTable := ACommand.Table;

  if not Assigned(LOtherTable) then
    LOtherTable := ACommand.Table;

  LWhere := TSQLWherePropertyField.Create(UpperCase(FPropertyName), UpperCase(FOtherPropertyName)
    , LTable, LOtherTable);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  TWhereCommand(ACommand).WhereFields.Add(LWhere);
end;

end.
