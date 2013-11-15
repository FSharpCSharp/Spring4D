unit Core.Criteria.Criterion.Junction;

interface

uses
  Core.Interfaces
  ,SQL.Types
  ,SQL.Params
  ,SQL.Commands
  ,SQL.Interfaces
  ,Generics.Collections
  ;

type
  TJunction = class(TInterfacedObject, ICriterion)
  private
    FCriterions: TList<ICriterion>;
    FEntityClass: TClass;
  protected
    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; virtual;
    procedure SetEntityClass(const Value: TClass); virtual;
    function GetEntityClass: TClass; virtual;
    function GetMatchMode(): TMatchMode; virtual;
    function GetWhereOperator(): TWhereOperator; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): TJunction;

    property Criterions: TList<ICriterion> read FCriterions;
  end;

implementation

{ TJunction }

function TJunction.Add(ACriterion: ICriterion): TJunction;
begin
  FCriterions.Add(ACriterion);
  Result := Self;
end;

constructor TJunction.Create;
begin
  inherited Create;
  FCriterions := TList<ICriterion>.Create;
end;

destructor TJunction.Destroy;
begin
  FCriterions.Free;
  inherited Destroy;
end;

function TJunction.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TJunction.GetMatchMode: TMatchMode;
begin
  Result := mmExact;
end;

procedure TJunction.SetEntityClass(const Value: TClass);
begin
  FEntityClass := Value;
end;

function TJunction.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand;
  AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LCriterion: ICriterion;
  i: Integer;
  LSql: string;
  LWhere: TSQLWhereField;
begin
  Result := '';

  Assert(ACommand is TWhereCommand);

  for i := 0 to FCriterions.Count - 1 do
  begin
    if (i<> 0) then
      Result := Result +  ' ' + WhereOpNames[GetWhereOperator] + ' ';

    LCriterion := FCriterions[i];
    LSql := LCriterion.ToSqlString(AParams, ACommand, AGenerator, False);

    Result := Result + LSql;
  end;

  if AAddToCommand then
  begin
    LWhere := TSQLWhereField.Create(Result, '');
    LWhere.MatchMode := GetMatchMode;
    LWhere.WhereOperator := woJunction;
    TWhereCommand(ACommand).WhereFields.Add(LWhere);
  end;
end;

end.
