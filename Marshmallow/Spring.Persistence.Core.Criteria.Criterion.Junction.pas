unit Spring.Persistence.Core.Criteria.Criterion.Junction;

interface

uses
  Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.SQL.Types
  ,Spring.Persistence.SQL.Params
  ,Spring.Persistence.SQL.Commands
  ,Spring.Persistence.SQL.Interfaces
  ,Spring.Collections
  ;

type
  TJunction = class(TInterfacedObject, ICriterion)
  private
    FCriterions: IList<ICriterion>;
    FEntityClass: TClass;
  protected
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; virtual;
    procedure SetEntityClass(const Value: TClass); virtual;
    function GetEntityClass: TClass; virtual;
    function GetMatchMode(): TMatchMode; virtual;
    function GetWhereOperator(): TWhereOperator; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): TJunction;

    property Criterions: IList<ICriterion> read FCriterions;
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
  FCriterions := TCollections.CreateList<ICriterion>;
end;

destructor TJunction.Destroy;
begin
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

function TJunction.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand;
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
