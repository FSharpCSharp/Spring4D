unit Adapters.ObjectDataset.IndexList;

interface

uses
  Generics.Collections
  ,Rtti
  ,Spring.Collections
  ;

type
  TODIndexList = class(TList<Integer>)
  private
    FDataList: IList;
  protected
    procedure FixIndexes(AStart: Integer);
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Rebuild();

    function AddModel(const AModel: TValue): Integer;
    procedure DeleteModel(AIndex: Integer);
    function GetModel(const AIndex: Integer): TValue;
    procedure SetModel(AIndex: Integer; const AModel: TValue);

    property DataList: IList read FDataList write FDataList;
  end;

implementation


{ TODIndexList<Integer> }

function TODIndexList.AddModel(const AModel: TValue): Integer;
begin
  FDataList.Add(AModel);
  Result := Add(FDataList.Count - 1);
end;

constructor TODIndexList.Create();
begin
  inherited Create();
end;

procedure TODIndexList.DeleteModel(AIndex: Integer);
begin
  FDataList.Delete(Items[AIndex]);
  Delete(AIndex);
  FixIndexes(AIndex);
end;

destructor TODIndexList.Destroy;
begin
  inherited Destroy;
end;

procedure TODIndexList.FixIndexes(AStart: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if (Items[i] > AStart) then
    begin
      Items[i] := Items[i] - 1;
    end;
  end;
end;

function TODIndexList.GetModel(const AIndex: Integer): TValue;
begin
  Result := FDataList[Items[AIndex]];
end;

procedure TODIndexList.Rebuild;
var
  i: Integer;
begin
  Clear;
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
    begin
      Add(i);
    end;
  end;
end;

procedure TODIndexList.SetModel(AIndex: Integer; const AModel: TValue);
begin
  FDataList[Items[AIndex]] := AModel;
end;

end.
