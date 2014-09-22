unit Spring.Persistence.ObjectDataset.IndexList;

interface

uses
  Generics.Collections
  ,Rtti
  ,Spring.Collections
  ;

type
  TIndexItem = record
    DataListIndex: Integer;
    DataListObject: TValue;
  end;

  TODIndexList = class
  private
    FDataList: IObjectList;
    FList: TList<TIndexItem>;
    procedure SetDataList(const Value: IObjectList);
    function GetItem(Index: Integer): TIndexItem;
    procedure SetItem(Index: Integer; const Value: TIndexItem);
    function GetCount: Integer;
  protected
    procedure FixIndexes(AStart: Integer);

    procedure Insert(AIndex, ADataListIndex: Integer; const AModel: TValue);

    property Items[Index: Integer]: TIndexItem read GetItem write SetItem; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Rebuild;

    function Add(ADataListIndex: Integer; const ADataListObject: TValue): Integer; virtual;
    function AddModel(const AModel: TValue): Integer;
    function ContainsModel(const AModel: TValue): Boolean;
    procedure Delete(Index: Integer);
    procedure DeleteModel(AIndex: Integer);
    function IndexOfModel(const AModel: TValue): Integer;
    procedure InsertModel(const AModel: TValue; AIndex: Integer);
    function GetModel(const AIndex: Integer): TValue;
    procedure SetModel(AIndex: Integer; const AModel: TValue);

    procedure Clear;

    property Count: Integer read GetCount;
    property DataList: IObjectList read FDataList write SetDataList;
  end;

implementation


{ TODIndexList<Integer> }

function TODIndexList.Add(ADataListIndex: Integer; const ADataListObject: TValue): Integer;
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := ADataListObject;
  Result := FList.Add(LItem);
end;

function TODIndexList.AddModel(const AModel: TValue): Integer;
begin
  FDataList.Add(AModel.AsObject);
  Result := Add(FDataList.Count - 1, AModel);
end;

procedure TODIndexList.Clear;
begin
  FList.Clear;
end;

function TODIndexList.ContainsModel(const AModel: TValue): Boolean;
begin
  Result := (IndexOfModel(AModel) <> -1);
end;

constructor TODIndexList.Create;
begin
  inherited Create;
  FList := TList<TIndexItem>.Create;
end;

procedure TODIndexList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TODIndexList.DeleteModel(AIndex: Integer);
var
  LFixIndex: Integer;
begin
  LFixIndex := Items[AIndex].DataListIndex;
  FDataList.Delete(LFixIndex);
  Delete(AIndex);
  FixIndexes(LFixIndex);
end;

destructor TODIndexList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TODIndexList.FixIndexes(AStart: Integer);
var
  i: Integer;
  LItem: TIndexItem;
begin
  for i := 0 to Count - 1 do
  begin
    if (Items[i].DataListIndex > AStart) then
    begin
      LItem.DataListIndex := Items[i].DataListIndex - 1;
      LItem.DataListObject := Items[i].DataListObject;
      Items[i] := LItem;
      //Items[i] := Items[i] - 1;
    end;
  end;
end;

function TODIndexList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TODIndexList.GetItem(Index: Integer): TIndexItem;
begin
  Result := FList[Index];
end;

function TODIndexList.GetModel(const AIndex: Integer): TValue;
begin
  Result := Items[AIndex].DataListObject; // FDataList[Items[AIndex]];
end;

function TODIndexList.IndexOfModel(const AModel: TValue): Integer;
begin
  if AModel.IsEmpty then
    Exit(-1);

  for Result := 0 to Count - 1 do
  begin
    if (GetModel(Result).AsObject = AModel.AsObject) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TODIndexList.Insert(AIndex, ADataListIndex: Integer; const AModel: TValue);
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := AModel;
  FList.Insert(AIndex, LItem);
end;

procedure TODIndexList.InsertModel(const AModel: TValue; AIndex: Integer);
begin
  FDataList.Add(AModel.AsObject);
  Insert(AIndex, FDataList.Count - 1, AModel);
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
      Add(i, FDataList[i]);
    end;
  end;
end;

procedure TODIndexList.SetDataList(const Value: IObjectList);
begin
  FDataList := Value;
  Rebuild;
end;

procedure TODIndexList.SetItem(Index: Integer; const Value: TIndexItem);
begin
  FList[Index] := Value;
end;

procedure TODIndexList.SetModel(AIndex: Integer; const AModel: TValue);
var
  LItem: TIndexItem;
begin
  LItem := Items[AIndex];
  LItem.DataListObject := AModel;
  Items[AIndex] := LItem;
 // FDataList[Items[AIndex]] := AModel;
end;

end.
