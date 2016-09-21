{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Data.ObjectDataSet.IndexList;

interface

uses
  Spring.Collections;

type
  TIndexItem = record
    DataListIndex: Integer;
    DataListObject: TObject;
  end;

  TIndexList = class
  private
    fDataList: IObjectList;
    fIndexes: IList<TIndexItem>;
    fIsChanging: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TIndexItem;
    function GetModel(Index: Integer): TObject;
    procedure SetDataList(const Value: IObjectList);
    procedure SetItem(Index: Integer; const Value: TIndexItem);
    procedure SetModel(Index: Integer; const Value: TObject);

    procedure FixIndexes(AStart: Integer);
    procedure Insert(AIndex, ADataListIndex: Integer; const AModel: TObject);
    property Items[Index: Integer]: TIndexItem read GetItem write SetItem; default;
  public
    constructor Create;

    procedure Rebuild;

    function Add(ADataListIndex: Integer; const ADataListObject: TObject): Integer; virtual;
    function AddModel(const Model: TObject): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteModel(Index: Integer);
    procedure InsertModel(const Model: TObject; Index: Integer);

    function ContainsModel(const Model: TObject): Boolean;
    function IndexOfModel(const Model: TObject): Integer;

    procedure Clear;

    property Count: Integer read GetCount;
    property DataList: IObjectList read fDataList write SetDataList;
    property IsChanging: Boolean read fIsChanging;
    property Models[Index: Integer]: TObject read GetModel write SetModel;
  end;

implementation


{$REGION 'TIndexList'}

constructor TIndexList.Create;
begin
  inherited Create;
  fIndexes := TCollections.CreateList<TIndexItem>;
end;

function TIndexList.Add(ADataListIndex: Integer; const ADataListObject: TObject): Integer;
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := ADataListObject;
  Result := fIndexes.Add(LItem);
end;

function TIndexList.AddModel(const Model: TObject): Integer;
begin
  fIsChanging := True;
  try
    fDataList.Add(Model);
    Result := Add(fDataList.Count - 1, Model);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.Clear;
begin
  fIndexes.Clear;
end;

function TIndexList.ContainsModel(const Model: TObject): Boolean;
begin
  Result := IndexOfModel(Model) <> -1;
end;

procedure TIndexList.Delete(Index: Integer);
begin
  fIndexes.Delete(Index);
end;

procedure TIndexList.DeleteModel(Index: Integer);
var
  LFixIndex: Integer;
begin
  LFixIndex := Items[Index].DataListIndex;
  fIsChanging := True;
  try
    fDataList.Delete(LFixIndex);
    Delete(Index);
    FixIndexes(LFixIndex);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.FixIndexes(AStart: Integer);
var
  i: Integer;
  LItem: TIndexItem;
begin
  for i := 0 to Count - 1 do
    if Items[i].DataListIndex > AStart then
    begin
      LItem.DataListIndex := Items[i].DataListIndex - 1;
      LItem.DataListObject := Items[i].DataListObject;
      Items[i] := LItem;
      //Items[i] := Items[i] - 1;
    end;
end;

function TIndexList.GetCount: Integer;
begin
  Result := fIndexes.Count;
end;

function TIndexList.GetItem(Index: Integer): TIndexItem;
begin
  Result := fIndexes[Index];
end;

function TIndexList.GetModel(Index: Integer): TObject;
begin
  Result := Items[Index].DataListObject; // FDataList[Items[AIndex]];
end;

function TIndexList.IndexOfModel(const Model: TObject): Integer;
begin
  if Model = nil then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Models[Result] = Model then
      Exit;
  Result := -1;
end;

procedure TIndexList.Insert(AIndex, ADataListIndex: Integer; const AModel: TObject);
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := AModel;
  fIndexes.Insert(AIndex, LItem);
end;

procedure TIndexList.InsertModel(const Model: TObject; Index: Integer);
begin
  fIsChanging := True;
  try
    fDataList.Add(Model);
    Insert(Index, fDataList.Count - 1, Model);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.Rebuild;
var
  i: Integer;
begin
  Clear;
  if Assigned(fDataList) then
    for i := 0 to fDataList.Count - 1 do
      Add(i, fDataList[i]);
end;

procedure TIndexList.SetDataList(const Value: IObjectList);
begin
  fDataList := Value;
  Rebuild;
end;

procedure TIndexList.SetItem(Index: Integer; const Value: TIndexItem);
begin
  fIndexes[Index] := Value;
end;

procedure TIndexList.SetModel(Index: Integer; const Value: TObject);
var
  LItem: TIndexItem;
begin
  LItem := Items[Index];
  LItem.DataListObject := Value;
  Items[Index] := LItem;
 // FDataList[Items[AIndex]] := Value;
end;

{$ENDREGION}


end.
