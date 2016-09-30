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

unit Spring.Data.IndexList;

interface

uses
  Spring.Collections;

type
  TIndexItem = record
    Index: Integer;
    Obj: TObject;
  end;

  TIndexList = class
  private
    fDataList: IObjectList;
    fIndexes: IList<TIndexItem>;
    fIsChanging: Boolean;
    function GetCount: Integer;
    function GetIndexes: IReadOnlyList<TIndexItem>;
    function GetObject(index: Integer): TObject;
    procedure SetDataList(const value: IObjectList);
    procedure SetObject(index: Integer; const value: TObject);

    procedure FixIndexes(startIndex: Integer);
  public
    constructor Create;

    function AddIndex(index: Integer): Integer;
    procedure DeleteIndex(index: Integer);

    function AddObject(const obj: TObject): Integer;
    procedure DeleteObject(index: Integer);
    procedure InsertObject(const obj: TObject; index: Integer);

    function Contains(const obj: TObject): Boolean;
    function IndexOf(const obj: TObject): Integer;

    procedure Clear;
    procedure Rebuild;

    property Count: Integer read GetCount;
    property DataList: IObjectList read fDataList write SetDataList;
    property Indexes: IReadOnlyList<TIndexItem> read GetIndexes;
    property IsChanging: Boolean read fIsChanging;
    property Objects[index: Integer]: TObject read GetObject write SetObject;
  end;

implementation


{$REGION 'TIndexList'}

constructor TIndexList.Create;
begin
  inherited Create;
  fIndexes := TCollections.CreateList<TIndexItem>;
end;

function TIndexList.AddIndex(index: Integer): Integer;
var
  indexItem: TIndexItem;
begin
  indexItem.Index := index;
  indexItem.Obj := fDataList[index];
  Result := fIndexes.Add(indexItem);
end;

function TIndexList.AddObject(const obj: TObject): Integer;
begin
  fIsChanging := True;
  try
    Result := AddIndex(fDataList.Add(obj));
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.Clear;
begin
  fIndexes.Clear;
end;

function TIndexList.Contains(const obj: TObject): Boolean;
begin
  Result := IndexOf(obj) <> -1;
end;

procedure TIndexList.DeleteIndex(index: Integer);
begin
  fIndexes.Delete(index);
end;

procedure TIndexList.DeleteObject(index: Integer);
var
  fixIndex: Integer;
begin
  fixIndex := fIndexes[index].Index;
  fIsChanging := True;
  try
    fDataList.Delete(fixIndex);
    DeleteIndex(index);
    FixIndexes(fixIndex);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.FixIndexes(startIndex: Integer);
var
  i: Integer;
  indexItem: TIndexItem;
begin
  for i := 0 to Count - 1 do
  begin
    indexItem := fIndexes[i];
    if indexItem.Index > startIndex then
    begin
      Dec(indexItem.Index);
      fIndexes[i] := indexItem;
    end;
  end;
end;

function TIndexList.GetCount: Integer;
begin
  Result := fIndexes.Count;
end;

function TIndexList.GetIndexes: IReadOnlyList<TIndexItem>;
begin
  Result := fIndexes.AsReadOnlyList;
end;

function TIndexList.GetObject(index: Integer): TObject;
begin
  Result := fIndexes[index].Obj;
end;

function TIndexList.IndexOf(const obj: TObject): Integer;
begin
  if obj = nil then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Objects[Result] = obj then
      Exit;
  Result := -1;
end;

procedure TIndexList.InsertObject(const obj: TObject; index: Integer);
var
  indexItem: TIndexItem;
begin
  fIsChanging := True;
  try
    indexItem.Index := fDataList.Add(obj);
    indexItem.Obj := obj;
    fIndexes.Insert(index, indexItem);
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
      AddIndex(i);
end;

procedure TIndexList.SetDataList(const Value: IObjectList);
begin
  fDataList := Value;
  Rebuild;
end;

procedure TIndexList.SetObject(index: Integer; const Value: TObject);
var
  indexItem: TIndexItem;
begin
  indexItem := fIndexes[index];
  indexItem.Obj := Value;
  fIndexes[index] := indexItem;
end;

{$ENDREGION}


end.
