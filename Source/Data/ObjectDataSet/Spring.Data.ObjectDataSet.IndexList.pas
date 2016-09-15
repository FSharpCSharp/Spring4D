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
  Generics.Collections,
  Spring.Collections;

type
  TIndexItem = record
    DataListIndex: Integer;
    DataListObject: TObject;
  end;

  TIndexList = class
  private
    FDataList: IObjectList;
    FList: TList<TIndexItem>;
    FChangingDataList: Boolean;
    procedure SetDataList(const Value: IObjectList);
    function GetItem(Index: Integer): TIndexItem;
    procedure SetItem(Index: Integer; const Value: TIndexItem);
    function GetCount: Integer;
  protected
    procedure FixIndexes(AStart: Integer);

    procedure Insert(AIndex, ADataListIndex: Integer; const AModel: TObject);

    property Items[Index: Integer]: TIndexItem read GetItem write SetItem; default;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Rebuild;

    function Add(ADataListIndex: Integer; const ADataListObject: TObject): Integer; virtual;
    function AddModel(const AModel: TObject): Integer;
    function ContainsModel(const AModel: TObject): Boolean;
    procedure Delete(Index: Integer);
    procedure DeleteModel(AIndex: Integer);
    function IndexOfModel(const AModel: TObject): Integer;
    procedure InsertModel(const AModel: TObject; AIndex: Integer);
    function GetModel(const AIndex: Integer): TObject;
    procedure SetModel(AIndex: Integer; const AModel: TObject);

    procedure Clear;

    property DataListIsChanging: Boolean read FChangingDataList;
    property Count: Integer read GetCount;
    property DataList: IObjectList read FDataList write SetDataList;
  end;

implementation


{$REGION 'TIndexList'}

function TIndexList.Add(ADataListIndex: Integer; const ADataListObject: TObject): Integer;
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := ADataListObject;
  Result := FList.Add(LItem);
end;

function TIndexList.AddModel(const AModel: TObject): Integer;
begin
  FChangingDataList := True;
  try
    FDataList.Add(AModel);
    Result := Add(FDataList.Count - 1, AModel);
  finally
    FChangingDataList := False;
  end;
end;

procedure TIndexList.Clear;
begin
  FList.Clear;
end;

function TIndexList.ContainsModel(const AModel: TObject): Boolean;
begin
  Result := IndexOfModel(AModel) <> -1;
end;

constructor TIndexList.Create;
begin
  inherited Create;
  FList := TList<TIndexItem>.Create;
end;

procedure TIndexList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TIndexList.DeleteModel(AIndex: Integer);
var
  LFixIndex: Integer;
begin
  LFixIndex := Items[AIndex].DataListIndex;
  FChangingDataList := True;
  try
    FDataList.Delete(LFixIndex);
    Delete(AIndex);
    FixIndexes(LFixIndex);
  finally
    FChangingDataList := False;
  end;
end;

destructor TIndexList.Destroy;
begin
  FList.Free;
  inherited Destroy;
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
  Result := FList.Count;
end;

function TIndexList.GetItem(Index: Integer): TIndexItem;
begin
  Result := FList[Index];
end;

function TIndexList.GetModel(const AIndex: Integer): TObject;
begin
  Result := Items[AIndex].DataListObject; // FDataList[Items[AIndex]];
end;

function TIndexList.IndexOfModel(const AModel: TObject): Integer;
begin
  if AModel = nil then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if GetModel(Result) = AModel then
      Exit;
  Result := -1;
end;

procedure TIndexList.Insert(AIndex, ADataListIndex: Integer; const AModel: TObject);
var
  LItem: TIndexItem;
begin
  LItem.DataListIndex := ADataListIndex;
  LItem.DataListObject := AModel;
  FList.Insert(AIndex, LItem);
end;

procedure TIndexList.InsertModel(const AModel: TObject; AIndex: Integer);
begin
  FChangingDataList := True;
  try
    FDataList.Add(AModel);
    Insert(AIndex, FDataList.Count - 1, AModel);
  finally
    FChangingDataList := False;
  end;
end;

procedure TIndexList.Rebuild;
var
  i: Integer;
begin
  Clear;
  if Assigned(FDataList) then
    for i := 0 to FDataList.Count - 1 do
      Add(i, FDataList[i]);
end;

procedure TIndexList.SetDataList(const Value: IObjectList);
begin
  FDataList := Value;
  Rebuild;
end;

procedure TIndexList.SetItem(Index: Integer; const Value: TIndexItem);
begin
  FList[Index] := Value;
end;

procedure TIndexList.SetModel(AIndex: Integer; const AModel: TObject);
var
  LItem: TIndexItem;
begin
  LItem := Items[AIndex];
  LItem.DataListObject := AModel;
  Items[AIndex] := LItem;
 // FDataList[Items[AIndex]] := AModel;
end;

{$ENDREGION}


end.
