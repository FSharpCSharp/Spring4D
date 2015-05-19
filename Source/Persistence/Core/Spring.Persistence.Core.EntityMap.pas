{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Core.EntityMap;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;

type
  TEntityMapKey = string;
  TEntityMapValue = TArray<TPair<string, TValue>>;

  TEntityMap = class(TInterfacedObject, IEntityMap)
  private
    fEntityValues: IDictionary<TEntityMapKey, TEntityMapValue>;
    fCriticalSection: ICriticalSection;
  protected
    function GetEntityKey(const instance: TObject): TEntityMapKey; overload;
    function GetEntityKey(const className, id: string): TEntityMapKey; overload;
    function GetEntityValues(const instance: TObject; const id: string): TEntityMapValue;

    procedure PutEntity(const entity: IEntityWrapper);

    procedure FinalizeItem(const item: TPair<TEntityMapKey, TEntityMapValue>);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddOrReplace(const instance: IEntityWrapper);
    procedure Remove(const instance: TObject);
    procedure Clear;

    function IsMapped(const instance: TObject): Boolean;

    function GetChangedMembers(const instance: TObject;
      const entityData: TEntityData): IList<ColumnAttribute>; virtual;
    function GetMemberValue(const className, id, memberName: string): TValue;
  end;

implementation

uses
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TEntityMap'}

constructor TEntityMap.Create;
begin
  inherited Create;
  fCriticalSection := TInterfacedCriticalSection.Create;
  fEntityValues := TCollections.CreateDictionary<TEntityMapKey,TEntityMapValue>;
end;

destructor TEntityMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TEntityMap.FinalizeItem(
  const item: TPair<TEntityMapKey, TEntityMapValue>);
var
  pair: TPair<string, TValue>;
  value: TValue;
begin
  for pair in Item.Value do
  begin
    value := pair.Value;
    value.Free;
  end;
end;

procedure TEntityMap.AddOrReplace(const instance: IEntityWrapper);
begin
  Assert(Assigned(instance), 'Entity not assigned');
  PutEntity(instance);
end;

procedure TEntityMap.Clear;
var
  pair: TPair<TEntityMapKey,TEntityMapValue>;
begin
  for pair in fEntityValues do
    FinalizeItem(pair);
  fEntityValues.Clear;
end;

function TEntityMap.GetChangedMembers(const instance: TObject; const entityData: TEntityData): IList<ColumnAttribute>;
var
  currentValue, dirtyValue: TValue;
  id: string;
  values: TEntityMapValue;
  i: Integer;
  col: ColumnAttribute;
begin
  Result := TCollections.CreateList<ColumnAttribute>;  
  id := entityData.GetPrimaryKeyValueAsString(instance);
  values := GetEntityValues(instance, id);

  for i := 0 to entityData.Columns.Count - 1 do
  begin
    col := entityData.Columns[i];
    currentValue := col.GetValue(instance);
    dirtyValue := values[i].Value;
    if not currentValue.Equals(dirtyValue) then
      Result.Add(col);
  end;
end;

function TEntityMap.GetEntityValues(const instance: TObject; const id: string): TEntityMapValue;
begin
  if not fEntityValues.TryGetValue(GetEntityKey(instance.ClassName, id), Result) then
    SetLength(Result, 0);
end;

function TEntityMap.GetMemberValue(const className, id, memberName: string): TValue;
var
  entityMapValue: TEntityMapValue;
  pair: TPair<string,TValue>;
begin
  if fEntityValues.TryGetValue(GetEntityKey(className, id), entityMapValue) then
    for pair in entityMapValue do
      if pair.Key = memberName then
        Exit(pair.Value);

  Result := TValue.Empty;
end;

function TEntityMap.GetEntityKey(const instance: TObject): TEntityMapKey;
var
  id: string;
begin
  id := TEntityCache.Get(instance.ClassType).GetPrimaryKeyValueAsString(instance);
  Result := GetEntityKey(instance.ClassName, id);
end;

function TEntityMap.GetEntityKey(const className, id: string): TEntityMapKey;
begin
  Result := className + '$' + id;
end;

function TEntityMap.IsMapped(const instance: TObject): Boolean;
begin
  Result := fEntityValues.ContainsKey(GetEntityKey(instance));
end;

procedure TEntityMap.PutEntity(const entity: IEntityWrapper);
var
  col: TColumnData;
  columnValue: TValue;
  values: TEntityMapValue;
  i: Integer;
  pair: TPair<TEntityMapKey,TEntityMapValue>;
  key: string;
  id: string;
begin
  columnValue := entity.GetPrimaryKeyValue;
  if columnValue.IsEmpty then
    Exit;
  id := columnValue.ToString;
  if id = '' then
    Exit;

  SetLength(values, entity.GetColumnsToMap.Count);
  for i := 0 to entity.GetColumnsToMap.Count - 1 do
  begin
    col := entity.GetColumnsToMap[i];
    columnValue := entity.GetColumnValue(col.ColumnAttr);
    if columnValue.IsObject and (columnValue.AsObject <> nil) then
      columnValue := TRttiExplorer.Clone(columnValue.AsObject);
    values[i].Key := col.MemberName;
    values[i].Value := columnValue;
  end;
  key := GetEntityKey(entity.GetEntity.ClassName, id);
  fCriticalSection.Enter;
  try
    pair := fEntityValues.ExtractPair(key);
    fEntityValues.Add(key, values);
  finally
    fCriticalSection.Leave;
    FinalizeItem(pair);
  end;
end;

procedure TEntityMap.Remove(const instance: TObject);
var
  id: string;
  entityData: TEntityData;
  pair: TPair<TEntityMapKey,TEntityMapValue>;
begin
  entityData := TEntityCache.Get(instance.ClassType);
  id := entityData.GetPrimaryKeyValueAsString(instance);
  fCriticalSection.Enter;
  try
    pair := fEntityValues.ExtractPair(GetEntityKey(instance.ClassName, id));
  finally
    fCriticalSection.Leave;
    FinalizeItem(pair);
  end;
end;

{$ENDREGION}


end.
