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
  Rtti,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes
  ;

type
  TEntityMapKey = string;
  TEntityMapValue = TArray<TPair<string, TValue>>;

  TEntityMap = class
  private
    fEntityValues: TDictionary<TEntityMapKey, TEntityMapValue>;
    fCriticalSection: ICriticalSection;
  protected
    function GetEntityKey(const instance: TObject): TEntityMapKey; overload;
    function GetEntityKey(const instance: TObject; const id: String): TEntityMapKey; overload;
    function GetEntityValues(const instance: TObject; const id: String): TEntityMapValue;

    procedure PutEntity(const instance: TObject; const id: String; entityDetails: TEntityData);

    procedure FinalizeItem(const item: TPair<TEntityMapKey, TEntityMapValue>);
  public
    constructor Create;
    destructor Destroy; override;

    function IsMapped(const instance: TObject): Boolean;  
    procedure AddOrReplace(const instance: TObject);
    procedure Remove(const instance: TObject);
    
    function GetChangedMembers(const instance: TObject; const entityDetails: TEntityData): IList<ColumnAttribute>;      
    procedure Clear;   
  end;

implementation

uses
  Spring.Reflection,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection  
  ;


{$REGION 'TEntityMap'}

constructor TEntityMap.Create;
begin
  inherited Create;
  fCriticalSection := TInterfacedCriticalSection.Create;
  fEntityValues := TDictionary<TEntityMapKey,TEntityMapValue>.Create;
end;

destructor TEntityMap.Destroy;
begin
  Clear;
  fEntityValues.Free;
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
    TFinalizer.FinalizeInstance(value);
  end;
end;

procedure TEntityMap.AddOrReplace(const instance: TObject);
var
  id: String;
  entityDetails: TEntityData;
begin
  Assert(Assigned(instance), 'Entity not assigned');
  entityDetails := TEntityCache.Get(instance.ClassType);
  id := entityDetails.GetPrimaryKeyValueAsString(instance);
  PutEntity(instance, id, entityDetails);
end;

procedure TEntityMap.Clear;
var
  pair: TPair<TEntityMapKey,TEntityMapValue>;
begin
  for pair in fEntityValues do
  begin
    FinalizeItem(pair);
  end;
  fEntityValues.Clear;
end;

function TEntityMap.GetChangedMembers(const instance: TObject; const entityDetails: TEntityData): IList<ColumnAttribute>;
var
  currentValue, dirtyValue: TValue;
  id: String;
  values: TEntityMapValue;
  i: Integer;
begin
  Result := TCollections.CreateList<ColumnAttribute>;  
  id := entityDetails.GetPrimaryKeyValueAsString(instance);
  values := GetEntityValues(instance, id);

  for i := 0 to entityDetails.Columns.Count - 1 do
  begin
    currentValue := TRttiExplorer.GetMemberValue(instance, values[i].Key);
    dirtyValue := values[i].Value;
    if not Spring.Persistence.Core.Reflection.SameValue(currentValue, dirtyValue) then
      Result.Add(entityDetails.Columns[i]);
  end;
end;

function TEntityMap.GetEntityValues(const instance: TObject; const id: String): TEntityMapValue;
begin
  if not fEntityValues.TryGetValue(GetEntityKey(instance, id), Result) then
    SetLength(Result, 0);
end;

function TEntityMap.GetEntityKey(const instance: TObject): TEntityMapKey;
var
  id: String;
begin
  id := TEntityCache.Get(instance.ClassType).GetPrimaryKeyValueAsString(instance);
  Result := GetEntityKey(instance, id);
end;

function TEntityMap.GetEntityKey(const instance: TObject; const id: String): TEntityMapKey;
begin
  Result := instance.ClassName + '$' + id;
end;

function TEntityMap.IsMapped(const instance: TObject): Boolean;
begin
  Result := fEntityValues.ContainsKey(GetEntityKey(instance));
end;

procedure TEntityMap.PutEntity(const instance: TObject; const id: String; entityDetails: TEntityData);
var
  col: ColumnAttribute;
  columnValue: TValue;
  values: TEntityMapValue;
  i: Integer;
  pair: TPair<TEntityMapKey,TEntityMapValue>;
  key: string;
begin
  if (id = '') then
    Exit;

  //commented out implementation is cleaner, but slower as well
  SetLength(values, entityDetails.Columns.Count);
  for i:=0 to entityDetails.Columns.Count-1 do
  begin
    col := entityDetails.Columns[i];
    columnValue := TRttiExplorer.GetMemberValue(instance, col.MemberName);
    if (columnValue.IsObject) and (columnValue.AsObject <> nil) then
      columnValue := TRttiExplorer.Clone(columnValue.AsObject);
    values[i].Key := col.MemberName;
    values[i].Value := columnValue;
  end;
  key := GetEntityKey(instance, id);
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
  id: String;
  entityDetails: TEntityData;
  pair: TPair<TEntityMapKey,TEntityMapValue>;
begin
  entityDetails := TEntityCache.Get(instance.ClassType);
  id := entityDetails.GetPrimaryKeyValueAsString(instance);
  fCriticalSection.Enter;
  try
    pair := fEntityValues.ExtractPair(GetEntityKey(instance, id));
  finally
    fCriticalSection.Leave;
    FinalizeItem(pair);
  end;
end;

{$ENDREGION}


end.
