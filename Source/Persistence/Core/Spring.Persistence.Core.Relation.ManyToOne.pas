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

unit Spring.Persistence.Core.Relation.ManyToOne;

interface

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Relation.Abstract,
  Spring.Persistence.Mapping.Attributes;

type
  TManyToOneRelation = class(TAbstractRelation)
  private
    fEntityData: TEntityData;
    fMappedByColumn: ColumnAttribute;
    fNewColumns: TColumnDataList;
    fNewEntityClass: TClass;
    fNewTableName: string;
  protected
    function DoBuildColumnName(const column: TColumnData): string; virtual;
    procedure ResolveColumns(const resultSet: IDBResultSet); virtual;
  public
    NewEntity: TObject;

    constructor Create; virtual;
    destructor Destroy; override;

    class function BuildColumnName(
      const tableName, mappedByColumnName, columnName: string): string;
    class function GetMappedByColumn(const fromColumn: ManyToOneAttribute;
      entityClass: TClass): ColumnAttribute;

    procedure SetAssociation(const attribute: TORMAttribute;
      const entity: TObject; const resultSet: IDBResultSet); override;

    property EntityData: TEntityData read fEntityData;
    property NewColumns: TColumnDataList read fNewColumns;
    property NewEntityClass: TClass read fNewEntityClass;
  end;

implementation

uses
  SysUtils,
  Spring.Helpers,
  Spring.Reflection,
  Spring.Reflection.Activator,
  Spring.Persistence.Core.Exceptions;


{$REGION 'TManyToOneRelation'}

constructor TManyToOneRelation.Create;
begin
  inherited Create;
  fNewColumns := nil;
end;

destructor TManyToOneRelation.Destroy;
begin
  if Assigned(fNewColumns) then
    fNewColumns.Free;
  inherited Destroy;
end;

class function TManyToOneRelation.BuildColumnName(const tableName, mappedByColumnName,
  columnName: string): string;
begin
  Result := Format('%0:S_%1:S_%2:S', [tableName, mappedByColumnName, columnName]);
end;

function TManyToOneRelation.DoBuildColumnName(const column: TColumnData): string;
begin
  Result := BuildColumnName(fNewTableName, fMappedByColumn.Name, column.Name);
end;

class function TManyToOneRelation.GetMappedByColumn(
  const fromColumn: ManyToOneAttribute; entityClass: TClass): ColumnAttribute;
begin
  Result := nil;
  if not TEntityCache.TryGetColumnByMemberName(entityClass, fromColumn.MappedBy, Result) then
    raise EORMManyToOneMappedByColumnNotFound.CreateFmt(
      'Mapped by column ("%S") not found in the base class "%S".',
      [fromColumn.MappedBy, entityClass.ClassName]);
end;

procedure TManyToOneRelation.ResolveColumns(const resultSet: IDBResultSet);
var
  i: Integer;
  columnData: TColumnData;
  columnName: string;
begin
  for i := fNewColumns.Count - 1 downto 0 do
  begin
    columnData := fNewColumns[i];
    columnName := DoBuildColumnName(columnData);
    if not resultSet.FieldNameExists(columnName) then
    begin
      fNewColumns.Delete(i);
      Continue;
    end;
    columnData.Name := columnName;
    fNewColumns[i] := columnData;
    if columnData.IsPrimaryKey then
      fNewColumns.PrimaryKeyColumn := columnData;
  end;
end;

procedure TManyToOneRelation.SetAssociation(const attribute: TORMAttribute;
  const entity: TObject; const resultSet: IDBResultSet);
var
  column: ManyToOneAttribute;
begin
  column := attribute as ManyToOneAttribute;
  //check if entity has associations attributes
  fNewEntityClass := TType.GetType(column.GetColumnTypeInfo).AsInstance.MetaclassType;
  fEntityData := TEntityCache.Get(fNewEntityClass);
  fNewTableName := fEntityData.EntityTable.TableName;
  NewEntity := TActivator.CreateInstance(fNewEntityClass);
  if Assigned(fNewColumns) then
    fNewColumns.Free;
  fNewColumns := TEntityCache.CreateColumnsData(fNewEntityClass);
  //get column name from MappedBy field or property
  fMappedByColumn := GetMappedByColumn(column, entity.ClassType);
  //resolve columns which we need to set
  ResolveColumns(resultSet);
  //assign newly created column to base entity property
  TType.SetMemberValue(entity, column.ClassMemberName, NewEntity);
end;

{$ENDREGION}


end.
