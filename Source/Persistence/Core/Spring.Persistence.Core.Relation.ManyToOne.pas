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
    FNewColumns: TColumnDataList;
    FNewEntityClass: TClass;
    FMappedByCol: ColumnAttribute;
    FNewTableName: string;
    FEntityData: TEntityData;
  protected
    function DoBuildColumnName(AColumn: TColumnData): string; virtual;
    procedure ResolveColumns(AResultset: IDBResultset); virtual;
  public
    NewEntity: TObject;

    constructor Create; virtual;
    destructor Destroy; override;

    class function BuildColumnName(const ATableName, AMappedByColName, AColumnName: string): string;
    class function GetMappedByColumn(AFromColumn: ManyToOneAttribute; AClass: TClass): ColumnAttribute;

    procedure SetAssociation(const attribute: TORMAttribute;
      const entity: TObject; const resultSet: IDBResultset); override;

    property EntityData: TEntityData read FEntityData;
    property NewEntityClass: TClass read FNewEntityClass;
    property NewColumns: TColumnDataList read FNewColumns;
  end;

implementation

uses
  SysUtils,
  Spring.Reflection,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer;

{ TManyToOneRelation }

class function TManyToOneRelation.BuildColumnName(const ATableName, AMappedByColName,
  AColumnName: string): string;
begin
  Result := Format('%0:S_%1:S_%2:S', [
                                      ATableName                  //table name
                                     ,AMappedByColName              //mapped by column name
                                     ,AColumnName                   //column name
                                      ]);
end;

constructor TManyToOneRelation.Create;
begin
  inherited Create;
  FNewColumns := nil;
end;

destructor TManyToOneRelation.Destroy;
begin
  if Assigned(FNewColumns) then
    FNewColumns.Free;
  inherited Destroy;
end;

function TManyToOneRelation.DoBuildColumnName(AColumn: TColumnData): string;
begin
  Result := BuildColumnName(FNewTableName, FMappedByCol.Name, AColumn.Name);
end;

class function TManyToOneRelation.GetMappedByColumn(AFromColumn: ManyToOneAttribute; AClass: TClass): ColumnAttribute;
begin
  Result := nil;
  if not TRttiExplorer.TryGetColumnByMemberName(AClass, AFromColumn.MappedBy, Result) then
    raise EORMManyToOneMappedByColumnNotFound.CreateFmt('Mapped by column ("%S") not found in the base class "%S".'
      , [AFromColumn.MappedBy, AClass.ClassName]);
end;

procedure TManyToOneRelation.ResolveColumns(AResultset: IDBResultset);
var
  LCol: TColumnData;
  LColName: string;
  i: Integer;
begin
  for i := FNewColumns.Count - 1 downto 0 do
  begin
    LCol := FNewColumns[i];
    LColName := DoBuildColumnName(LCol);
    if not AResultset.FieldNameExists(LColName) then
    begin
      FNewColumns.Delete(i);
      Continue;
    end;
    LCol.Name := LColName;
    FNewColumns[i] := LCol;
    if LCol.IsPrimaryKey then
      FNewColumns.PrimaryKeyColumn := LCol;
  end;
end;

procedure TManyToOneRelation.SetAssociation(const attribute: TORMAttribute;
  const entity: TObject; const resultSet: IDBResultset);
var
  LCol: ManyToOneAttribute;
begin
  LCol := attribute as ManyToOneAttribute;
  //check if entity has associations attributes
  FNewEntityClass := TType.GetType(LCol.GetColumnTypeInfo).AsInstance.MetaclassType;
  FEntityData := TEntityCache.Get(FNewEntityClass);
  FNewTableName := FEntityData.EntityTable.TableName;
  NewEntity := TRttiExplorer.CreateType(FNewEntityClass);
  if Assigned(FNewColumns) then
    FNewColumns.Free;
  FNewColumns := TEntityCache.CreateColumnsData(FNewEntityClass);
  //get column name from MappedBy field or property
  FMappedByCol := GetMappedByColumn(LCol, entity.ClassType);
  //resolve columns which we need to set
  ResolveColumns(resultSet);
  //assign newly created column to base entity property
  TRttiExplorer.SetMemberValueSimple(entity, LCol.ClassMemberName, NewEntity);
end;

end.
