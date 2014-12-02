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

unit Spring.Persistence.SQL.Commands.Update;

interface

uses
  Classes,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.EntityMap,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing <c>update</c> statements.
  ///	</summary>
  {$ENDREGION}
  TUpdateExecutor = class(TAbstractCommandExecutor)
  private
    fTable: TSQLTable;
    fCommand: TUpdateCommand;
    fColumns: IList<ColumnAttribute>;
    fEntityMap: TEntityMap;
  protected
    function GetCommand: TDMLCommand; override;
    function TryIncrementVersionFor(AEntity: TObject): Boolean; virtual;
    function HasChangedVersionColumnOnly: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    procedure Execute(const entity: TObject);

    property EntityMap: TEntityMap read fEntityMap write fEntityMap;
  end;

implementation

uses
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TUpdateCommand'}

constructor TUpdateExecutor.Create;
begin
  inherited Create;
  fTable := TSQLTable.Create;
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fCommand := TUpdateCommand.Create(fTable);
end;

destructor TUpdateExecutor.Destroy;
begin
  fTable.Free;
  fCommand.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
  LSql: string;
begin
  Assert(Assigned(entity));

  if EntityData.HasVersionColumn and not TryIncrementVersionFor(entity) then
    raise EORMOptimisticLockException.Create(entity);

  LStmt := Connection.CreateStatement;
  LSql := SQL;

  fColumns.Clear;
  if fEntityMap.IsMapped(entity) then
  begin
    fColumns := fEntityMap.GetChangedMembers(entity, EntityData);
    if HasChangedVersionColumnOnly then
      Exit;
    fCommand.SetCommandFieldsFromColumns(fColumns);
  end
  else
    fColumns.AddRange(EntityData.Columns);

  if fCommand.UpdateFields.IsEmpty then
    Exit;

  fCommand.Entity := entity;
  LSql := Generator.GenerateUpdate(fCommand);
  //NoSQL db generators can't prebuild query without entity object, so they return empty string.
 // if (LSql = '') then
  //  LSql := Generator.GenerateUpdate(fCommand);

  if (LSql = '') then
    raise EORMCannotGenerateQueryStatement.Create(entity);

  LStmt.SetSQLCommand(LSql);
  BuildParams(entity);
  LStmt.SetParams(SQLParameters);
  LStmt.Execute;
end;

function TUpdateExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TUpdateExecutor.HasChangedVersionColumnOnly: Boolean;
begin
  Result := (fColumns.Count = 1) and (fColumns.First.IsVersionColumn);
end;

function TUpdateExecutor.TryIncrementVersionFor(AEntity: TObject): Boolean;
var
  LStatement: IDBStatement;
  LVersionValue, LPKValue: TValue;
  LQuery: Variant;
  LQueryMetadata: TQueryMetadata;
begin
  LStatement := Connection.CreateStatement;
  LVersionValue := TRttiExplorer.GetMemberValue(AEntity, EntityData.VersionColumn.MemberName);
  LPKValue := TRttiExplorer.GetMemberValue(AEntity, EntityData.PrimaryKeyColumn.MemberName);
  LQuery := Generator.GetUpdateVersionFieldQuery(fCommand, EntityData.VersionColumn,
    TUtils.AsVariant(LVersionValue), TUtils.AsVariant(LPKValue));
  LQueryMetadata.QueryOperation := ctUpdateVersion;  
  LQueryMetadata.TableName := fCommand.Table.Name;
  LStatement.SetQuery(LQueryMetadata, LQuery);

  Result := (LStatement.Execute > 0);
  if Result then
    TRttiExplorer.SetMemberValueSimple(AEntity, EntityData.VersionColumn.MemberName, LVersionValue.AsInteger + 1);
end;

procedure TUpdateExecutor.Build(entityClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  inherited Build(entityClass);
  LAtrTable := EntityData.EntityTable;

  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [entityClass.ClassName]);

  fTable.SetFromAttribute(LAtrTable);
  fColumns.Clear;
  fColumns.AddRange(EntityData.Columns);

  fCommand.PrimaryKeyColumn := EntityData.PrimaryKeyColumn;
   //add fields to tsqltable
  fCommand.SetCommandFieldsFromColumns(fColumns);
  SQL := Generator.GenerateUpdate(fCommand);
end;

procedure TUpdateExecutor.BuildParams(const entity: TObject);
var
  LParam: TDBParam;
  LUpdateField: TSQLUpdateField;
  LWhereField: TSQLWhereField;
begin
  inherited BuildParams(entity);

  for LUpdateField in fCommand.UpdateFields do
  begin
    LParam := CreateParam(entity, LUpdateField);
    SQLParameters.Add(LParam);
  end;

  for LWhereField in fCommand.WhereFields do
  begin
    LParam := CreateParam(entity, LWhereField);
    SQLParameters.Add(LParam);
  end;
end;

{$ENDREGION}


end.
