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

unit Spring.Persistence.SQL.Commands.Update;

{$I Spring.inc}

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
    FTable: TSQLTable;
    FCommand: TUpdateCommand;
    FColumns: IList<ColumnAttribute>;
    FEntityMap: TEntityMap;
    FEntityCache: TEntityData;
    FMapped: Boolean;
  protected
    function GetCommand: TDMLCommand; override;
    function TryIncrementVersionFor(AEntity: TObject): Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
    procedure Update(AEntity: TObject; AEntity2: TObject); overload;

    property EntityMap: TEntityMap read FEntityMap write FEntityMap;
  end;

implementation

uses
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;

{ TUpdateCommand }

procedure TUpdateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
  LDirtyObject: TObject;
begin
  Assert(Assigned(AEntity));

  if FEntityCache.HasVersionColumn and not TryIncrementVersionFor(AEntity) then
    raise EORMOptimisticLockException.Create(AEntity);

  LStmt := Connection.CreateStatement;

  FMapped := FEntityMap.IsMapped(AEntity);
  if FMapped then
  begin
    LDirtyObject := FEntityMap.Get(AEntity);
    FColumns.Clear;
    TRttiExplorer.GetChangedMembers(AEntity, LDirtyObject, FColumns);
    if (FColumns.Count = 1) and (FColumns.First.IsVersionColumn) then
      Exit;
  end;

  FCommand.SetTable(FColumns);
  FCommand.Entity := AEntity;

  SQL := Generator.GenerateUpdate(FCommand);

  if (SQL = '') then
    Exit;

  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);

    inherited Execute(AEntity);

    LStmt.Execute;
  finally
    LStmt := nil;
  end;
end;

function TUpdateExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

function TUpdateExecutor.TryIncrementVersionFor(AEntity: TObject): Boolean;
var
  LStatement: IDBStatement;
  LVersionValue, LPKValue: TValue;
  LQuery: Variant;
  LQueryMetadata: TQueryMetadata;
begin
  LStatement := Connection.CreateStatement;
  LVersionValue := TRttiExplorer.GetMemberValue(AEntity, FEntityCache.VersionColumn.ClassMemberName);
  LPKValue := TRttiExplorer.GetMemberValue(AEntity, FEntityCache.PrimaryKeyColumn.ClassMemberName);
  LQuery := Generator.GetUpdateVersionFieldQuery(FCommand, FEntityCache.VersionColumn
    , TUtils.AsVariant(LVersionValue), TUtils.AsVariant(LPKValue));
  LQueryMetadata.QueryType := ctUpdateVersion;
  case VarType(LQuery) of
    varUString, varString, varStrArg, varOleStr: LStatement.SetSQLCommand(LQuery)
    else
    begin
      LQueryMetadata.TableName := FCommand.Table.Name;
      LStatement.SetQuery(LQueryMetadata, LQuery);
    end;
  end;
  Result := (LStatement.Execute > 0);
  if Result then
    TRttiExplorer.SetMemberValueSimple(AEntity, FEntityCache.VersionColumn.ClassMemberName, LVersionValue.AsInteger + 1);
end;

procedure TUpdateExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  EntityClass := AClass;
  FEntityCache := TEntityCache.Get(EntityClass);
  LAtrTable := FEntityCache.EntityTable;

  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);
  FColumns.Clear;
  FColumns.AddRange(FEntityCache.Columns);

  FCommand.PrimaryKeyColumn := FEntityCache.PrimaryKeyColumn;
   //add fields to tsqltable
//  FCommand.SetTable(FColumns);

 // SQL := Generator.GenerateUpdate(FCommand);
end;

procedure TUpdateExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LColumn: ColumnAttribute;
begin
  inherited BuildParams(AEntity);

  for LColumn in FColumns do
  begin
    if LColumn.CanUpdate then
    begin
      LParam := CreateParam(AEntity, LColumn);
      SQLParameters.Add(LParam);
    end;
  end;

  if Assigned(FCommand.PrimaryKeyColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

constructor TUpdateExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FColumns := TCollections.CreateList<ColumnAttribute>;
  FCommand := TUpdateCommand.Create(FTable);
end;

destructor TUpdateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Update(AEntity, AEntity2: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
