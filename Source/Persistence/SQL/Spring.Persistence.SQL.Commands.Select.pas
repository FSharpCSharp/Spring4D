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

unit Spring.Persistence.SQL.Commands.Select;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  TSelectType = (stOne, stList, stObjectList);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>select</c> executor. Responsible for building and
  ///	  executing <c>select</c> statements.
  ///	</summary>
  {$ENDREGION}
  TSelectExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TSelectCommand;
    FColumns: IList<ColumnAttribute>;
    FID: TValue;
    FLazyColumn: ColumnAttribute;
    FSelectEntityClassType: TClass;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure DoExecute(AEntity: TObject); virtual;
    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    function Select(AEntity: TObject; AEntityClassType: TClass): IDBResultset;
    function SelectAll(AEntity: TObject; AEntityClassType: TClass): IDBResultset;

    property Command: TSelectCommand read FCommand;
    property ID: TValue read FID write FID;
    property LazyColumn: ColumnAttribute read FLazyColumn write FLazyColumn;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Params;

{ TSelectCommand }

procedure TSelectExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
  LCache: TEntityData;
begin
  EntityClass := AClass;
  LCache := TEntityCache.Get(EntityClass);
  LAtrTable := LCache.EntityTable;

  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);
  FColumns.Clear;
  FColumns.AddRange(LCache.Columns);

  FCommand.PrimaryKeyColumn := LCache.PrimaryKeyColumn;
  FCommand.SetTable(FColumns);
  FCommand.SetAssociations(EntityClass);
end;

procedure TSelectExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
begin
  inherited BuildParams(AEntity);

  if Assigned(FCommand.ForeignColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.ForeignColumn);
    SQLParameters.Add(LParam);
  end
  else if Assigned(FCommand.PrimaryKeyColumn) then
  begin
    if AEntity = nil then
      LParam := DoCreateParam(FCommand.PrimaryKeyColumn, TUtils.AsVariant(FID))
    else
      LParam := CreateParam(AEntity, FCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

constructor TSelectExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FColumns := TCollections.CreateList<ColumnAttribute>;
  FCommand := TSelectCommand.Create(FTable);
  FLazyColumn := nil;
end;

destructor TSelectExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

procedure TSelectExecutor.DoExecute(AEntity: TObject);
var
  LSelects: IList<TSQLSelectField>;
begin
  //add where fields if needed
  FCommand.WhereFields.Clear;
  {DONE -oLinas -cGeneral : Must know for what column to set where condition field and value. Setting always for primary key is not good for foreign key tables}
   //AEntity - base table class type
   //EntityClass - can be foreign table class type
  if EntityClass = FSelectEntityClassType then
  begin
    FCommand.SetFromPrimaryColumn;  //select from the same as base table
  end
  else
  begin
    //get foreign column name and compare it to the base table primary key column name
    FCommand.SetFromForeignColumn(FSelectEntityClassType, EntityClass);
  end;

  if Assigned(FLazyColumn) then
  begin
    LSelects := TCollections.CreateList<TSQLSelectField>;
    LSelects.AddRange(FCommand.SelectFields);
    (FCommand.SelectFields as ICollectionOwnership).OwnsObjects := False;
    FCommand.SelectFields.Clear;
    FCommand.SelectFields.Add(TSQLSelectField.Create(FLazyColumn.Name, FTable));
    SQL := Generator.GenerateSelect(FCommand);
    (FCommand.SelectFields as ICollectionOwnership).OwnsObjects := True;
    FCommand.SelectFields.Clear;
    FCommand.SelectFields.AddRange(LSelects);
  end
  else
    SQL := Generator.GenerateSelect(FCommand);
end;

function TSelectExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

function TSelectExecutor.Select(AEntity: TObject; AEntityClassType: TClass): IDBResultset;
var
  LStmt: IDBStatement;
begin
  FSelectEntityClassType := AEntityClassType;
  DoExecute(AEntity);
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  if SQLParameters.Count > 0 then
    LStmt.SetParams(SQLParameters);

  Execute(AEntity);

  Result := LStmt.ExecuteQuery;
end;

function TSelectExecutor.SelectAll(AEntity: TObject; AEntityClassType: TClass): IDBResultset;
var
  LStmt: IDBStatement;
begin
  FSelectEntityClassType := AEntityClassType;
  FCommand.WhereFields.Clear;
  SQL := Generator.GenerateSelect(FCommand);

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

 { BuildParams(AEntity);
  if SQLParameters.Count > 0 then
    LStmt.SetParams(SQLParameters); }

  Execute(AEntity);

  Result := LStmt.ExecuteQuery;
end;

end.
