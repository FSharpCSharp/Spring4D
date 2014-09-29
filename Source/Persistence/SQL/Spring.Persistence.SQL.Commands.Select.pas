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
  /// <summary>
  ///   Represents <c>select</c> executor. Responsible for building and
  ///   executing <c>select</c> statements.
  /// </summary>
  TSelectExecutor = class(TAbstractCommandExecutor)
  private
    fTable: TSQLTable;
    fCommand: TSelectCommand;
    fColumns: IList<ColumnAttribute>;
    fID: TValue;
    fLazyColumn: ColumnAttribute;
    fSelectEntityClass: TClass;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(const entity: TObject); override;
    procedure DoExecute(const entity: TObject); virtual;
    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    function Select(const entity: TObject; selectEntityClass: TClass): IDBResultset;
    function SelectAll(selectEntityClass: TClass): IDBResultset;

    property Command: TSelectCommand read fCommand;
    property ID: TValue read fID write fID;
    property LazyColumn: ColumnAttribute read fLazyColumn write fLazyColumn;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Params;


{$REGION 'TSelectCommand'}

constructor TSelectExecutor.Create;
begin
  inherited Create;
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fTable := TSQLTable.Create;
  fCommand := TSelectCommand.Create(fTable);
end;

destructor TSelectExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TSelectExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);
  if not EntityData.IsTableEntity then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [entityClass.ClassName]);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fColumns.Clear;
  fColumns.AddRange(EntityData.Columns);

  fCommand.PrimaryKeyColumn := EntityData.PrimaryKeyColumn;
  fCommand.SetCommandFieldsFromColumns(fColumns);
  fCommand.SetAssociations(entityClass);
end;

procedure TSelectExecutor.BuildParams(const entity: TObject);
var
  LParam: TDBParam;
begin
  inherited BuildParams(entity);

  if Assigned(fCommand.ForeignColumn) then
  begin
    LParam := CreateParam(entity, fCommand.ForeignColumn);
    SQLParameters.Add(LParam);
  end
  else if Assigned(fCommand.PrimaryKeyColumn) then
  begin
    if entity = nil then
      LParam := DoCreateParam(fCommand.PrimaryKeyColumn, TUtils.AsVariant(fID))
    else
      LParam := CreateParam(entity, fCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

procedure TSelectExecutor.DoExecute(const entity: TObject);
var
  LSelects: IList<TSQLSelectField>;
begin
  //add where fields if needed
  fCommand.WhereFields.Clear;
  {DONE -oLinas -cGeneral : Must know for what column to set where condition field and value. Setting always for primary key is not good for foreign key tables}
   //AEntity - base table class type
   //EntityClass - can be foreign table class type
  if EntityClass = fSelectEntityClass then
    fCommand.SetFromPrimaryColumn  //select from the same as base table
  else
    fCommand.SetFromForeignColumn(fSelectEntityClass, EntityClass);

  if Assigned(fLazyColumn) then
  begin
    LSelects := TCollections.CreateList<TSQLSelectField>;
    LSelects.AddRange(fCommand.SelectFields);
    (fCommand.SelectFields as ICollectionOwnership).OwnsObjects := False;
    fCommand.SelectFields.Clear;
    fCommand.SelectFields.Add(TSQLSelectField.Create(fLazyColumn.Name, fTable));
    SQL := Generator.GenerateSelect(fCommand);
    (fCommand.SelectFields as ICollectionOwnership).OwnsObjects := True;
    fCommand.SelectFields.Clear;
    fCommand.SelectFields.AddRange(LSelects);
  end
  else
    SQL := Generator.GenerateSelect(fCommand);
end;

procedure TSelectExecutor.Execute(const entity: TObject);
begin
  // do nothing
end;

function TSelectExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TSelectExecutor.Select(const entity: TObject;
  selectEntityClass: TClass): IDBResultset;
var
  LStmt: IDBStatement;
begin
  fSelectEntityClass := selectEntityClass;
  DoExecute(entity);
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(entity);
  if SQLParameters.Any then
    LStmt.SetParams(SQLParameters);

  Result := LStmt.ExecuteQuery;
end;

function TSelectExecutor.SelectAll(selectEntityClass: TClass): IDBResultSet;
var
  LStmt: IDBStatement;
begin
  fSelectEntityClass := selectEntityClass;
  fCommand.WhereFields.Clear;
  SQL := Generator.GenerateSelect(fCommand);

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  Result := LStmt.ExecuteQuery;
end;

{$ENDREGION}


end.
