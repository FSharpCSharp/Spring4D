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

unit Spring.Persistence.SQL.Commands.Select;

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
    fSelectColumn: ColumnAttribute;
    fForeignEntityClass: TClass;
  protected
    function GetCommand: TDMLCommand; override;
    function ShouldFetchFromOneColumn: Boolean;
  public
    constructor Create; overload; override;
    constructor Create(const id: TValue; selectColumn: ColumnAttribute); reintroduce; overload;
    destructor Destroy; override;

    procedure DoExecute; virtual;
    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    function Select: IDBResultset;
    function SelectAll(selectEntityClass: TClass): IDBResultset;

    property Command: TSelectCommand read fCommand;
    property ID: TValue read fID write fID;
    property ForeignEntityClass: TClass read fForeignEntityClass write fForeignEntityClass;
    property SelectColumn: ColumnAttribute read fSelectColumn write fSelectColumn;
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

  if ShouldFetchFromOneColumn then
    fColumns.Add(fSelectColumn)
  else
    fColumns.AddRange(EntityData.SelectColumns);

  fCommand.PrimaryKeyColumn := EntityData.PrimaryKeyColumn;
  fCommand.SetCommandFieldsFromColumns(fColumns);
  fCommand.SetAssociations(entityClass);
end;

procedure TSelectExecutor.BuildParams(const entity: TObject);
var
  LParam: TDBParam;
  LColumnName: string;
  LWhereField: TSQLWhereField;
begin
  inherited BuildParams(entity);
  Assert(not Assigned(entity), 'Entity should not be assigned here');

  LColumnName := fCommand.PrimaryKeyColumn.ColumnName;
  if Assigned(fForeignEntityClass) then
    LColumnName := fCommand.ForeignColumn.Name;

  for LWhereField in fCommand.WhereFields do
  begin
    LParam := DoCreateParam(LWhereField, TUtils.AsVariant(fID));
    SQLParameters.Add(LParam);
  end;
end;

constructor TSelectExecutor.Create(const id: TValue;
  selectColumn: ColumnAttribute);
begin
  Create;
  fID := id;
  fSelectColumn := selectColumn;
end;

procedure TSelectExecutor.DoExecute;
begin
  fCommand.WhereFields.Clear;

  if Assigned(fForeignEntityClass) then
    fCommand.SetFromForeignColumn(EntityClass, fForeignEntityClass)
  else
    fCommand.SetFromPrimaryColumn;

  SQL := Generator.GenerateSelect(fCommand);
end;

function TSelectExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TSelectExecutor.Select: IDBResultset;
var
  LStmt: IDBStatement;
begin
  DoExecute;
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(nil);
  if SQLParameters.Any then
    LStmt.SetParams(SQLParameters);

  Result := LStmt.ExecuteQuery;
end;

function TSelectExecutor.SelectAll(selectEntityClass: TClass): IDBResultSet;
var
  LStmt: IDBStatement;
begin
  fCommand.WhereFields.Clear;
  SQL := Generator.GenerateSelect(fCommand);

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  Result := LStmt.ExecuteQuery;
end;

function TSelectExecutor.ShouldFetchFromOneColumn: Boolean;
begin
  Result := Assigned(fSelectColumn);
end;

{$ENDREGION}

end.
