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

unit Spring.Persistence.SQL.Commands.Delete;

interface

uses
  Rtti,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing <c>delete</c> statements. 
  /// </summary>
  TDeleteExecutor = class(TAbstractCommandExecutor)
  private
    fTable: TSQLTable;
    fCommand: TDeleteCommand;
  protected
    function GetCommand: TDMLCommand; override;
    function GetPrimaryKeyValue(const entity: TObject): TValue; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    procedure Execute(const entity: TObject);
  end;

  TDeleteByValueExecutor = class(TDeleteExecutor)
  private
    fPrimaryKeyValue: TValue;
  protected
    function GetPrimaryKeyValue(const entity: TObject): TValue; override;
  public
    procedure Execute(const entity: TObject);

    property PrimaryKeyValue: TValue read fPrimaryKeyValue write fPrimaryKeyValue;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TDeleteCommand'}

constructor TDeleteExecutor.Create;
begin
  inherited Create;
  fTable := TSQLTable.Create;
  fCommand := TDeleteCommand.Create(fTable);
end;

destructor TDeleteExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TDeleteExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);
  if not EntityData.IsTableEntity then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [entityClass.ClassName]);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.PrimaryKeyColumnName := EntityData.PrimaryKeyColumn.ColumnName;
  SQL := Generator.GenerateDelete(fCommand);
end;

procedure TDeleteExecutor.BuildParams(const entity: TObject);
var
  LParam: TDBParam;
  LWhereField: TSQLWhereField;
begin
  Assert(EntityData.PrimaryKeyColumn <> nil);
  inherited BuildParams(entity);

  for LWhereField in fCommand.WhereFields do
  begin
    LParam := DoCreateParam(LWhereField, TUtils.AsVariant(GetPrimaryKeyValue(entity)));
    SQLParameters.Add(LParam);
  end;
end;

procedure TDeleteExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(entity));

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  BuildParams(entity);
  try
    LStmt.SetParams(SQLParameters);
    LStmt.Execute;
  finally
    LStmt := nil;
  end;
end;

function TDeleteExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TDeleteExecutor.GetPrimaryKeyValue(const entity: TObject): TValue;
begin
  Result := TRttiExplorer.GetPrimaryKeyValue(entity);
end;

{$ENDREGION}


{$REGION 'TDeleteByValueExecutor'}

procedure TDeleteByValueExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
begin
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  BuildParams(entity);
  try
    LStmt.SetParams(SQLParameters);
    LStmt.Execute;
  finally
    LStmt := nil;
  end;
end;

function TDeleteByValueExecutor.GetPrimaryKeyValue(const entity: TObject): TValue;
begin
  Result := fPrimaryKeyValue;
end;

{$ENDREGION}


end.
