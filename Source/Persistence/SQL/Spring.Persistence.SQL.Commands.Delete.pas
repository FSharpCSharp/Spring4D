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
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing <c>delete</c> statements. 
  ///	</summary>
  {$ENDREGION}
  TDeleteExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TDeleteCommand;
    FPrimaryKeyColumnName: string;
    FEntity: TObject;
  protected
    function GetCommand: TDMLCommand; override;

    function GetPrimaryKeyValue: TValue; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
  end;

  TDeleteByValueExecutor = class(TDeleteExecutor)
  private
    FPrimaryKeyValue: TValue;
  protected
    function GetPrimaryKeyValue: TValue; override;
  public
    procedure Execute(AEntity: TObject); override;

    property PrimaryKeyValue: TValue read FPrimaryKeyValue write FPrimaryKeyValue;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;

{ TDeleteCommand }

procedure TDeleteExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  EntityClass := AClass;
  LAtrTable := TRttiExplorer.GetTable(EntityClass);
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);

  FPrimaryKeyColumnName := TRttiExplorer.GetPrimaryKeyColumnName(EntityClass);
  FCommand.PrimaryKeyColumnName := FPrimaryKeyColumnName;
  SQL := Generator.GenerateDelete(FCommand);
end;

procedure TDeleteExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LVal: TValue;
begin
  Assert(FPrimaryKeyColumnName <> '');
  FEntity := AEntity;
  inherited BuildParams(AEntity);

  LParam := TDBParam.Create;
  LParam.Name := Command.GetExistingParameterName(FPrimaryKeyColumnName);
  LVal :=  GetPrimaryKeyValue;
  LParam.Value := TUtils.AsVariant(LVal);
  SQLParameters.Add(LParam);
end;

constructor TDeleteExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FCommand := TDeleteCommand.Create(FTable);
  FPrimaryKeyColumnName := '';
end;

procedure TDeleteExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(AEntity));

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);
    LStmt.Execute;
  finally
    LStmt := nil;
  end;
end;

function TDeleteExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

function TDeleteExecutor.GetPrimaryKeyValue: TValue;
begin
  Result := TRttiExplorer.GetPrimaryKeyValue(FEntity);
end;

destructor TDeleteExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

{ TDeleteByValueExecutor }

procedure TDeleteByValueExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);
    LStmt.Execute;
  finally
    LStmt := nil;
  end;
end;

function TDeleteByValueExecutor.GetPrimaryKeyValue: TValue;
begin
  Result := FPrimaryKeyValue;
end;

end.
