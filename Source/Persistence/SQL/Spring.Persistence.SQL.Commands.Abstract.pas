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

unit Spring.Persistence.SQL.Commands.Abstract;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TAbstractCommandExecutor = class
  private
    fConnection: IDBConnection;
    fGenerator: ISQLGenerator;
    fEntityClass: TClass;
    fSQL: string;
    fEntityData: TEntityData;
    fParams: IList<TDBParam>;
  protected
    function CanUpdateParamFieldType(const value: Variant): Boolean; virtual;

    function CreateParam(const paramField: TSQLParamField;
      const value: Variant): TDBParam; overload; virtual;
    function CreateParam(const entity: TObject;
      const paramField: TSQLParamField): TDBParam; overload; virtual;

    function GetCommand: TDMLCommand; virtual; abstract;

    property Command: TDMLCommand read GetCommand;
    property Connection: IDBConnection read fConnection;
    property EntityClass: TClass read fEntityClass;// write fEntityClass;
    property EntityData: TEntityData read fEntityData;// write fEntityData;
    property Generator: ISQLGenerator read fGenerator;
    property SQL: string read fSQL write fSQL;
    property SQLParameters: IList<TDBParam> read fParams;
  public
    constructor Create(const connection: IDBConnection); virtual;

    function TableExists(const tableName: string): Boolean; virtual;
    procedure FillDbTableColumns(const tableName: string; const columns: IList<string>); virtual;

    procedure Build(entityClass: TClass); virtual;
    procedure BuildParams(const entity: TObject); virtual;
  end;

implementation

uses
  Classes,
  Variants,
  Spring,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Register;


{$REGION 'TAbstractCommandExecutor'}

constructor TAbstractCommandExecutor.Create(const connection: IDBConnection);
begin
  Guard.CheckNotNull(connection, 'connection');
  inherited Create;
  fConnection := connection;
  fGenerator := TSQLGeneratorRegister.GetGenerator(fConnection.GetQueryLanguage);
  fParams := TCollections.CreateObjectList<TDBParam>;
end;

procedure TAbstractCommandExecutor.Build(entityClass: TClass);
begin
  fEntityClass := entityClass;
  fEntityData := TEntityCache.Get(entityClass);
end;

procedure TAbstractCommandExecutor.BuildParams(const entity: TObject);
begin
  fParams.Clear;
  if Assigned(Command) then
    Command.Entity := entity;
end;

function TAbstractCommandExecutor.CanUpdateParamFieldType(const value: Variant): Boolean;
begin
  Result := (VarIsNull(value) or VarIsEmpty(value)) and (Connection.GetQueryLanguage = qlOracle);
end;

function TAbstractCommandExecutor.CreateParam(const entity: TObject;
  const paramField: TSQLParamField): TDBParam;
var
  LVal, LRes: TValue;
  bFree: Boolean;
begin
  Result := TDBParam.Create;
  Result.Name := paramField.ParamName;
  LVal := TRttiExplorer.GetMemberValueDeep(entity, paramField.Column.MemberName);
  //convert/serialize objects to stream. If value is nullable or lazy get it's real value
  if LVal.IsObject and TryConvert(LVal, TypeInfo(TStream), LRes, bFree) then
    LVal := LRes.AsObject;

  Result.Value := TUtils.AsVariant(LVal);
  if CanUpdateParamFieldType(Result.Value) then
    Result.SetParamTypeFromTypeInfo(paramField.Column.MemberType);

  if bFree then
    FreeValueObject(LVal);
end;

function TAbstractCommandExecutor.CreateParam(
  const paramField: TSQLParamField; const value: Variant): TDBParam;
begin
  Result := TDBParam.Create;
  Result.Name := paramField.ParamName;
  Result.Value := value;
end;

procedure TAbstractCommandExecutor.FillDbTableColumns(const tableName: string;
  const columns: IList<string>);
var
  LSqlTableCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
  i: Integer;
begin
  LSqlTableCount := Generator.GetTableColumns(tableName);
  if (LSqlTableCount <> '') then
  begin
    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(LSqlTableCount);
    LResults := LStmt.ExecuteQuery;
    columns.Clear;
    for i := 0 to LResults.GetFieldCount - 1 do
      columns.Add(LResults.GetFieldName(i));
  end;
end;

function TAbstractCommandExecutor.TableExists(const tableName: string): Boolean;
var
  LSqlTableCount, LSqlTableExists: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
begin
  Result := False;
  LSqlTableCount := '';
  LSqlTableExists := Generator.GetSQLTableExists(tableName);
  if LSqlTableExists = '' then
    LSqlTableCount := Generator.GetSQLTableCount(tableName);
  if (LSqlTableCount <> '') or (LSqlTableExists <> '') then
  begin
    LStmt := Connection.CreateStatement;
    try
      try
        if (LSqlTableExists <> '') then
          LStmt.SetSQLCommand(LSqlTableExists)
        else
          LStmt.SetSQLCommand(LSqlTableCount);

        LResults := LStmt.ExecuteQuery;

        if (LSqlTableExists <> '') then
          Result := (LResults.GetFieldValue(0) > 0)
        else
          Result := not LResults.IsEmpty;
      except
        Result := False;
      end;
    finally
      LResults := nil;
      LStmt := nil;
    end;
  end;
end;

{$ENDREGION}


end.

