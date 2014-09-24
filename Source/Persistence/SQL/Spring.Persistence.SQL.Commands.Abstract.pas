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

unit Spring.Persistence.SQL.Commands.Abstract;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TAbstractCommandExecutor = class
  private
    FConnection: IDBConnection;
    FGenerator: ISQLGenerator;
    FClass: TClass;
    FSQL: string;
    FParams: IList<TDBParam>;
    procedure SetConnection(const Value: IDBConnection);
  protected
    function DoCreateParam(AColumn: ColumnAttribute; AValue: Variant): TDBParam; virtual;
    function CanUpdateParamFieldType(const AValue: Variant): Boolean; virtual;
    function CreateParam(AEntity: TObject; AColumn: ColumnAttribute): TDBParam; overload; virtual;
    function CreateParam(AEntity: TObject; AForeignColumn: ForeignJoinColumnAttribute): TDBParam; overload; virtual;
    function GetCommand: TDMLCommand; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function TableExists(const ATablename: string): Boolean; virtual;
    procedure FillDbTableColumns(const ATablename: string; AColumns: IList<string>); virtual;

    procedure Execute(AEntity: TObject); virtual; abstract;
    procedure Build(AClass: TClass); virtual; abstract;
    procedure BuildParams(AEntity: TObject); virtual;

    property Command: TDMLCommand read GetCommand;
    property Connection: IDBConnection read FConnection write SetConnection;
    property Generator: ISQLGenerator read FGenerator;
    property EntityClass: TClass read FClass write FClass;
    property SQLParameters: IList<TDBParam> read FParams;
    property SQL: string read FSQL write FSQL;
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

{ TAbstractCommandExecutor }

procedure TAbstractCommandExecutor.BuildParams(AEntity: TObject);
begin
  FParams.Clear;
  if Assigned(Command) then
    Command.Entity := AEntity;
end;

function TAbstractCommandExecutor.CanUpdateParamFieldType(const AValue: Variant): Boolean;
begin
  Result := (VarIsNull(AValue) or VarIsEmpty(AValue)) and (Connection.GetQueryLanguage = qlOracle);
end;

constructor TAbstractCommandExecutor.Create;
begin
  inherited Create;
  FGenerator := nil; //TSQLGeneratorRegister.GetCurrentGenerator;
  FParams := TCollections.CreateObjectList<TDBParam>;
end;

function TAbstractCommandExecutor.CreateParam(AEntity: TObject;
  AForeignColumn: ForeignJoinColumnAttribute): TDBParam;
var
  LVal, LRes: TValue;
  bFree: Boolean;
begin
  bFree := False;
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AForeignColumn.Name);
  LVal := TRttiExplorer.GetMemberValue(AEntity, AForeignColumn.ReferencedColumnName);
  //convert/serialize objects to stream
  if LVal.IsObject then
  begin
    if LVal.TryConvert(TypeInfo(TStream), LRes, bFree) then
    begin
      LVal := LRes.AsObject;
    end;
  end;
  Result.Value := TUtils.AsVariant(LVal);

  if CanUpdateParamFieldType(Result.Value) then
  begin
    Result.SetParamTypeFromTypeInfo
      ( TRttiExplorer.GetMemberTypeInfo(AEntity.ClassType, AForeignColumn.ReferencedColumnName)
      );
  end;

  if bFree then
  begin
    FreeValueObject(LVal);
  end;
end;

function TAbstractCommandExecutor.CreateParam(AEntity: TObject; AColumn: ColumnAttribute): TDBParam;
var
  LVal, LRes: TValue;
  bFree: Boolean;
begin
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AColumn.Name);
  LVal := TRttiExplorer.GetMemberValueDeep(AEntity, AColumn.ClassMemberName);
  //convert/serialize objects to stream. If value is nullable or lazy get it's real value
  if LVal.IsObject then
  begin
    if LVal.TryConvert(TypeInfo(TStream), LRes, bFree) then
    begin
      LVal := LRes.AsObject;
    end;
  end;

  Result.Value := TUtils.AsVariant(LVal);
  if CanUpdateParamFieldType(Result.Value) then
  begin
    Result.SetParamTypeFromTypeInfo(TRttiExplorer.GetMemberTypeInfo(AColumn.BaseEntityClass, AColumn.ClassMemberName));
  end;

  if bFree then
  begin
    FreeValueObject(LVal);
  end;
end;

destructor TAbstractCommandExecutor.Destroy;
begin
  FConnection := nil;
  FGenerator := nil;
  inherited Destroy;
end;

function TAbstractCommandExecutor.DoCreateParam(AColumn: ColumnAttribute; AValue: Variant): TDBParam;
begin
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AColumn.Name);
  Result.Value := AValue;
end;

procedure TAbstractCommandExecutor.FillDbTableColumns(const ATablename: string; AColumns: IList<string>);
var
  LSqlTableCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
  i: Integer;
begin
  LSqlTableCount := Generator.GetTableColumns(ATablename);
  if (LSqlTableCount <> '') then
  begin
    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(LSqlTableCount);
    LResults := LStmt.ExecuteQuery;
    AColumns.Clear;
    for i := 0 to LResults.GetFieldCount - 1 do
    begin
      AColumns.Add(LResults.GetFieldName(i));
    end;
  end;
end;

procedure TAbstractCommandExecutor.SetConnection(const Value: IDBConnection);
begin
  FConnection := Value;
  if Assigned(FConnection) then
    FGenerator := TSQLGeneratorRegister.GetGenerator(FConnection.GetQueryLanguage);
end;

function TAbstractCommandExecutor.TableExists(const ATablename: string): Boolean;
var
  LSqlTableCount, LSqlTableExists: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
begin
  Result := False;
  LSqlTableCount := '';
  LSqlTableExists := Generator.GetSQLTableExists(ATablename);
  if (LSqlTableExists = '') then
    LSqlTableCount := Generator.GetSQLTableCount(ATablename);
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

end.

