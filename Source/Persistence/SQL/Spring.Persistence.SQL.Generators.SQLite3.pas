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

unit Spring.Persistence.SQL.Generators.SQLite3;

{$I Spring.inc}

interface

uses
  Generics.Collections,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>SQLite3</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TSQLiteSQLGenerator = class(TAnsiSQLGenerator)
  protected
    function DoGenerateBackupTable(const ATableName: string): TArray<string>; override;
    function DoGenerateRestoreTable(const ATablename: string;
      ACreateColumns: IList<TSQLCreateField>; ADbColumns: IList<string>): TArray<string>; override;
    function DoGenerateCreateTable(const ATableName: string; AColumns: IList<TSQLCreateField>): string; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetSQLTableExists(const ATablename: string): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;

{ TSQLiteSQLGenerator }

const
  TBL_TEMP = 'ORMTEMPTABLE';

function TSQLiteSQLGenerator.DoGenerateBackupTable(const ATableName: string): TArray<string>;
begin
  SetLength(Result, 3);
  Result[0] := Format(' DROP TABLE IF EXISTS %0:S ', [TBL_TEMP]);
  //select old data to temporary table
  Result[1] := Format(' CREATE TEMPORARY TABLE %0:S AS SELECT * FROM %1:S ',
    [TBL_TEMP, ATableName]);
  //drop table
  Result[2] := Format(' DROP TABLE IF EXISTS %0:S ', [ATableName]);
end;

function TSQLiteSQLGenerator.DoGenerateCreateTable(const ATableName: string; AColumns: IList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.AppendFormat(' CREATE TABLE %0:S ', [ATableName])
      .Append('(')
      .AppendLine;
    for i := 0 to AColumns.Count - 1 do
    begin
      LField := AColumns[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      //0 - Column name, 1 - Column data type name, 2 - NOT NULL condition
      LSqlBuilder.AppendFormat(' %0:S %1:S %2:S %3:S %4:S %5:S',
        [
          LField.Fieldname
          ,GetSQLDataTypeName(LField)
          ,IfThen(cpPrimaryKey in LField.Properties, 'PRIMARY KEY')
          ,IfThen(LField.IsIdentity, 'AUTOINCREMENT')
          ,IfThen(cpUnique in LField.Properties, 'UNIQUE')
          ,IfThen(cpNotNull in LField.Properties, 'NOT NULL', 'NULL')
        ]
      );
    end;
    LSqlBuilder.Append(')');

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.DoGenerateRestoreTable(const ATablename: string;
  ACreateColumns: IList<TSQLCreateField>; ADbColumns: IList<string>): TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := Format(' INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S',
    [ATablename, TBL_TEMP, GetCreateFieldsAsString(ACreateColumns),
    GetCopyFieldsAsString(ACreateColumns, ADbColumns)]);

  //drop temporary table
  Result[1] := Format(' DROP TABLE IF EXISTS %0:S', [TBL_TEMP]);
end;

function TSQLiteSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>;
var
  LSqlBuilder: TStringBuilder;
  LCreateTableString: string;
  i: Integer;
  LField: TSQLForeignKeyField;
  LRes: TArray<string>;
begin
  Assert(Assigned(ACreateFKCommand));
  Result := TCollections.CreateList<string>;

  if ACreateFKCommand.ForeignKeys.Count < 1 then
    Exit;

  LSqlBuilder := TStringBuilder.Create;
  try
    LRes := DoGenerateBackupTable(ACreateFKCommand.Table.Name);
    Result.AddRange(LRes);
    //recreate table with foreign keys
    LCreateTableString := DoGenerateCreateTable(ACreateFKCommand.Table.Name, ACreateFKCommand.Columns);
    //remove ")" from the end of the string
    SetLength(LCreateTableString, Length(LCreateTableString)-1);

    LSqlBuilder.Append(LCreateTableString).Append(',').AppendLine;
    for i := 0 to ACreateFKCommand.ForeignKeys.Count - 1 do
    begin
      LField := ACreateFKCommand.ForeignKeys[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      LSqlBuilder.AppendFormat(' CONSTRAINT %0:S FOREIGN KEY (%1:S) REFERENCES %2:S (%3:S)',
        [LField.ForeignKeyName, LField.Fieldname, LField.ReferencedTableName, LField.ReferencedColumnName]);

    end;
    LSqlBuilder.Append(');');

    Result.Add(LSqlBuilder.ToString);

    LRes := DoGenerateRestoreTable(ACreateFKCommand.Table.Name, ACreateFKCommand.Columns, ACreateFKCommand.DbColumns);
    Result.AddRange(LRes);
  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT last_insert_rowid;';
end;

function TSQLiteSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlSQLite;
end;

function TSQLiteSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if ContainsStr(Result, 'CHAR') then
    Result := 'TEXT';
end;

function TSQLiteSQLGenerator.GetSQLTableExists(const ATablename: string): string;
begin
  Result := 'select count(*) from sqlite_master where [type] = ''table'' and lower([name]) = ''' +
    LowerCase(ATablename) + ''' ';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TSQLiteSQLGenerator.Create);

end.
