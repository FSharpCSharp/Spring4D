(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.Generator.SQLite3;

interface

uses
  SQL.Generator.Ansi, Mapping.Attributes, SQL.Interfaces, SQL.Commands, SQL.Types, Generics.Collections, SysUtils;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>SQLite3</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TSQLiteSQLGenerator = class(TAnsiSQLGenerator)
  protected
    procedure DoGenerateBackupTable(const ATableName: string; var ASqlBuilder: TStringBuilder); override;
    procedure DoGenerateRestoreTable(const ATablename: string;
      ACreateColumns: TObjectList<TSQLCreateField>; ADbColumns: TList<string>; var ASqlBuilder: TStringBuilder); override;
    function DoGenerateCreateTable(const ATableName: string; AColumns: TObjectList<TSQLCreateField>): string; override;
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string; override;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
  end;

implementation

uses
  SQL.Register
  ,StrUtils
  ;


{ TSQLiteSQLGenerator }

const
  TBL_TEMP = 'ORMTEMPTABLE';

procedure TSQLiteSQLGenerator.DoGenerateBackupTable(const ATableName: string; var ASqlBuilder: TStringBuilder);
begin
  ASqlBuilder.AppendFormat(' DROP TABLE IF EXISTS %0:S; ', [TBL_TEMP]).AppendLine;
  //select old data to temporary table
  ASqlBuilder.AppendFormat(' CREATE TEMPORARY TABLE %0:S AS SELECT * FROM %1:S; ',
    [TBL_TEMP, ATableName]).AppendLine;
  //drop table
  ASqlBuilder.AppendFormat(' DROP TABLE IF EXISTS %0:S; ', [ATableName]).AppendLine;
end;

function TSQLiteSQLGenerator.DoGenerateCreateTable(const ATableName: string; AColumns: TObjectList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create();
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
    //LSqlBuilder.Append(');');

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

procedure TSQLiteSQLGenerator.DoGenerateRestoreTable(const ATablename: string;
  ACreateColumns: TObjectList<TSQLCreateField>; ADbColumns: TList<string>; var ASqlBuilder: TStringBuilder);
begin
  ASqlBuilder.AppendFormat(' INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S;',
    [ATablename, TBL_TEMP, GetCreateFieldsAsString(ACreateColumns),
    GetCopyFieldsAsString(ACreateColumns, ADbColumns)]).AppendLine;

  //drop temporary table
  ASqlBuilder.AppendFormat(' DROP TABLE IF EXISTS %0:S;', [TBL_TEMP]);
end;

function TSQLiteSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LCreateTableString: string;
  i: Integer;
  LField: TSQLForeignKeyField;
begin
  Assert(Assigned(ACreateFKCommand));
  if ACreateFKCommand.ForeignKeys.Count < 1 then
    Exit('');

  LSqlBuilder := TStringBuilder.Create;
  try
    DoGenerateBackupTable(ACreateFKCommand.Table.Name, LSqlBuilder);
    //recreate table with foreign keys
    LCreateTableString := DoGenerateCreateTable(ACreateFKCommand.Table.Name, ACreateFKCommand.Columns);
    LSqlBuilder.Append(LCreateTableString).Append(',').AppendLine;
    for i := 0 to ACreateFKCommand.ForeignKeys.Count - 1 do
    begin
      LField := ACreateFKCommand.ForeignKeys[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      LSqlBuilder.AppendFormat(' CONSTRAINT %0:S FOREIGN KEY (%1:S) REFERENCES %2:S (%3:S)',
        [LField.ForeignKeyName, LField.Fieldname, LField.ReferencedTableName, LField.ReferencedColumnName]);

    end;
    LSqlBuilder.Append(');').AppendLine;

    DoGenerateRestoreTable(ACreateFKCommand.Table.Name, ACreateFKCommand.Columns, ACreateFKCommand.DbColumns, LSqlBuilder);

    Result := LSqlBuilder.ToString;

  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LCreateTableString: string;
begin
  Assert(Assigned(ACreateTableCommand));

  LSqlBuilder := TStringBuilder.Create;
  try
    if ACreateTableCommand.TableExists then
    begin
      DoGenerateBackupTable(ACreateTableCommand.Table.Name, LSqlBuilder);
    end;

    LCreateTableString := DoGenerateCreateTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns);
    LSqlBuilder.Append(LCreateTableString)
      .Append(');').AppendLine;

    if ACreateTableCommand.TableExists then
    begin
      DoGenerateRestoreTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns, ACreateTableCommand.DbColumns, LSqlBuilder);
    end;

    Result := LSqlBuilder.ToString;

  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT last_insert_rowid();';
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

initialization
  TSQLGeneratorRegister.RegisterGenerator(TSQLiteSQLGenerator.Create());

end.
