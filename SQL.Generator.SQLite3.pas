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
  SQL.Generator.Ansi, Mapping.Attributes, SQL.Interfaces, SQL.Commands, SQL.Types, Generics.Collections;

type
  TSQLiteSQLGenerator = class(TAnsiSQLGenerator)
  protected
    function DoGenerateCreateTable(const ATableName: string; AColumns: TObjectList<TSQLCreateField>): string; virtual;
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
  ,SysUtils
  ,StrUtils
  ;


{ TSQLiteSQLGenerator }

const
  TBL_TEMP = 'ORMTEMPTABLE';

function TSQLiteSQLGenerator.DoGenerateCreateTable(const ATableName: string; AColumns: TObjectList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create();
  try
    LSqlBuilder.AppendFormat('CREATE TABLE %0:S ', [ATableName])
      .Append('(')
      .AppendLine;
    for i := 0 to AColumns.Count - 1 do
    begin
      LField := AColumns[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      //0 - Column name, 1 - Column data type name, 2 - NOT NULL condition
      LSqlBuilder.AppendFormat('%0:S %1:S %2:S %3:S %4:S %5:S',
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
    LSqlBuilder.AppendFormat('DROP TABLE IF EXISTS %0:S; ', [TBL_TEMP]).AppendLine;
    //select old data to temporary table
    LSqlBuilder.AppendFormat('CREATE TEMPORARY TABLE %0:S AS SELECT * FROM %1:S; ',
      [TBL_TEMP, ACreateFKCommand.Table.Name]).AppendLine;
    //drop table
    LSqlBuilder.AppendFormat('DROP TABLE IF EXISTS %0:S; ', [ACreateFKCommand.Table.Name]).AppendLine;
    //recreate table with foreign keys
    LCreateTableString := DoGenerateCreateTable(ACreateFKCommand.Table.Name, ACreateFKCommand.Columns);
    LSqlBuilder.Append(LCreateTableString).Append(',').AppendLine;
    for i := 0 to ACreateFKCommand.ForeignKeys.Count - 1 do
    begin
      LField := ACreateFKCommand.ForeignKeys[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      LSqlBuilder.AppendFormat('CONSTRAINT %0:S FOREIGN KEY (%1:S) REFERENCES %2:S (%3:S)',
        [LField.ForeignKeyName, LField.Fieldname, LField.ReferencedTableName, LField.ReferencedColumnName]);

    end;

    LSqlBuilder.Append(');').AppendLine;

    //fill new table with old data
    LSqlBuilder.AppendFormat('INSERT INTO %0:S SELECT * FROM %1:S;',
      [ACreateFKCommand.Table.Name, TBL_TEMP]).AppendLine;

    //drop temporary table
    LSqlBuilder.AppendFormat('DROP TABLE IF EXISTS %0:S;', [TBL_TEMP]);

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
      //drop if exists
      LSqlBuilder.AppendFormat('DROP TABLE IF EXISTS %0:S; ', [TBL_TEMP]).AppendLine;

      LSqlBuilder.AppendFormat('CREATE TEMPORARY TABLE %0:S AS SELECT * FROM %1:S; ',
        [TBL_TEMP, ACreateTableCommand.Table.Name]).AppendLine;

      LSqlBuilder.AppendFormat('DROP TABLE %0:S; ', [ACreateTableCommand.Table.Name]).AppendLine;
    end;

    LCreateTableString := DoGenerateCreateTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns);
    LSqlBuilder.Append(LCreateTableString)
      .Append(');').AppendLine;

    if ACreateTableCommand.TableExists then
    begin
      LSqlBuilder.AppendFormat('INSERT INTO %0:S %1:S SELECT %1:S FROM %2:S; ',
        [ACreateTableCommand.Table.Name, GetCreateFieldsAsString(ACreateTableCommand.Columns),
        TBL_TEMP]).AppendLine;

      //drop temporary table
      LSqlBuilder.AppendFormat('DROP TABLE %0:S; ', [TBL_TEMP]);
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
  if Result = 'BIT' then
    Result := 'BLOB'
  else if ContainsStr(Result, 'CHAR') then
    Result := 'TEXT';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TSQLiteSQLGenerator.Create());

end.
