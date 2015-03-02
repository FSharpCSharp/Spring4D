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

unit Spring.Persistence.SQL.Generators.Ansi;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Abstract,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents base class responsible for generating Ansi SQL compatible
  ///   statements. To write your custom SQL generator it is recommended to
  ///   inherit your type from this class and override needed methods which
  ///   requires different treatment for current database type.
  /// </summary>
  TAnsiSQLGenerator = class(TAbstractSQLGenerator)
  protected
    function DoGenerateBackupTableUsingCreate(const tableName: string): TArray<string>;
  protected
    function DoGenerateBackupTable(const tableName: string): TArray<string>; virtual;
    function DoGenerateRestoreTable(const tableName: string;
      const createColumns: IList<TSQLCreateField>; const dbColumns: IList<string>): TArray<string>; virtual;
    function DoGenerateCreateTable(const tableName: string; const columns: IList<TSQLCreateField>): string; virtual;

    function GetGroupByAsString(const groupFields: IList<TSQLGroupByField>): string; virtual;
    function GetJoinAsString(const join: TSQLJoin): string; virtual;
    function GetJoinsAsString(const joinFields: IList<TSQLJoin>): string; virtual;
    function GetOrderAsString(const orderByFields: IList<TSQLOrderByField>): string; virtual;
    function GetWhereAsString(const whereFields: IList<TSQLWhereField>): string; virtual;
    function GetSelectFieldsAsString(const selectFields: IList<TSQLSelectField>): string; virtual;
    function GetCreateFieldsAsString(const createFields: IList<TSQLCreateField>): string; overload; virtual;
    function GetCreateFieldsAsString(const ACreateFields: IList<string>): string; overload; virtual;
    function GetCopyFieldsAsString(const createFields: IList<TSQLCreateField>; const ACopyFields: IList<string>): string; virtual;
    function GetTempTableName: string; virtual;
    function GetPrimaryKeyDefinition(const field: TSQLCreateField): string; virtual;
    function GetSplitStatementSymbol: string; virtual;
    procedure ParseFullTablename(const fullTableName: string; out tableName, schemaName: string); virtual;
    function GetEscapeFieldnameChar: Char; override;
    function GetUpdateVersionFieldQuery(const command: TUpdateCommand;
      const versionColumn: VersionAttribute; const version, primaryKey: Variant): Variant; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateSelect(const command: TSelectCommand): string; override;
    function GenerateInsert(const command: TInsertCommand): string; override;
    function GenerateUpdate(const command: TUpdateCommand): string; override;
    function GenerateDelete(const command: TDeleteCommand): string; override;
    function GenerateCreateTable(const command: TCreateTableCommand):  IList<string>; override;
    function GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>; override;

    /// <summary>
    ///   First drop sequence and then create it
    /// </summary>
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GenerateGetQueryCount(const sql: string): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableCount(const tableName: string): string; override;
    function GetSQLSequenceCount(const sequenceName: string): string; override;
    function GetTableColumns(const tableName: string): string; override;

    /// <summary>
    ///   Constructs a query which checks if given table exists in the
    ///   database. Query which executes returned statement should return 0 or
    ///   raise an exception if table does not exist and &gt; 0 when it exists.
    /// </summary>
    function GetSQLTableExists(const tableName: string): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection;


{$REGION 'TAnsiSQLGenerator'}

function TAnsiSQLGenerator.DoGenerateBackupTable(const tableName: string): TArray<string>;
begin
  //select old data to temporary table
  SetLength(Result, 2);
  Result[0] := Format('SELECT * INTO %0:S FROM %1:S',
        [GetTempTableName, tableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:S ', [tableName]);
end;

function TAnsiSQLGenerator.DoGenerateBackupTableUsingCreate(
  const tableName: string): TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := Format('CREATE TABLE %0:S AS SELECT * FROM %1:S',
        [GetTempTableName, tableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:S ', [tableName]);
end;

function TAnsiSQLGenerator.DoGenerateCreateTable(const tableName: string;
  const columns: IList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.AppendFormat('CREATE TABLE %0:S ', [tableName])
      .Append('(')
      .AppendLine;
    for i := 0 to columns.Count - 1 do
    begin
      LField := columns[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      //0 - Column name, 1 - Column data type name, 2 - NOT NULL condition
      LSqlBuilder.AppendFormat('%0:S %1:S %2:S %3:S',
        [
          LField.GetEscapedFieldname(GetEscapeFieldnameChar)
          ,GetSQLDataTypeName(LField)
          ,IfThen(cpPrimaryKey in LField.Properties, GetPrimaryKeyDefinition(LField))
          ,IfThen(cpNotNull in LField.Properties, 'NOT NULL', 'NULL')
        ]
      );
    end;

    LSqlBuilder.AppendLine.Append(')');

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.DoGenerateRestoreTable(const tableName: string;
  const createColumns: IList<TSQLCreateField>; const dbColumns: IList<string>): TArray<string>;
begin
  SetLength(Result, 2);

  Result[0] := Format('INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S' + sLineBreak,
    [tableName, GetTempTableName, GetCreateFieldsAsString(createColumns),
    GetCopyFieldsAsString(createColumns, dbColumns)]);

  //drop temporary table
  Result[1] := Format('DROP TABLE %0:S', [GetTempTableName]);
end;

function TAnsiSQLGenerator.GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLForeignKeyField;
begin
  Result := TCollections.CreateList<string>;
  LSqlBuilder := TStringBuilder.Create;
  try
    for i := 0 to command.ForeignKeys.Count - 1 do
    begin
      LField := command.ForeignKeys[i];
      LSqlBuilder.Clear;
      LSqlBuilder.AppendFormat('ALTER TABLE %0:S ', [command.Table.Name])
        .AppendLine
        .AppendFormat('ADD CONSTRAINT %0:S', [LField.ForeignKeyName])
        .AppendLine
        .AppendFormat('FOREIGN KEY(%0:S)', [LField.GetEscapedFieldname(GetEscapeFieldnameChar)])
        .AppendLine
        .AppendFormat(' REFERENCES %0:S (%1:S)', [LField.ReferencedTableName, LField.GetEscapedName(LField.ReferencedColumnName, GetEscapeFieldnameChar)])
        .AppendLine
        .Append(LField.GetConstraintsAsString);

      Result.Add(LSqlBuilder.ToString)
    end;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateCreateSequence(const command: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateCreateTable(const command: TCreateTableCommand): IList<string>;
var
  LResult: TArray<string>;
begin
  Assert(Assigned(command));
  Result := TCollections.CreateList<string>;

  if command.TableExists then
  begin
    LResult := DoGenerateBackupTable(command.Table.Name);
    Result.AddRange(LResult);
  end;

  Result.Add( DoGenerateCreateTable(command.Table.Name, command.Columns) );

  if command.TableExists then
  begin
    LResult := DoGenerateRestoreTable(command.Table.Name, command.Columns,
      command.DbColumns);
    Result.AddRange(LResult);
  end;
end;

function TAnsiSQLGenerator.GenerateDelete(const command: TDeleteCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LWhereField: TSQLWhereField;
  ix: Integer;
begin
  Assert(Assigned(command));

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('DELETE FROM ')
      .Append(command.Table.Name);

    ix := 0;

    for LWhereField in command.WhereFields do
    begin
      if ix = 0 then
        LSqlBuilder.AppendLine.Append(' WHERE ')
      else
        LSqlBuilder.Append(' AND ');

      {TODO -oLinas -cGeneral : implement where operators}

      LSqlBuilder.Append(Format('%0:S=%1:S', [LWhereField.GetEscapedFieldname(GetEscapeFieldnameChar), LWhereField.ParamName]));

      Inc(ix);
    end;

    LSqlBuilder.Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetQueryCount(const sql: string): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  try
    LSQL := sql;
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT COUNT(*) FROM (')
      .AppendLine
      .Append(LSQL)
      .AppendLine
      .Append(') AS ORM_GET_QUERY_COUNT').Append(GetSplitStatementSymbol)
      ;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateInsert(const command: TInsertCommand): string;
var
  i: Integer;
  sFields, sParams: string;
begin
  Assert(Assigned(command));
  Assert(command.InsertFields.Any);

  Result := 'INSERT INTO ';

  sFields := '';
  sParams := '';

  for i := 0 to command.InsertFields.Count - 1 do
  begin
    if i > 0 then
    begin
      sFields := sFields + ',';
      sParams := sParams + ',';
    end;

    sFields := sFields + command.InsertFields[i].GetEscapedFieldname(GetEscapeFieldnameChar);
    sParams := sParams + command.InsertFields[i].ParamName;
  end;

  Result := Result + command.Table.Name + ' (' + CRLF + '  ' + sFields + ')' + CRLF +
    '  VALUES (' + CRLF + sParams + ')' + GetSplitStatementSymbol;
end;

function TAnsiSQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  LSQL: string;
begin
  LSQL := sql;
  if EndsStr(';', LSQL) then
    SetLength(LSQL, Length(LSQL)-1);

  Result := LSQL + Format(' LIMIT %1:D,%0:D %2:S', [limit, offset, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GenerateSelect(const command: TSelectCommand): string;
var
  LSqlBuilder: TStringBuilder;
begin
  Assert(Assigned(command));
  Assert(command.SelectFields.Any);
  Assert(command.Table.Alias <> '');

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('SELECT ')
      .Append(GetSelectFieldsAsString(command.SelectFields)).AppendLine
      .Append(' FROM ').Append(command.Table.GetFullTableName)
      .Append(GetJoinsAsString(command.Joins))
      .Append(GetWhereAsString(command.WhereFields))
      .Append(GetGroupByAsString(command.GroupByFields))
      .Append(GetOrderAsString(command.OrderByFields))
      .Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateUpdate(const command: TUpdateCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LField: TSQLUpdateField;
  LWhereField: TSQLWhereField;
  ix: Integer;
begin
  Assert(Assigned(command));

  if not command.UpdateFields.Any then
    Exit('');

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('UPDATE ')
      .Append(command.Table.Name)
      .Append(' SET ').AppendLine;

    ix := 0;

    for LField in command.UpdateFields do
    begin
      if ix > 0 then
        LSqlBuilder.Append(',');

      LSqlBuilder.Append(Format('%0:S=%1:S', [LField.GetEscapedFieldname(GetEscapeFieldnameChar), LField.ParamName]));
      Inc(ix);
    end;

    ix := 0;

    for LWhereField in command.WhereFields do
    begin
      if ix = 0 then
        LSqlBuilder.AppendLine.Append(' WHERE ')
      else
        LSqlBuilder.Append(' AND ');

      LSqlBuilder.Append(Format('%0:S=%1:S', [LWhereField.GetEscapedFieldname(GetEscapeFieldnameChar), LWhereField.ParamName]));

      Inc(ix);
    end;

    LSqlBuilder.Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(
  const createFields: IList<TSQLCreateField>): string;
var
  i: Integer;
  LField: TSQLCreateField;
begin
  Result := '';
  i := 0;
  for LField in createFields do
  begin
    if i > 0 then
      Result := Result + ',';

    Result := Result + LField.GetEscapedFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetCopyFieldsAsString(const createFields: IList<TSQLCreateField>;
  const ACopyFields: IList<string>): string;
var
  LField: TSQLCreateField;
  i: Integer;
begin
  Result := '';
  i := 0;
  for LField in createFields do
  begin
    if i > 0 then
      Result := Result + ',';

    if ACopyFields.Contains(LField.Fieldname) then
      Result := Result + LField.GetEscapedFieldname(GetEscapeFieldnameChar)
    else
      Result := Result + 'NULL';

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(const ACreateFields: IList<string>): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ACreateFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ',';

    Result := Result + ACreateFields[i];
  end;
end;

function TAnsiSQLGenerator.GetEscapeFieldnameChar: Char;
begin
  Result := '"';
end;

function TAnsiSQLGenerator.GetGroupByAsString(
  const groupFields: IList<TSQLGroupByField>): string;
var
  LField: TSQLGroupByField;
  i: Integer;
begin
  Result := '';
  i := 0;

  for LField in groupFields do
  begin
    if i > 0 then
      Result := Result + ','
    else
      Result := CRLF + '  GROUP BY ';

    Result := Result + LField.GetFullFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetJoinAsString(const join: TSQLJoin): string;
var
  LSegment: TSQLJoinSegment;
  i: Integer;
begin
  Assert(join.Segments.Any);
  Result := ' ' + TSQLJoin.GetJoinTypeAsString(join.JoinType) + ' ';
  for i := 0 to join.Segments.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ' AND ';

    LSegment := join.Segments[i];

    Result := Result +
      LSegment.PKField.Table.GetFullTableName + ' ON '  +
      LSegment.PKField.GetFullFieldname(GetEscapeFieldnameChar) + '=' + LSegment.FKField.GetFullFieldname(GetEscapeFieldnameChar);
  end;
end;

function TAnsiSQLGenerator.GetJoinsAsString(const joinFields: IList<TSQLJoin>): string;
var
  LField: TSQLJoin;
begin
  Result := '';
  for LField in joinFields do
    Result := Result + CRLF + ' ' + GetJoinAsString(LField);
end;

function TAnsiSQLGenerator.GetOrderAsString(
  const orderByFields: IList<TSQLOrderByField>): string;
var
  i: Integer;
  LField: TSQLOrderByField;
begin
  Result := '';
  i := 0;
  for LField in orderByFields do
  begin
    if i > 0 then
      Result := Result + ','
    else
      Result := CRLF + '  ORDER BY ';


    Result := Result + LField.GetFullOrderByFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetPrimaryKeyDefinition(const field: TSQLCreateField): string;
begin
  Result := 'PRIMARY KEY';
end;

function TAnsiSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlAnsiSQL;
end;

procedure TAnsiSQLGenerator.ParseFullTablename(const fullTableName: string; out tableName, schemaName: string);
var
  LPos: Integer;
begin
  LPos := PosEx('.', fullTableName);
  tableName := fullTableName;
  schemaName := '';
  if LPos > 1 then
  begin
    schemaName := Copy(fullTableName, 1, LPos - 1);
    tableName := Copy(fullTableName, LPos + 1, Length(fullTableName) - 1);
  end;
end;

function TAnsiSQLGenerator.GetSelectFieldsAsString(
  const selectFields: IList<TSQLSelectField>): string;
var
  i: Integer;
  LField: TSQLSelectField;
begin
  Result := '';
  i := 0;

  for LField in selectFields do
  begin
    if i > 0 then
      Result := Result + ',';

    Result := Result + LField.GetFullFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetSplitStatementSymbol: string;
begin
  Result := ';';
end;

function TAnsiSQLGenerator.GetSQLDataTypeName(const field: TSQLCreateField): string;
var
  LDelphiTypeInfo: PTypeInfo;
  LClonedField: TSQLCreateField;
begin
  Assert(field <> nil);
  Result := 'INTEGER';

  LDelphiTypeInfo := field.TypeKindInfo;

  case field.TypeKindInfo.Kind of
    tkUnknown: ;
    tkInteger, tkInt64, tkEnumeration, tkSet:
    begin
      if field.Precision > 0 then
        Result := Format('NUMERIC(%0:D, %1:D)', [field.Precision, field.Scale])
      else
      begin
        Result := 'INTEGER';
        if (System.TypeInfo(Boolean) = LDelphiTypeInfo) then
          Result := 'BIT';
      end;
    end;

    tkChar: Result := Format('CHAR(%D)', [field.Length]);
    tkFloat:
    begin
      if (System.TypeInfo(TDate) = LDelphiTypeInfo) then
        Result := 'DATE'
      else if (System.TypeInfo(TDateTime) = LDelphiTypeInfo) then
        Result := 'TIMESTAMP'
      else if (System.TypeInfo(TTime) = LDelphiTypeInfo) then
        Result := 'TIME'
      else
        if field.Precision > 0 then
          Result := Format('NUMERIC(%0:D, %1:D)', [field.Precision, field.Scale])
        else
          Result := 'FLOAT';
    end;
    tkString, tkLString: Result := Format('VARCHAR(%D)', [field.Length]);
    tkClass, tkArray, tkDynArray, tkVariant: Result := 'BLOB';
    tkMethod: ;
    tkWChar: Result := Format('NCHAR(%D)', [field.Length]);
    tkWString, tkUString: Result := Format('NVARCHAR(%D)', [field.Length]);
    tkRecord:
    begin
      if IsNullable(LDelphiTypeInfo) or TType.IsLazyType(LDelphiTypeInfo) then
      begin
        LClonedField := field.Clone;
        try
          LClonedField.TypeKindInfo := TRttiExplorer.GetLastGenericArgumentType(LDelphiTypeInfo).Handle;
          Result := GetSQLDataTypeName(LClonedField);
        finally
          LClonedField.Free;
        end;
      end;
    end;
    tkInterface: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
  end;
end;

function TAnsiSQLGenerator.GetSQLSequenceCount(const sequenceName: string): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GetSQLTableCount(const tableName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM %0:S %1:S', [tableName, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := ''; //override to implement specific functionality
end;

function TAnsiSQLGenerator.GetTableColumns(const tableName: string): string;
begin
  Result := Format('SELECT * FROM %0:S WHERE 1<>2 %1:S', [tableName, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetTempTableName: string;
begin
  Result := TBL_TEMP;
end;

function TAnsiSQLGenerator.GetUpdateVersionFieldQuery(
  const command: TUpdateCommand; const versionColumn: VersionAttribute;
  const version, primaryKey: Variant): Variant;
var
  LSQL: string;
begin
  LSQL := Format('UPDATE %0:S SET %1:S = coalesce(%1:S,0) + 1 WHERE (%2:S = %3:S) AND (coalesce(%1:S,0) = %4:S)',
    [command.Table.Name, versionColumn.ColumnName,
    command.PrimaryKeyColumn.ColumnName, VarToStr(primaryKey), VarToStr(version)]);
  Result := LSQL;
end;

function TAnsiSQLGenerator.GetWhereAsString(const whereFields: IList<TSQLWhereField>): string;
var
  i, ix: Integer;
  LField: TSQLWhereField;
begin
  Result := '';
  ix := 0;
  for i:=0 to whereFields.Count - 1 do
  begin
    if i < ix then
      Continue;

    ix := i;

    LField := whereFields[i];
    if i > 0 then
      Result := Result + ' AND '
    else
      Result := CRLF + '  WHERE ';

    if LField.WhereOperator in StartOperators then
    begin
      ix := FindEnd(whereFields, i, LField.WhereOperator, GetEndOperator(LField.WhereOperator));
    end;

    Result := Result + LField.ToSQLString(GetEscapeFieldnameChar);
    Inc(ix);
  end;
end;

{$ENDREGION}


end.
