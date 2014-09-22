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

unit Spring.Persistence.SQL.Generators.Ansi;

{$I Spring.inc}

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Abstract,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents base class responsible for generating Ansi SQL compatible
  ///	  statements. To write your custom SQL generator it is recommended to 
  ///	  inherit your type from this class and override needed methods which 
  ///	  requires different treatment for current database type.
  ///	</summary>
  {$ENDREGION}
  TAnsiSQLGenerator = class(TAbstractSQLGenerator)
  protected
    function DoGenerateBackupTable(const ATableName: string): TArray<string>; virtual;
    function DoGenerateRestoreTable(const ATablename: string;
      ACreateColumns: IList<TSQLCreateField>; ADbColumns: IList<string>): TArray<string>; virtual;
    function DoGenerateCreateTable(const ATableName: string; AColumns: IList<TSQLCreateField>): string; virtual;

    function GetGroupByAsString(const AGroupFields: IList<TSQLGroupByField>): string; virtual;
    function GetJoinAsString(const AJoin: TSQLJoin): string; virtual;
    function GetJoinsAsString(const AJoinFields: IList<TSQLJoin>): string; virtual;
    function GetOrderAsString(const AOrderFields: IList<TSQLOrderField>): string; virtual;
    function GetWhereAsString(const AWhereFields: IList<TSQLWhereField>): string; virtual;
    function GetSelectFieldsAsString(const ASelectFields: IList<TSQLSelectField>): string; virtual;
    function GetCreateFieldsAsString(const ACreateFields: IList<TSQLCreateField>): string; overload; virtual;
    function GetCreateFieldsAsString(const ACreateFields: IList<string>): string; overload; virtual;
    function GetCopyFieldsAsString(const ACreateFields: IList<TSQLCreateField>; const ACopyFields: IList<string>): string; virtual;
    function GetTempTableName: string; virtual;
    function GetPrimaryKeyDefinition(AField: TSQLCreateField): string; virtual;
    function GetSplitStatementSymbol: string; virtual;
    procedure ParseFullTablename(const AFullTablename: string; out ATablename, ASchemaName: string); virtual;
    function GetEscapeFieldnameChar: Char; override;
    function GetUpdateVersionFieldQuery(AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute; AVersionValue, APKValue: Variant): Variant; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateSelect(ASelectCommand: TSelectCommand): string; override;
    function GenerateInsert(AInsertCommand: TInsertCommand): string; override;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string; override;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string; override;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand):  IList<string>; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>; override;
    /// <summary>
    /// First drop sequence and then create it
    /// </summary>
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GenerateGetQueryCount(const ASql: string): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetSQLTableCount(const ATablename: string): string; override;
    function GetSQLSequenceCount(const ASequenceName: string): string; override;
    function GetTableColumns(const ATableName: string): string; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Constructs a query which checks if given table exists in the
    ///	  database. Query which executes returned statement should return 0 or
    ///	  raise an exception if table does not exist and &gt; 0 when it exists.
    ///	</summary>
    {$ENDREGION}
    function GetSQLTableExists(const ATablename: string): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;

{ TAnsiSQLGenerator }

function TAnsiSQLGenerator.DoGenerateBackupTable(const ATableName: string): TArray<string>;
begin
  //select old data to temporary table
  SetLength(Result, 2);
  Result[0] := Format('SELECT * INTO %0:S FROM %1:S',
        [GetTempTableName, ATableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:S ', [ATableName]);
end;

function TAnsiSQLGenerator.DoGenerateCreateTable(const ATableName: string;
  AColumns: IList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create;
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

function TAnsiSQLGenerator.DoGenerateRestoreTable(const ATablename: string;
  ACreateColumns: IList<TSQLCreateField>; ADbColumns: IList<string>): TArray<string>;
begin
  SetLength(Result, 2);

  Result[0] := Format('INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S' + #13#10,
    [ATablename, GetTempTableName, GetCreateFieldsAsString(ACreateColumns),
    GetCopyFieldsAsString(ACreateColumns, ADbColumns)]);

  //drop temporary table
  Result[1] := Format('DROP TABLE %0:S', [GetTempTableName]);
end;

function TAnsiSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLForeignKeyField;
begin
  Result := TCollections.CreateList<string>;
  LSqlBuilder := TStringBuilder.Create;
  try
    for i := 0 to ACreateFKCommand.ForeignKeys.Count - 1 do
    begin
      LField := ACreateFKCommand.ForeignKeys[i];
      LSqlBuilder.Clear;
      LSqlBuilder.AppendFormat('ALTER TABLE %0:S ', [ACreateFKCommand.Table.Name])
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

function TAnsiSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): IList<string>;
var
  LResult: TArray<string>;
begin
  Assert(Assigned(ACreateTableCommand));
  Result := TCollections.CreateList<string>;

  if ACreateTableCommand.TableExists then
  begin
    LResult := DoGenerateBackupTable(ACreateTableCommand.Table.Name);
    Result.AddRange(LResult);
  end;

  Result.Add( DoGenerateCreateTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns) );

  if ACreateTableCommand.TableExists then
  begin
    LResult := DoGenerateRestoreTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns,
      ACreateTableCommand.DbColumns);
    Result.AddRange(LResult);
  end;
end;

function TAnsiSQLGenerator.GenerateDelete(ADeleteCommand: TDeleteCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LWhereField: TSQLWhereField;
  ix: Integer;
begin
  Assert(Assigned(ADeleteCommand));

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('DELETE FROM ')
      .Append(ADeleteCommand.Table.Name);

    ix := 0;

    for LWhereField in ADeleteCommand.WhereFields do
    begin
      if ix = 0 then
        LSqlBuilder.AppendLine.Append(' WHERE ')
      else
        LSqlBuilder.Append(' AND ');

      {TODO -oLinas -cGeneral : implement where operators}

      LSqlBuilder.Append(Format('%0:S=%1:S', [LWhereField.GetEscapedFieldname(GetEscapeFieldnameChar), ADeleteCommand.GetAndIncParameterName(LWhereField.Fieldname)]));

      Inc(ix);
    end;

    LSqlBuilder.Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetQueryCount(const ASql: string): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  try
    LSQL := ASql;
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

function TAnsiSQLGenerator.GenerateInsert(AInsertCommand: TInsertCommand): string;
var
  i: Integer;
  sFields, sParams: string;
begin
  Assert(Assigned(AInsertCommand));
  Assert(AInsertCommand.InsertFields.Count > 0);

  Result := 'INSERT INTO ';

  sFields := '';
  sParams := '';

  for i := 0 to AInsertCommand.InsertFields.Count - 1 do
  begin
    if i > 0 then
    begin
      sFields := sFields + ',';
      sParams := sParams + ',';
    end;

    sFields := sFields + AInsertCommand.InsertFields[i].GetEscapedFieldname(GetEscapeFieldnameChar);
    sParams := sParams + AInsertCommand.GetAndIncParameterName(AInsertCommand.InsertFields[i].Fieldname);//  ':' + AInsertCommand.InsertFields[i].Fieldname;
  end;

  Result := Result + AInsertCommand.Table.Name + ' (' + CRLF + '  ' + sFields + ')' + CRLF +
    '  VALUES (' + CRLF + sParams + ')' + GetSplitStatementSymbol;
end;

function TAnsiSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit,
  AOffset: Integer): string;
var
  LSQL: string;
begin
  LSQL := ASql;
  if EndsStr(';', LSQL) then
    SetLength(LSQL, Length(LSQL)-1);

  Result := LSQL + Format(' LIMIT %1:D,%0:D %2:S', [ALimit, AOffset, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GenerateSelect(ASelectCommand: TSelectCommand): string;
var
  LSqlBuilder: TStringBuilder;
begin
  Assert(Assigned(ASelectCommand));
  Assert(ASelectCommand.SelectFields.Count > 0);
  Assert(ASelectCommand.Table.Alias <> '');

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('SELECT ')
      .Append(GetSelectFieldsAsString(ASelectCommand.SelectFields)).AppendLine
      .Append(' FROM ').Append(ASelectCommand.Table.GetFullTableName)
      .Append(GetJoinsAsString(ASelectCommand.Joins))
      .Append(GetWhereAsString(ASelectCommand.WhereFields))
      .Append(GetGroupByAsString(ASelectCommand.GroupByFields))
      .Append(GetOrderAsString(ASelectCommand.OrderByFields))
      .Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateUpdate(AUpdateCommand: TUpdateCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LField: TSQLField;
  LWhereField: TSQLWhereField;
  ix: Integer;
begin
  Assert(Assigned(AUpdateCommand));

  if (AUpdateCommand.UpdateFields.Count < 1) then
    Exit('');

  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.Append('UPDATE ')
      .Append(AUpdateCommand.Table.Name)
      .Append(' SET ').AppendLine;

    ix := 0;

    for LField in AUpdateCommand.UpdateFields do
    begin
      if ix > 0 then
        LSqlBuilder.Append(',');

      LSqlBuilder.Append(Format('%0:S=%1:S', [LField.GetEscapedFieldname(GetEscapeFieldnameChar), AUpdateCommand.GetAndIncParameterName(LField.Fieldname)]));
      Inc(ix);
    end;

    ix := 0;

    for LWhereField in AUpdateCommand.WhereFields do
    begin
      if ix = 0 then
        LSqlBuilder.AppendLine.Append(' WHERE ')
      else
        LSqlBuilder.Append(' AND ');

      LSqlBuilder.Append(Format('%0:S=%1:S', [LWhereField.GetEscapedFieldname(GetEscapeFieldnameChar), AUpdateCommand.GetAndIncParameterName(LWhereField.Fieldname)]));

      Inc(ix);
    end;

    LSqlBuilder.Append(GetSplitStatementSymbol);

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(
  const ACreateFields: IList<TSQLCreateField>): string;
var
  i: Integer;
  LField: TSQLCreateField;
begin
  Result := '';
  i := 0;
  for LField in ACreateFields do
  begin
    if i > 0 then
      Result := Result + ',';

    Result := Result + LField.GetEscapedFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetCopyFieldsAsString(const ACreateFields: IList<TSQLCreateField>;
  const ACopyFields: IList<string>): string;
var
  LField: TSQLCreateField;
  i: Integer;
begin
  Result := '';
  i := 0;
  for LField in ACreateFields do
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
  const AGroupFields: IList<TSQLGroupByField>): string;
var
  LField: TSQLGroupByField;
  i: Integer;
begin
  Result := '';
  i := 0;

  for LField in AGroupFields do
  begin
    if i > 0 then
      Result := Result + ','
    else
      Result := CRLF + '  GROUP BY ';

    Result := Result + LField.GetFullFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetJoinAsString(const AJoin: TSQLJoin): string;
var
  LSegment: TSQLJoinSegment;
  i: Integer;
begin
  Assert(AJoin.Segments.Count > 0);
  Result := ' ' + TSQLJoin.GetJoinTypeAsString(AJoin.JoinType) + ' ';
  for i := 0 to AJoin.Segments.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ' AND ';

    LSegment := AJoin.Segments[i];

    Result := Result +
      LSegment.PKField.Table.GetFullTableName + ' ON '  +
      LSegment.PKField.GetFullFieldname(GetEscapeFieldnameChar) + '=' + LSegment.FKField.GetFullFieldname(GetEscapeFieldnameChar);
  end;
end;

function TAnsiSQLGenerator.GetJoinsAsString(const AJoinFields: IList<TSQLJoin>): string;
var
  LField: TSQLJoin;
begin
  Result := '';
  for LField in AJoinFields do
  begin
    Result := Result + CRLF + ' ' + GetJoinAsString(LField);
  end;
end;

function TAnsiSQLGenerator.GetOrderAsString(
  const AOrderFields: IList<TSQLOrderField>): string;
var
  i: Integer;
  LField: TSQLOrderField;
begin
  Result := '';
  i := 0;
  for LField in AOrderFields do
  begin
    if i > 0 then
      Result := Result + ','
    else
      Result := CRLF + '  ORDER BY ';


    Result := Result + LField.GetFullOrderByFieldname(GetEscapeFieldnameChar);

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetPrimaryKeyDefinition(AField: TSQLCreateField): string;
begin
  Result := 'PRIMARY KEY';
end;

function TAnsiSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlAnsiSQL;
end;

procedure TAnsiSQLGenerator.ParseFullTablename(const AFullTablename: string; out ATablename, ASchemaName: string);
var
  LPos: Integer;
begin
  LPos := PosEx('.', AFullTablename);
  ATablename := AFullTablename;
  ASchemaName := '';
  if LPos > 1 then
  begin
    ASchemaName := Copy(AFullTablename, 1, LPos - 1);
    ATablename := Copy(AFullTablename, LPos + 1, Length(AFullTablename) - 1);
  end;
end;

function TAnsiSQLGenerator.GetSelectFieldsAsString(
  const ASelectFields: IList<TSQLSelectField>): string;
var
  i: Integer;
  LField: TSQLSelectField;
begin
  Result := '';
  i := 0;

  for LField in ASelectFields do
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

function TAnsiSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
var
  LDelphiTypeInfo: PTypeInfo;
  LClonedField: TSQLCreateField;
begin
  Assert(AField <> nil);
  Result := 'INTEGER';

  LDelphiTypeInfo := AField.TypeKindInfo;

  case AField.TypeKindInfo.Kind of
    tkUnknown: ;
    tkInteger, tkInt64, tkEnumeration, tkSet:
    begin
      if (AField.Precision > 0) then
      begin
        Result := Format('NUMERIC(%0:D, %1:D)', [AField.Precision, AField.Scale]);
      end
      else
      begin
        Result := 'INTEGER';
        if (System.TypeInfo(Boolean) = LDelphiTypeInfo) then
          Result := 'BIT';
      end;
    end;

    tkChar: Result := Format('CHAR(%D)', [AField.Length]);
    tkFloat:
    begin
      if (System.TypeInfo(TDate) = LDelphiTypeInfo) then
        Result := 'DATE'
      else if (System.TypeInfo(TDateTime) = LDelphiTypeInfo) then
        Result := 'TIMESTAMP'
      else if (System.TypeInfo(TTime) = LDelphiTypeInfo) then
        Result := 'TIME'
      else
      begin
        if AField.Precision > 0 then
        begin
          Result := Format('NUMERIC(%0:D, %1:D)', [AField.Precision, AField.Scale]);
        end
        else
          Result := 'FLOAT';
      end;
    end;
    tkString, tkLString: Result := Format('VARCHAR(%D)', [AField.Length]);
    tkClass, tkArray, tkDynArray, tkVariant: Result := 'BLOB';
    tkMethod: ;
    tkWChar: Result := Format('NCHAR(%D)', [AField.Length]);
    tkWString, tkUString: Result := Format('NVARCHAR(%D)', [AField.Length]);
    tkRecord:
    begin
      if TUtils.IsNullableType(LDelphiTypeInfo) or TUtils.IsLazyType(LDelphiTypeInfo) then
      begin
        LClonedField := AField.Clone;
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

function TAnsiSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GetSQLTableCount(const ATablename: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM %0:S %1:S', [ATablename, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetSQLTableExists(const ATablename: string): string;
begin
  Result := ''; //override to implement specific functionality
end;

function TAnsiSQLGenerator.GetTableColumns(const ATableName: string): string;
begin
  Result := Format('SELECT * FROM %0:S WHERE 1<>2 %1:S', [ATableName, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetTempTableName: string;
begin
  Result := TBL_TEMP;
end;

function TAnsiSQLGenerator.GetUpdateVersionFieldQuery(
  AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute;
  AVersionValue, APKValue: Variant): Variant;
var
  LSQL: string;
begin
  LSQL := Format('UPDATE %0:S SET %1:S = coalesce(%1:S,0) + 1 WHERE (%2:S = %3:S) AND (coalesce(%1:S,0) = %4:S)'
    , [AUpdateCommand.Table.Name, AVersionColumn.Name,
      AUpdateCommand.PrimaryKeyColumn.Name, VarToStr(APKValue),
      VarToStr(AVersionValue)]);
  Result := LSQL;
end;

function TAnsiSQLGenerator.GetWhereAsString(const AWhereFields: IList<TSQLWhereField>): string;
var
  i, ix: Integer;
  LField: TSQLWhereField;
begin
  Result := '';
  ix := 0;
  for i:=0 to AWhereFields.Count - 1 do
  begin
    if i < ix then
      Continue;

    ix := i;

    LField := AWhereFields[i];
    if i > 0 then
      Result := Result + ' AND '
    else
      Result := CRLF + '  WHERE ';

    if LField.WhereOperator in StartOperators then
    begin
      ix := FindEnd(AWhereFields, i, LField.WhereOperator, GetEndOperator(LField.WhereOperator));
    end;

    Result := Result + LField.ToSQLString(GetEscapeFieldnameChar);
    Inc(ix);
  end;
end;

end.
