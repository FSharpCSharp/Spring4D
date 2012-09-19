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
unit SQL.Generator.Ansi;

interface

uses
  SQL.AbstractSQLGenerator, SQL.Commands, SQL.Types, Generics.Collections, Mapping.Attributes
  , SQL.Interfaces, TypInfo, SysUtils;

type
  TAnsiSQLGenerator = class(TAbstractSQLGenerator)
  protected
    procedure DoGenerateBackupTable(const ATableName: string; var ASqlBuilder: TStringBuilder); virtual;
    procedure DoGenerateRestoreTable(const ATablename: string;
      ACreateColumns: TObjectList<TSQLCreateField>; ADbColumns: TList<string>; var ASqlBuilder: TStringBuilder); virtual;
    function DoGenerateCreateTable(const ATableName: string; AColumns: TObjectList<TSQLCreateField>): string; virtual;

    function GetGroupByAsString(const AGroupFields: TEnumerable<TSQLGroupByField>): string; virtual;
    function GetJoinAsString(const AJoin: TSQLJoin): string; virtual;
    function GetJoinsAsString(const AJoinFields: TEnumerable<TSQLJoin>): string; virtual;
    function GetOrderAsString(const AOrderFields: TEnumerable<TSQLOrderField>): string; virtual;
    function GetWhereAsString(const AWhereFields: TEnumerable<TSQLWhereField>): string; virtual;
    function GetSelectFieldsAsString(const ASelectFields: TEnumerable<TSQLSelectField>): string; virtual;
    function GetCreateFieldsAsString(const ACreateFields: TEnumerable<TSQLCreateField>): string; overload; virtual;
    function GetCreateFieldsAsString(const ACreateFields: TList<string>): string; overload; virtual;
    function GetCopyFieldsAsString(const ACreateFields: TEnumerable<TSQLCreateField>; const ACopyFields: TList<string>): string; virtual;
    function GetTempTableName(): string; virtual;
    function GetPrimaryKeyDefinition(AField: TSQLCreateField): string; virtual;
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateSelect(ASelectCommand: TSelectCommand): string; override;
    function GenerateInsert(AInsertCommand: TInsertCommand): string; override;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string; override;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string; override;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string; override;
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
  end;

implementation

uses
  Core.Exceptions
  ,Core.Utils
  ,Mapping.RttiExplorer
  ,StrUtils
  ;

{ TAnsiSQLGenerator }

procedure TAnsiSQLGenerator.DoGenerateBackupTable(const ATableName: string;
  var ASqlBuilder: TStringBuilder);
begin
  //select old data to temporary table
  ASqlBuilder.AppendFormat(' SELECT * INTO %0:S FROM %1:S;',
        [GetTempTableName, ATableName]).AppendLine;
  //drop table
  ASqlBuilder.AppendFormat(' DROP TABLE %0:S; ', [ATableName]).AppendLine;
end;

function TAnsiSQLGenerator.DoGenerateCreateTable(const ATableName: string;
  AColumns: TObjectList<TSQLCreateField>): string;
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
      LSqlBuilder.AppendFormat('%0:S %1:S %2:S %3:S',
        [
          LField.Fieldname
          ,GetSQLDataTypeName(LField)
          ,IfThen(cpPrimaryKey in LField.Properties, GetPrimaryKeyDefinition(LField))
          ,IfThen(cpNotNull in LField.Properties, 'NOT NULL', 'NULL')
        ]
      );
    end;

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

procedure TAnsiSQLGenerator.DoGenerateRestoreTable(const ATablename: string;
  ACreateColumns: TObjectList<TSQLCreateField>; ADbColumns: TList<string>;
  var ASqlBuilder: TStringBuilder);
begin
  ASqlBuilder.AppendFormat(' INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S;',
    [ATablename, GetTempTableName, GetCreateFieldsAsString(ACreateColumns),
    GetCopyFieldsAsString(ACreateColumns, ADbColumns)]).AppendLine;

  //drop temporary table
  ASqlBuilder.AppendFormat(' DROP TABLE %0:S;', [GetTempTableName]);
end;

function TAnsiSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLForeignKeyField;
begin
  LSqlBuilder := TStringBuilder.Create();
  try
    Result := '';

    for i := 0 to ACreateFKCommand.ForeignKeys.Count - 1 do
    begin
      LField := ACreateFKCommand.ForeignKeys[i];
      LSqlBuilder.Clear;
      LSqlBuilder.AppendFormat('ALTER TABLE %0:S ', [ACreateFKCommand.Table.Name])
        .AppendLine
        .AppendFormat('ADD CONSTRAINT %0:S', [LField.ForeignKeyName])
        .AppendLine
        .AppendFormat('FOREIGN KEY(%0:S)', [LField.Fieldname])
        .AppendLine
        .AppendFormat(' REFERENCES %0:S (%1:S)', [LField.ReferencedTableName, LField.ReferencedColumnName])
        .AppendLine
        .Append(LField.GetConstraintsAsString)
        .Append(';').AppendLine;

      Result := Result + LSqlBuilder.ToString;
    end;

  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string;
var
  LSqlBuilder: TStringBuilder;
begin
  Assert(Assigned(ACreateTableCommand));

  LSqlBuilder := TStringBuilder.Create();
  try
    if ACreateTableCommand.TableExists then
    begin
      DoGenerateBackupTable(ACreateTableCommand.Table.Name, LSqlBuilder);
    end;

    LSqlBuilder.Append(DoGenerateCreateTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns));
    LSqlBuilder.Append(');');

    if ACreateTableCommand.TableExists then
    begin
      DoGenerateRestoreTable(ACreateTableCommand.Table.Name, ACreateTableCommand.Columns,
        ACreateTableCommand.DbColumns, LSqlBuilder);
    end;

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateDelete(ADeleteCommand: TDeleteCommand): string;
var
  LSqlBuilder: TStringBuilder;
  LWhereField: TSQLWhereField;
  ix: Integer;
begin
  Assert(Assigned(ADeleteCommand));

  LSqlBuilder := TStringBuilder.Create();
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

      LSqlBuilder.Append(Format('%0:S=:%0:S', [LWhereField.Fieldname]));

      Inc(ix);
    end;

    LSqlBuilder.Append(';');

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
  LBuilder := TStringBuilder.Create();
  try
    LSQL := ASql;
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT COUNT(*) FROM (')
      .AppendLine
      .Append(LSQL)
      .AppendLine
      .Append(') AS ORM_GET_QUERY_COUNT;')
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

    sFields := sFields + AInsertCommand.InsertFields[i].Fieldname;
    sParams := sParams + ':' + AInsertCommand.InsertFields[i].Fieldname;
  end;

  Result := Result + AInsertCommand.Table.Name + ' (' + CRLF + '  ' + sFields + ')' + CRLF +
    '  VALUES (' + CRLF + sParams + ');';
end;

function TAnsiSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit,
  AOffset: Integer): string;
var
  LSQL: string;
begin
  LSQL := ASql;
  if EndsStr(';', LSQL) then
    SetLength(LSQL, Length(LSQL)-1);

  Result := LSQL + Format(' LIMIT %1:D,%0:D;', [ALimit, AOffset]);
end;

function TAnsiSQLGenerator.GenerateSelect(ASelectCommand: TSelectCommand): string;
var
  LSqlBuilder: TStringBuilder;
begin
  Assert(Assigned(ASelectCommand));
  Assert(ASelectCommand.SelectFields.Count > 0);
  Assert(ASelectCommand.Table.Alias <> '');

  LSqlBuilder := TStringBuilder.Create();
  try
    LSqlBuilder.Append('SELECT ')
      .Append(GetSelectFieldsAsString(ASelectCommand.SelectFields)).AppendLine
      .Append(' FROM ').Append(ASelectCommand.Table.GetFullTableName)
      .Append(GetJoinsAsString(ASelectCommand.Joins))
      .Append(GetWhereAsString(ASelectCommand.WhereFields))
      .Append(GetGroupByAsString(ASelectCommand.GroupByFields))
      .Append(GetOrderAsString(ASelectCommand.OrderByFields))
      .Append(';');

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

  LSqlBuilder := TStringBuilder.Create();
  try
    LSqlBuilder.Append('UPDATE ')
      .Append(AUpdateCommand.Table.Name)
      .Append(' SET ').AppendLine;

    ix := 0;

    for LField in AUpdateCommand.UpdateFields do
    begin
      if ix > 0 then
        LSqlBuilder.Append(',');

      LSqlBuilder.Append(Format('%0:S=:%0:S', [LField.Fieldname]));
      Inc(ix);
    end;

    ix := 0;

    for LWhereField in AUpdateCommand.WhereFields do
    begin
      if ix = 0 then
        LSqlBuilder.AppendLine.Append(' WHERE ')
      else
        LSqlBuilder.Append(' AND ');

      LSqlBuilder.Append(Format('%0:S=:%0:S', [LWhereField.Fieldname]));

      Inc(ix);
    end;

    LSqlBuilder.Append(';');

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(
  const ACreateFields: TEnumerable<TSQLCreateField>): string;
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

    Result := Result + LField.Fieldname;

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetCopyFieldsAsString(const ACreateFields: TEnumerable<TSQLCreateField>;
  const ACopyFields: TList<string>): string;
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
      Result := Result + LField.Fieldname
    else
      Result := Result + 'NULL';

    Inc(i);
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(const ACreateFields: TList<string>): string;
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

function TAnsiSQLGenerator.GetGroupByAsString(
  const AGroupFields: TEnumerable<TSQLGroupByField>): string;
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

    Result := Result + LField.GetFullFieldname;

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
      LSegment.PKField.GetFullFieldname + '=' + LSegment.FKField.GetFullFieldname;
  end;
end;

function TAnsiSQLGenerator.GetJoinsAsString(const AJoinFields: TEnumerable<TSQLJoin>): string;
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
  const AOrderFields: TEnumerable<TSQLOrderField>): string;
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


    Result := Result + LField.GetFullOrderByFieldname;

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

function TAnsiSQLGenerator.GetSelectFieldsAsString(
  const ASelectFields: TEnumerable<TSQLSelectField>): string;
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

    Result := Result + LField.GetFullFieldname;

    Inc(i);
  end;
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
  Result := Format('SELECT COUNT(*) FROM %0:S;', [ATablename]);
end;

function TAnsiSQLGenerator.GetTableColumns(const ATableName: string): string;
begin
  Result := Format('SELECT * FROM %0:S WHERE 1<>2;', [ATableName]);
end;

function TAnsiSQLGenerator.GetTempTableName: string;
begin
  Result := TBL_TEMP;
end;

function TAnsiSQLGenerator.GetWhereAsString(const AWhereFields: TEnumerable<TSQLWhereField>): string;
var
  i: Integer;
  LField: TSQLWhereField;
begin
  Result := '';
  i := 0;
  for LField in AWhereFields do
  begin
    if i > 0 then
      Result := Result + ' AND '
    else
      Result := CRLF + '  WHERE ';


    Result := Result + LField.ToSQLString;

    Inc(i);
  end;
end;

end.
