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
unit SQL.AnsiSQLGenerator;

interface

uses
  SQL.AbstractSQLGenerator, SQL.Commands, SQL.Types, Generics.Collections, Mapping.Attributes;

type
  TAnsiSQLGenerator = class(TAbstractSQLGenerator)
  protected
    function GetGroupByAsString(const AGroupFields: TEnumerable<TSQLGroupByField>): string; virtual;
    function GetJoinAsString(const AJoin: TSQLJoin): string; virtual;
    function GetJoinsAsString(const AJoinFields: TEnumerable<TSQLJoin>): string; virtual;
    function GetOrderAsString(const AOrderFields: TEnumerable<TSQLOrderField>): string; virtual;
    function GetWhereAsString(const AWhereFields: TEnumerable<TSQLWhereField>): string; virtual;
    function GetSelectFieldsAsString(const ASelectFields: TEnumerable<TSQLSelectField>): string; virtual;
  public
    function GetDriverName(): string; override;
    function GenerateSelect(ASelectCommand: TSelectCommand): string; override;
    function GenerateInsert(AInsertCommand: TInsertCommand): string; override;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string; override;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string; override;
    function GenerateCreateTable(): string; override;
    function GenerateCreateFK(): string; override;
    function GenerateCreateSequence(ASequence: SequenceAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GenerateGetQueryCount(const ASql: string): string; override;
  end;

implementation

uses
  Core.Exceptions
  ,SysUtils
  ,StrUtils
  ;

{ TAnsiSQLGenerator }

function TAnsiSQLGenerator.GenerateCreateFK: string;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TAnsiSQLGenerator.GenerateCreateSequence(ASequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateCreateTable: string;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
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

function TAnsiSQLGenerator.GetDriverName: string;
begin
  Result := 'ANSI-SQL';
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
