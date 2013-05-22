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
unit SQL.Generator.MSSQL;

interface

uses
  SQL.Generator.Ansi, Mapping.Attributes, SQL.Interfaces, SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>Microsoft SQL Server</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TMSSQLServerSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetTempTableName(): string; override;
    function GetPrimaryKeyDefinition(AField: TSQLCreateField): string; override;
    function GetSQLTableExists(const ATablename: string): string; override;
  end;

implementation

uses
  SQL.Register
  ,StrUtils
  ,SysUtils
  ;

{ TMSSQLServerSQLGenerator }

function TMSSQLServerSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT CAST( SCOPE_IDENTITY() AS BIGINT);';
end;

function TMSSQLServerSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit,
  AOffset: Integer): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  LSQL := ASql;
  try
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT * FROM (')
      .AppendLine
      .Append('  SELECT *, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS ORM_ROW_NUM FROM (')
      .AppendLine.Append('    ')
      .Append(LSQL)
      .Append(') AS ORM_TOTAL_1')
      .AppendLine
      .Append('  ) AS ORM_TOTAL_2')
      .AppendLine
      .AppendFormat(' WHERE (ORM_ROW_NUM>=%0:D) AND (ORM_ROW_NUM < %0:D+%1:D);', [AOffset, ALimit]);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TMSSQLServerSQLGenerator.GetPrimaryKeyDefinition(AField: TSQLCreateField): string;
begin
  Result := Format('CONSTRAINT PK_%0:S_%1:S PRIMARY KEY',
    [AField.Table.GetNameWithoutSchema, AField.Fieldname]);
end;

function TMSSQLServerSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMSSQL;
end;

function TMSSQLServerSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if Result = 'BLOB' then
    Result := 'IMAGE'
  else if Result = 'TIMESTAMP' then
    Result := 'DATETIME';

  if AField.IsIdentity then
    Result := Result + ' IDENTITY(1,1)';
end;

function TMSSQLServerSQLGenerator.GetSQLTableExists(const ATablename: string): string;
var
  LSchema, LTable: string;
begin
  ParseFullTablename(ATablename, LTable, LSchema);
  if (LSchema <> '') then
    Result := Format('SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = %1:S AND TABLE_NAME = %0:S '
    , [QuotedStr(LTable), QuotedStr(LSchema)])
  else
    Result := Format('SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = %0:S'
    , [QuotedStr(LTable)]);
end;

function TMSSQLServerSQLGenerator.GetTempTableName: string;
begin
  Result := '#' + inherited GetTempTableName;
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TMSSQLServerSQLGenerator.Create());

end.
