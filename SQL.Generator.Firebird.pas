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
unit SQL.Generator.Firebird;

interface

uses
  SQL.Generator.Ansi, Mapping.Attributes, SQL.Interfaces, SQL.Commands, SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>Firebird/Interbase</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TFirebirdSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GetSQLSequenceCount(const ASequenceName: string): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
  end;

implementation

uses
  SysUtils
  ,StrUtils
  ,SQL.Register
  ;

{ TFirebirdSQLGenerator }

function TFirebirdSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := ASequence.Sequence;
  Result := '';
  if ASequence.SequenceExists then
  begin
    Result := Format('DROP SEQUENCE %0:S; ', [LSequence.SequenceName]);
  end;

  Result := Result + Format('CREATE SEQUENCE %0:S;', [LSequence.SequenceName]);
end;

function TFirebirdSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TFirebirdSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Assert(Assigned(ASequence));
  Result := Format('SELECT NEXT VALUE FOR %0:S FROM RDB$DATABASE;', [ASequence.SequenceName]);
end;

function TFirebirdSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit,
  AOffset: Integer): string;
var
  LSQL: string;
begin
  LSQL := ASql;
  if EndsStr(';', LSQL) then
    SetLength(LSQL, Length(LSQL)-1);

  Result := LSQL + Format(' ROWS %1:D TO %0:D;', [AOffset + ALimit, AOffset]);
end;

function TFirebirdSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlFirebird;
end;

function TFirebirdSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if StartsText(Result, 'NCHAR') then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET UNICODE_FSS'
  else if StartsText(Result, 'NVARCHAR') then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET UNICODE_FSS';
end;

function TFirebirdSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := Format('SELECT COUNT(*) '+
		'FROM RDB$GENERATORS '+
		'WHERE (RDB$SYSTEM_FLAG=0) AND (RDB$GENERATOR_NAME = %0:S); ', [QuotedStr(ASequenceName)]);
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TFirebirdSQLGenerator.Create());

end.
