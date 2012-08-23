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
unit SQL.Generator.Oracle;

interface

uses
  SQL.Generator.Ansi, Mapping.Attributes, SQL.Interfaces, SQL.Commands;

type
  TOracleSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GetSQLSequenceCount(const ASequenceName: string): string; override;
  end;

implementation

uses
  SysUtils
  ,StrUtils
  ,SQL.Register
  ;

{ TOracleSQLGenerator }

function TOracleSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := ASequence.Sequence;
  Result := '';
  if ASequence.SequenceExists then
  begin
    Result := Format('DROP SEQUENCE "%0:S"; ', [LSequence.SequenceName]);
  end;

  Result := Result + Format('CREATE SEQUENCE "%0:S" MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY %2:D START WITH %1:D CACHE 20 NOORDER NOCYCLE;',
    [LSequence.SequenceName, LSequence.InitialValue, LSequence.Increment]);
end;

function TOracleSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
  //Result := Format('returning %0:S as NewID;', [AIdentityColumn.Name]);
end;

function TOracleSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Assert(Assigned(ASequence));
  Result := Format('select %0:S.nextval from dual;', [ASequence.SequenceName]);
end;

function TOracleSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string;
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
      .AppendLine.Append('    ')
      .Append(LSQL)
      .Append(') AS ORM_TOTAL_1')
      .AppendLine
      .AppendFormat(' WHERE (ROWNUM>=%0:D) AND (ROWNUM < %0:D+%1:D);', [AOffset, ALimit]);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TOracleSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlOracle;
end;

function TOracleSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM USER_SEQUENCES WHERE SEQUENCE_NAME = %0:S; ',
    [QuotedStr(ASequenceName)]);
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TOracleSQLGenerator.Create());

end.
