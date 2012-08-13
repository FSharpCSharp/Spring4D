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
unit Adapters.ASA;

interface

uses
  Adapters.ADO, SysUtils, Mapping.Attributes;

type
  TASAResultsetAdapter = class(TADOResultSetAdapter);

  TASAStatementAdapter = class(TADOStatementAdapter);

  TASAConnectionAdapter = class(TADOConnectionAdapter)
  public
    function GetDriverName: string; override;
  end;

  TASATransactionAdapter = class(TADOTransactionAdapter);

  TASASQLGenerator = class(TADOSQLGenerator)
  public
    function GetDriverName(): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: Column): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
  end;

  ESybaseASAStatementAdapterException = Exception;

implementation

uses
  SQL.Register
  ,StrUtils
  ;

{ TASAConnectionAdapter }

function TASAConnectionAdapter.GetDriverName: string;
begin
  Result := 'ASA';
end;

{ TASASQLGenerator }

function TASASQLGenerator.GenerateGetLastInsertId(AIdentityColumn: Column): string;
begin
  Result := 'SELECT @@IDENTITY;';
end;

function TASASQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit,
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
      .Append('  SELECT *, ROW_NUMBER() OVER (ORDER BY (NULL)) AS ORM_ROW_NUM FROM (')
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

function TASASQLGenerator.GetDriverName: string;
begin
  Result := 'ASA';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TASASQLGenerator.Create());

end.
