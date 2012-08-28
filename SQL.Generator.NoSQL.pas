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
unit SQL.Generator.NoSQL;

interface

uses
  SQL.AbstractSQLGenerator, SQL.Commands, SQL.Types, Generics.Collections, Mapping.Attributes
  , SQL.Interfaces;

type
  TNoSQLGenerator = class(TAbstractSQLGenerator)
  protected

  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateSelect(ASelectCommand: TSelectCommand): string; override;
    function GenerateInsert(AInsertCommand: TInsertCommand): string; override;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string; override;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string; override;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GenerateGetQueryCount(const ASql: string): string; override;
    function GetSQLTableCount(const ATablename: string): string; override;
    function GetSQLSequenceCount(const ASequenceName: string): string; override;
    function GetTableColumns(const ATableName: string): string; override;
  end;

implementation

uses
  Core.Exceptions
  ,Core.Utils
  ;


{ TNoSQLGenerator }

function TNoSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateDelete(ADeleteCommand: TDeleteCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '_id';
end;

function TNoSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateGetQueryCount(const ASql: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateInsert(AInsertCommand: TInsertCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateSelect(ASelectCommand: TSelectCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateUpdate(AUpdateCommand: TUpdateCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlNoSQL;
end;

function TNoSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetSQLTableCount(const ATablename: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetTableColumns(const ATableName: string): string;
begin
  Result := '';
end;

end.
