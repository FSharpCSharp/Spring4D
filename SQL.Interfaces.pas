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
unit SQL.Interfaces;

interface

uses
  Classes, SQL.Commands, SQL.Types, Mapping.Attributes;

type
  ICommandExecutionListener = interface
    ['{590E86C8-0B05-4BFE-9B26-3A9A4D0510BF}']
    procedure ExecutingCommand(const ACmd: string; AList: TList);
  end;

  TQueryLanguage = (qlAnsiSQL = 0, qlSQLite, qlMSSQL, qlASA, qlOracle, qlFirebird, qlPostgreSQL, qlMySQL, qlNoSQL);

  ISQLGenerator = interface
    ['{8F46D275-50E4-4DE8-9E56-7D6599935E32}']
    function GetQueryLanguage(): TQueryLanguage;
    function GenerateSelect(ASelectCommand: TSelectCommand): string;
    function GenerateInsert(AInsertCommand: TInsertCommand): string;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): string;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): string;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string;
    function GenerateGetQueryCount(const ASql: string): string;
    function GetSQLTableCount(const ATablename: string): string;
    function GetSQLSequenceCount(const ASequenceName: string): string;
    function GetTableColumns(const ATableName: string): string;
  end;

implementation

end.
