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

unit Spring.Persistence.SQL.Interfaces;

{$I Spring.inc}

interface

uses
  Classes,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Types;

type
  ICommandExecutionListener = interface
    ['{590E86C8-0B05-4BFE-9B26-3A9A4D0510BF}']
    procedure ExecutingCommand(const ACmd: string; AList: IList);
  end;

  TQueryLanguage = (qlAnsiSQL = 0, qlSQLite, qlMSSQL, qlASA, qlOracle, qlFirebird, qlPostgreSQL, qlMySQL, qlNoSQL, qlMongoDB);

  ISQLGenerator = interface
    ['{8F46D275-50E4-4DE8-9E56-7D6599935E32}']
    function GetQueryLanguage: TQueryLanguage;
    function GenerateSelect(ASelectCommand: TSelectCommand): string;
    function GenerateInsert(AInsertCommand: TInsertCommand): string;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): IList<string>;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string;
    function GenerateGetQueryCount(const ASql: string): string;
    function GenerateUniqueId: Variant;
    function GetSQLTableCount(const ATablename: string): string;
    function GetSQLSequenceCount(const ASequenceName: string): string;
    function GetTableColumns(const ATableName: string): string;
    function GetSQLTableExists(const ATablename: string): string;
    function GetEscapeFieldnameChar: Char;
    function GetUpdateVersionFieldQuery(AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute; AVersionValue, APKValue: Variant): Variant;
  end;

implementation

uses
  // auto register all generators
  Spring.Persistence.SQL.Generators.Register;

end.
