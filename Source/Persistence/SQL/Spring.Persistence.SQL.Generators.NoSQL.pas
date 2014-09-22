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

unit Spring.Persistence.SQL.Generators.NoSQL;

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
  ///	  Represents base <b>NoSQL</b> database statements generator.
  ///	</summary>
  {$ENDREGION}
  TNoSQLGenerator = class(TAbstractSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;

    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): IList<string>; override;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;

    function GetSQLSequenceCount(const ASequenceName: string): string; override;
    function GetTableColumns(const ATableName: string): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetSQLTableExists(const ATablename: string): string; override;
    function GetEscapeFieldnameChar: Char; override;
  end;

implementation

{ TNoSQLGenerator }

function TNoSQLGenerator.GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>;
begin
  Result := TCollections.CreateList<string>;
end;

function TNoSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): IList<string>;
begin
  Result := TCollections.CreateList<string>;
end;

function TNoSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := ' ';
end;

function TNoSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetEscapeFieldnameChar: Char;
begin
  Result := '"';
end;

function TNoSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlNoSQL;
end;

function TNoSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetSQLTableExists(const ATablename: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetTableColumns(const ATableName: string): string;
begin
  Result := '';
end;

end.
