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

unit Spring.Persistence.SQL.Generators.Firebird;

{$I Spring.inc}

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>Firebird/Interbase</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TFirebirdSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; override;
    function GetSQLSequenceCount(const ASequenceName: string): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetSQLTableExists(const ATablename: string): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;

{ TFirebirdSQLGenerator }

function TFirebirdSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := ASequence.Sequence;
  Result := '';
  if ASequence.SequenceExists then
    Result := Format('DROP SEQUENCE %0:S; ', [LSequence.SequenceName]);

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

function TFirebirdSQLGenerator.GetSQLTableExists(const ATablename: string): string;
begin
  Result := '';
 { Result := Format('SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = %0:S '
    , [QuotedStr(ATablename)]);}
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TFirebirdSQLGenerator.Create);

end.
