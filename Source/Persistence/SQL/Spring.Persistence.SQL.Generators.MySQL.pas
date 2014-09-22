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

unit Spring.Persistence.SQL.Generators.MySQL;

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
  ///	  Represents <b>MySQL</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TMySQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
    function GetEscapeFieldnameChar: Char; override;
  end;

implementation

uses
  StrUtils,
  Spring.Persistence.SQL.Register;

{ TMySQLGenerator }

function TMySQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TMySQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT LAST_INSERT_ID;';
end;

function TMySQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Result := '';
end;

// Pü 2014-06-01:
// MySQL has the backtick as FieldEscape unless MySQL is set to Ansi mode
function TMySQLGenerator.GetEscapeFieldnameChar: Char;
begin
  Result := '`';
end;

function TMySQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMySQL;
end;

function TMySQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if StartsText('NUMERIC', Result) then
    Result := 'DECIMAL' + Copy(Result, 8, Length(Result))
  else if StartsText('NCHAR', Result) then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET ucs2'
  else if StartsText('NVARCHAR', Result) then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET ucs2';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TMySQLGenerator.Create);

end.
