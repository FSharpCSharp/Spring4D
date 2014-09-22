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

unit Spring.Persistence.SQL.Generators.PostgreSQL;

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
  ///	  Represents <b>PostgreSQL</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TPostgreSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; override;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;

{ TPostgreSQLGenerator }

function TPostgreSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := ASequence.Sequence;
  Result := '';

  Result := Format('DROP SEQUENCE IF EXISTS %0:S; ', [LSequence.SequenceName]);

  Result := Result + Format('CREATE SEQUENCE %0:S INCREMENT %1:D MINVALUE %2:D;',
    [LSequence.SequenceName, LSequence.Increment, LSequence.InitialValue]);
end;

function TPostgreSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TPostgreSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Result := Format('SELECT nextval(%0:S);', [QuotedStr(ASequence.SequenceName)]);
end;

function TPostgreSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlPostgreSQL;
end;

function TPostgreSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if Result = 'FLOAT' then
    Result := 'DOUBLE PRECISION'
  else if StartsText('NCHAR', Result) then
    Result := Copy(Result, 2, Length(Result))
  else if StartsText('NVARCHAR', Result) then
    Result := Copy(Result, 2, Length(Result))
  else if Result = 'BLOB' then
    Result := 'BYTEA'
  else if Result = 'BIT' then
    Result := 'BOOLEAN';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TPostgreSQLGenerator.Create);

end.
