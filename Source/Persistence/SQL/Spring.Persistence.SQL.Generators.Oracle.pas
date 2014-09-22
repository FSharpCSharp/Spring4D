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

unit Spring.Persistence.SQL.Generators.Oracle;

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
  ///	  Represents <b>Oracle</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TOracleSQLGenerator = class(TAnsiSQLGenerator)
  protected
    function GetSplitStatementSymbol: string; override;
  public
    function DoGenerateBackupTable(const ATableName: string): TArray<string>; override;
    function GenerateGetQueryCount(const ASql: string): string; override;
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

{ TOracleSQLGenerator }

function TOracleSQLGenerator.DoGenerateBackupTable(const ATableName: string): TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := Format('CREATE TABLE %0:S AS SELECT * FROM %1:S',
        [GetTempTableName, ATableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:S ', [ATableName]);
end;

function TOracleSQLGenerator.GenerateCreateSequence(ASequence: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := ASequence.Sequence;
  Result := 'BEGIN ';
  if ASequence.SequenceExists then
    Result := Result + 'EXECUTE IMMEDIATE ' + QuotedStr(Format('DROP SEQUENCE "%0:S" ', [LSequence.SequenceName])) + ';';

  Result := Result + ' EXECUTE IMMEDIATE ' + QuotedStr(Format('CREATE SEQUENCE "%0:S" MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY %2:D START WITH %1:D CACHE 20 NOORDER NOCYCLE',
    [LSequence.SequenceName, LSequence.InitialValue, LSequence.Increment]));

  Result := Result + ' END;';
end;

function TOracleSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TOracleSQLGenerator.GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string;
begin
  Assert(Assigned(ASequence));
  Result := Format('select %0:S.nextval from dual', [ASequence.SequenceName]);
end;

function TOracleSQLGenerator.GenerateGetQueryCount(const ASql: string): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  try
    LSQL := ASql;
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT COUNT(*) FROM (')
      .AppendLine
      .Append(LSQL)
      .AppendLine
      .Append(')').Append(GetSplitStatementSymbol)
      ;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
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
      .AppendLine.Append(' SELECT AROWNUM.*, ROWNUM r___  FROM (  ')
      .Append(LSQL)
      .Append(') AROWNUM ')
      .AppendFormat('WHERE ROWNUM < (%0:D+%1:D) )', [AOffset+1, ALimit])
      .AppendLine
      .AppendFormat(' WHERE r___ >=%0:D', [AOffset+1]);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TOracleSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlOracle;
end;

function TOracleSQLGenerator.GetSplitStatementSymbol: string;
begin
  Result := '';
end;

function TOracleSQLGenerator.GetSQLDataTypeName(AField: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(AField);
  if StartsText('NUMERIC', Result) then
    Result := 'NUMBER' + Copy(Result, 8, Length(Result))
  else if StartsText('NVARCHAR', Result) then
    Result := 'NVARCHAR2' + Copy(Result, 9, Length(Result))
  else if StartsText('VARCHAR', Result) then
    Result := 'VARCHAR2' + Copy(Result, 8, Length(Result))
  else if Result = 'BIT' then
    Result := 'SMALLINT' // 'PLS_INTEGER'
  else if Result = 'FLOAT' then
    Result := 'BINARY_DOUBLE';
end;

function TOracleSQLGenerator.GetSQLSequenceCount(const ASequenceName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM USER_SEQUENCES WHERE SEQUENCE_NAME = %0:S ',
    [QuotedStr(ASequenceName)]);
end;

function TOracleSQLGenerator.GetSQLTableExists(const ATablename: string): string;
var
  LSchema, LTable: string;
begin
  ParseFullTablename(ATablename, LTable, LSchema);
  if (LSchema <> '') then
    Result := Format('SELECT COUNT(*) FROM ALL_OBJECTS WHERE OBJECT_TYPE = (''TABLE'') AND UPPER(OBJECT_NAME) = %0:S AND UPPER(OWNER) = %1:S'
    , [QuotedStr(UpperCase(LTable)), QuotedStr(UpperCase(LSchema))])
  else
    Result := Format('SELECT COUNT(*) FROM ALL_OBJECTS WHERE OBJECT_TYPE = (''TABLE'') AND UPPER(OBJECT_NAME) = %0:S'
    , [QuotedStr(UpperCase(LTable))]);
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TOracleSQLGenerator.Create);

end.
