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

unit Spring.Persistence.SQL.Generators.Abstract;

{$I Spring.inc}

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

const
  TBL_TEMP = 'ORM_TEMP';

type
  TAbstractSQLGenerator = class(TInterfacedObject, ISQLGenerator)
  protected
    function GetQueryLanguage: TQueryLanguage; virtual; abstract;
    function GenerateSelect(ASelectCommand: TSelectCommand): string; virtual; abstract;
    function GenerateInsert(AInsertCommand: TInsertCommand): string; virtual; abstract;
    function GenerateUpdate(AUpdateCommand: TUpdateCommand): string; virtual; abstract;
    function GenerateDelete(ADeleteCommand: TDeleteCommand): string; virtual; abstract;
    function GenerateCreateTable(ACreateTableCommand: TCreateTableCommand): IList<string>; virtual; abstract;
    function GenerateCreateFK(ACreateFKCommand: TCreateFKCommand): IList<string>; virtual; abstract;
    function GenerateCreateSequence(ASequence: TCreateSequenceCommand): string; virtual; abstract;
    function GenerateGetNextSequenceValue(ASequence: SequenceAttribute): string; virtual; abstract;
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; virtual; abstract;
    function GeneratePagedQuery(const ASql: string; const ALimit, AOffset: Integer): string; virtual; abstract;
    function GenerateGetQueryCount(const ASql: string): string; virtual; abstract;
    function GetSQLDataTypeName(AField: TSQLCreateField): string; virtual; abstract;
    function GetSQLTableCount(const ATablename: string): string; virtual; abstract;
    function GetSQLSequenceCount(const ASequenceName: string): string; virtual; abstract;
    function GetTableColumns(const ATableName: string): string; virtual; abstract;
    function GetSQLTableExists(const ATablename: string): string; virtual; abstract;
    function GetEscapeFieldnameChar: Char; virtual; abstract;
    function GenerateUniqueId: Variant; virtual;
    function GetUpdateVersionFieldQuery(AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute; AVersionValue,APKValue: Variant): Variant; virtual; abstract;
    function FindEnd(const AWhereFields: IList<TSQLWhereField>; AStartIndex: Integer; AStartToken, AEndToken: TWhereOperator): Integer; virtual;
  end;

implementation

uses
  Variants;

{ TAbstractSQLGenerator }

function TAbstractSQLGenerator.FindEnd(
  const AWhereFields: IList<TSQLWhereField>; AStartIndex: Integer; AStartToken,
  AEndToken: TWhereOperator): Integer;
var
    LCount: Integer;
  begin
    LCount := 0;
    for Result := AStartIndex to AWhereFields.Count - 1 do
    begin
      if (AWhereFields[Result].WhereOperator = AStartToken) then
      begin
        Inc(LCount);
        Continue;
      end;

      if (AWhereFields[Result].WhereOperator = AEndToken) then
      begin
        Dec(LCount);

        if LCount = 0 then
        begin
          Exit;
        end;
      end;
    end;
    Result := AStartIndex;
end;

function TAbstractSQLGenerator.GenerateUniqueId: Variant;
begin
  Result := Null;
end;

end.
