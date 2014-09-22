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

unit Spring.Persistence.Criteria.Criterion.PropertyExpression;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TPropertyExpression = class(TAbstractCriterion)
  private
    FOperator: TWhereOperator;
    FPropertyName: string;
    FOtherPropertyName: string;
    FTable: TSQLTable;
    FOtherTable: TSQLTable;
  public
    constructor Create(const APropertyName, AOtherPropertyName: string;
      AOperator: TWhereOperator; ATable: TSQLTable = nil;
      AOtherTable: TSQLTable = nil); virtual;
    destructor Destroy; override;
  public
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand;
      AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

uses
  SysUtils;

{ TPropertyExpression }

constructor TPropertyExpression.Create(const APropertyName, AOtherPropertyName: string; AOperator: TWhereOperator; ATable, AOtherTable: TSQLTable);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FOtherPropertyName := AOtherPropertyName;
  FOperator := AOperator;
  FTable := ATable;
  FOtherTable := AOtherTable;
end;

destructor TPropertyExpression.Destroy;
begin
  if Assigned(FTable) then
    FTable.Free;
  if Assigned(FOtherTable) then
    FOtherTable.Free;
  inherited Destroy;
end;

function TPropertyExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TPropertyExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWherePropertyField;
  LTable, LOtherTable: TSQLTable;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LTable := FTable;
  LOtherTable := FOtherTable;

  LTable := GetCriterionTable(ACommand, LTable);
  LOtherTable := GetCriterionTable(ACommand, LOtherTable);

  if not Assigned(LTable) then
    LTable := ACommand.Table;

  if not Assigned(LOtherTable) then
    LOtherTable := ACommand.Table;

  LWhere := TSQLWherePropertyField.Create(AnsiUpperCase(FPropertyName), UpperCase(FOtherPropertyName)
    , LTable, LOtherTable);
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

end.
