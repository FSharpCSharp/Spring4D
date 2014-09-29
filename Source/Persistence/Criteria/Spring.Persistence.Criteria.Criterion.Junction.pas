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

unit Spring.Persistence.Criteria.Criterion.Junction;

{$I Spring.inc}

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TJunction = class(TInterfacedObject, ICriterion)
  private
    fCriterions: IList<ICriterion>;
    fEntityClass: TClass;
    function GetEntityClass: TClass;
    procedure SetEntityClass(value: TClass);
  protected
    function GetMatchMode: TMatchMode; virtual;
    function GetWhereOperator: TWhereOperator; virtual; abstract;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TDMLCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; virtual;
  public
    constructor Create; virtual;

    function Add(const criterion: ICriterion): TJunction;

    property Criterions: IList<ICriterion> read fCriterions;
  end;

implementation


{$REGION 'TJunction'}

constructor TJunction.Create;
begin
  inherited Create;
  fCriterions := TCollections.CreateList<ICriterion>;
end;

function TJunction.Add(const criterion: ICriterion): TJunction;
begin
  fCriterions.Add(criterion);
  Result := Self;
end;

function TJunction.GetEntityClass: TClass;
begin
  Result := fEntityClass;
end;

function TJunction.GetMatchMode: TMatchMode;
begin
  Result := mmExact;
end;

procedure TJunction.SetEntityClass(value: TClass);
begin
  fEntityClass := value;
end;

function TJunction.ToSqlString(const params: IList<TDBParam>;
  const command: TDMLCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  i: Integer;
  criterion: ICriterion;
  sql: string;
  whereField: TSQLWhereField;
begin
  Assert(command is TWhereCommand);

  Result := '';
  for i := 0 to fCriterions.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ' ' + WhereOpNames[GetWhereOperator] + ' ';

    criterion := fCriterions[i];
    sql := criterion.ToSqlString(params, command, generator, False);

    Result := Result + sql;
  end;

  if addToCommand then
  begin
    whereField := TSQLWhereField.Create(Result, '');
    whereField.MatchMode := GetMatchMode;
    whereField.WhereOperator := woJunction;
    TWhereCommand(command).WhereFields.Add(whereField);
  end;
end;

{$ENDREGION}


end.
