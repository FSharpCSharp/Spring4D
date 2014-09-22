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
    FCriterions: IList<ICriterion>;
    FEntityClass: TClass;
  protected
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; virtual;
    procedure SetEntityClass(const Value: TClass); virtual;
    function GetEntityClass: TClass; virtual;
    function GetMatchMode: TMatchMode; virtual;
    function GetWhereOperator: TWhereOperator; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): TJunction;

    property Criterions: IList<ICriterion> read FCriterions;
  end;

implementation

{ TJunction }

constructor TJunction.Create;
begin
  inherited Create;
  FCriterions := TCollections.CreateList<ICriterion>;
end;

destructor TJunction.Destroy;
begin
  inherited Destroy;
end;

function TJunction.Add(ACriterion: ICriterion): TJunction;
begin
  FCriterions.Add(ACriterion);
  Result := Self;
end;

function TJunction.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TJunction.GetMatchMode: TMatchMode;
begin
  Result := mmExact;
end;

procedure TJunction.SetEntityClass(const Value: TClass);
begin
  FEntityClass := Value;
end;

function TJunction.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand;
  AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LCriterion: ICriterion;
  i: Integer;
  LSql: string;
  LWhere: TSQLWhereField;
begin
  Result := '';

  Assert(ACommand is TWhereCommand);

  for i := 0 to FCriterions.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ' ' + WhereOpNames[GetWhereOperator] + ' ';

    LCriterion := FCriterions[i];
    LSql := LCriterion.ToSqlString(AParams, ACommand, AGenerator, False);

    Result := Result + LSql;
  end;

  if AAddToCommand then
  begin
    LWhere := TSQLWhereField.Create(Result, '');
    LWhere.MatchMode := GetMatchMode;
    LWhere.WhereOperator := woJunction;
    TWhereCommand(ACommand).WhereFields.Add(LWhere);
  end;
end;

end.
