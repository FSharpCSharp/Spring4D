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

unit Spring.Persistence.Criteria.Criterion.LikeExpression;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TLikeExpression = class(TSimpleExpression)
  private
    FMatchMode: TMatchMode;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode); reintroduce; overload;

    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

uses
  SysUtils;

{ TLikeExpression }

constructor TLikeExpression.Create(const APropertyName: string; const AValue: TValue; AOperator: TWhereOperator; const AMatchMode: TMatchMode);
begin
  inherited Create(APropertyName, AValue, AOperator);
  FMatchMode := AMatchMode;
end;

function TLikeExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);

  Result := Format('%s %s %s', [PropertyName, WhereOpNames[GetWhereOperator],
    GetMatchModeString(FMatchMode, Value.AsString)]);

  LWhere := TSQLWhereField.Create(Result, GetCriterionTable(ACommand) );
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;

  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

end.
