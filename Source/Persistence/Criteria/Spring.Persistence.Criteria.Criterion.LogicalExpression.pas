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

unit Spring.Persistence.Criteria.Criterion.LogicalExpression;

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
  TLogicalExpression = class(TAbstractCriterion)
  private
    FOperator: TWhereOperator;
    FLeft: ICriterion;
    FRight: ICriterion;
  public
    constructor Create(ALeft, ARight: ICriterion; const AOperator: TWhereOperator); virtual;
  public
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

{ TLogicalExpression }

constructor TLogicalExpression.Create(ALeft, ARight: ICriterion; const AOperator: TWhereOperator);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
  FOperator := AOperator;
end;

function TLogicalExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TLogicalExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere, LEndOp: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LWhere := TSQLWhereField.Create('', '');
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere);
  LWhere.LeftSQL := FLeft.ToSqlString(AParams, ACommand, AGenerator, AAddToCommand);
  if Assigned(FRight) then
    LWhere.RightSQL := FRight.ToSqlString(AParams, ACommand, AGenerator, AAddToCommand);

  LEndOp := TSQLWhereField.Create('', '');
  LEndOp.MatchMode := GetMatchMode;
  LEndOp.WhereOperator := GetEndOperator(FOperator);
  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LEndOp);

  Result := LWhere.ToSQLString(AGenerator.GetEscapeFieldnameChar);

  if not AAddToCommand then
  begin
    LWhere.Free;
    LEndOp.Free;
  end;
end;

end.
