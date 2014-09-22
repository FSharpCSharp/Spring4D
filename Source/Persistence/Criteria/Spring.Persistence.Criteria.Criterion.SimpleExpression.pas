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

unit Spring.Persistence.Criteria.Criterion.SimpleExpression;

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
  TSimpleExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FValue: TValue;
    FOperator: TWhereOperator;
  public
    constructor Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
    function GetWhereOperator: TWhereOperator; override;

    property PropertyName: string read FPropertyName;
    property Value: TValue read FValue;
  end;

implementation

{ TSimpleExpression }

constructor TSimpleExpression.Create(const APropertyName: string; const AValue: TValue; const AOperator: TWhereOperator);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FValue := AValue;
  FOperator := AOperator;
end;

function TSimpleExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TSimpleExpression.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LParam: TDBParam;
  LWhere: TSQLWhereField;
  LParamName: string;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LParamName := ACommand.GetAndIncParameterName(FPropertyName);

  LWhere := TSQLWhereField.Create(FPropertyName, GetCriterionTable(ACommand) {ACommand.Table});
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  LWhere.ParamName := LParamName;

  Result := LWhere.ToSQLString(Generator.GetEscapeFieldnameChar); {TODO -oLinas -cGeneral : fix escape fields}
  LParam := TDBParam.Create;
  LParam.SetFromTValue(FValue);
  LParam.Name := LParamName;
  AParams.Add(LParam);

  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

end.
