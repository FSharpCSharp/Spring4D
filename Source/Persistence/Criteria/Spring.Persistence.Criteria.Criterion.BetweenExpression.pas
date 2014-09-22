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

unit Spring.Persistence.Criteria.Criterion.BetweenExpression;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TBetweenExpression = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FLowValue: TValue;
    FOperator: TWhereOperator;
    FHighValue: TValue;
  public
    constructor Create(const APropertyName: string; const ALowValue, AHighValue: TValue; const AOperator: TWhereOperator); virtual;
  public
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
    function GetWhereOperator: TWhereOperator; override;

    property PropertyName: string read FPropertyName;
    property LowValue: TValue read FLowValue;
    property HighValue: TValue read FHighValue write FHighValue;
  end;

implementation

{ TBetweenExpression }

constructor TBetweenExpression.Create(const APropertyName: string; const ALowValue,
  AHighValue: TValue; const AOperator: TWhereOperator);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FLowValue := ALowValue;
  FHighValue := AHighValue;
  FOperator := AOperator;
end;

function TBetweenExpression.GetWhereOperator: TWhereOperator;
begin
  Result := FOperator;
end;

function TBetweenExpression.ToSqlString(AParams: IList<TDBParam>;
  ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LParam: TDBParam;
  LWhere: TSQLWhereField;
  LParamName, LParamName2: string;
begin
  Assert(ACommand is TWhereCommand);
  inherited;
  LParamName := ACommand.GetAndIncParameterName(FPropertyName);
  LParamName2 := ACommand.GetAndIncParameterName(FPropertyName);
  LWhere := TSQLWhereField.Create(FPropertyName, GetCriterionTable(ACommand) );
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;
  LWhere.ParamName := LParamName;
  LWhere.ParamName2 := LParamName2;

  Result := LWhere.ToSQLString(AGenerator.GetEscapeFieldnameChar); {TODO -oLinas -cGeneral : fix escape fields}

  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;

  //1st parameter Low
  LParam := TDBParam.Create;
  LParam.SetFromTValue(FLowValue);
  LParam.Name := LParamName;
  AParams.Add(LParam);
  //2nd parameter High
  LParam := TDBParam.Create;
  LParam.SetFromTValue(FHighValue);
  LParam.Name := LParamName2;
  AParams.Add(LParam);
end;

end.
