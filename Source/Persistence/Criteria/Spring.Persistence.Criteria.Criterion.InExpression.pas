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

unit Spring.Persistence.Criteria.Criterion.InExpression;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TInExpression<T> = class(TSimpleExpression)
  private
    FValues: TArray<T>;
  protected
    function ValuesToSeparatedString: string;
  public
    constructor Create(const APropertyName: string; const AValues: TArray<T>; AOperator: TWhereOperator); reintroduce; overload;

    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; override;
  end;

implementation

uses
  SysUtils,
  TypInfo;

{ TInExpression<T> }

constructor TInExpression<T>.Create(const APropertyName: string; const AValues: TArray<T>;
  AOperator: TWhereOperator);
begin
  inherited Create(APropertyName, TValue.Empty, AOperator);
  FValues := AValues;
end;

function TInExpression<T>.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
var
  LWhere: TSQLWhereField;
begin
  Assert(ACommand is TWhereCommand);

  Result := Format('%S %S (%S)',
    [PropertyName, WhereOpNames[GetWhereOperator], ValuesToSeparatedString]);

  LWhere := TSQLWhereField.Create(Result, GetCriterionTable(ACommand));
  LWhere.MatchMode := GetMatchMode;
  LWhere.WhereOperator := GetWhereOperator;

  if AAddToCommand then
    TWhereCommand(ACommand).WhereFields.Add(LWhere)
  else
    LWhere.Free;
end;

function TInExpression<T>.ValuesToSeparatedString: string;
var
  LCurrent: T;
  i: Integer;
  LValue: TValue;
  LStringValue: string;
begin
  Result := 'NULL';
  i := 0;
  for LCurrent in FValues do
  begin
    if i = 0 then
      Result := ''
    else
      Result := Result + ',';

    LValue := TValue.From<T>(LCurrent);
    case LValue.Kind of
      tkChar, tkWChar, tkLString, tkWString, tkUString, tkString:
        LStringValue := QuotedStr(LValue.AsString)
      else
        LStringValue := LValue.ToString;
    end;
    Result := Result + LStringValue;
    Inc(i);
  end;
end;

end.
