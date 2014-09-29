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
    fValues: TArray<T>;
    function ValuesToSeparatedString: string;
  protected
    function ToSqlString(const params: IList<TDBParam>;
      const command: TDMLCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string; const values: TArray<T>;
      whereOperator: TWhereOperator); reintroduce; overload;
  end;

implementation

uses
  SysUtils,
  TypInfo;


{$REGION 'TInExpression<T>'}

constructor TInExpression<T>.Create(const propertyName: string;
  const values: TArray<T>; whereOperator: TWhereOperator);
begin
  inherited Create(propertyName, TValue.Empty, whereOperator);
  fValues := values;
end;

function TInExpression<T>.ToSqlString(const params: IList<TDBParam>;
  const command: TDMLCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField: TSQLWhereField;
begin
  Assert(command is TWhereCommand);

  Result := Format('%S %S (%S)',
    [PropertyName, WhereOpNames[GetWhereOperator], ValuesToSeparatedString]);

  whereField := TSQLWhereField.Create(Result, GetCriterionTable(command));
  whereField.MatchMode := GetMatchMode;
  whereField.WhereOperator := GetWhereOperator;

  if addToCommand then
    TWhereCommand(command).WhereFields.Add(whereField)
  else
    whereField.Free;
end;

function TInExpression<T>.ValuesToSeparatedString: string;
var
  isFirst: Boolean;
  item: T;
  value: TValue;
  s: string;
begin
  Result := 'NULL';
  isFirst := True;
  for item in fValues do
  begin
    if isFirst then
    begin
      Result := '';
      isFirst := False;
    end
    else
      Result := Result + ',';

    value := TValue.From<T>(item);
    case value.Kind of
      tkChar, tkWChar, tkLString, tkWString, tkUString, tkString:
        s := QuotedStr(value.AsString)
    else
      s := value.ToString;
    end;
    Result := Result + s;
  end;
end;

{$ENDREGION}


end.
