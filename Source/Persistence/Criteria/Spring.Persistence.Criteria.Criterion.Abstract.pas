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

unit Spring.Persistence.Criteria.Criterion.Abstract;

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
  TAbstractCriterion = class(TInterfacedObject, ICriterion)
  private
    FEntityClass: TClass;
    FGenerator: ISQLGenerator;
    procedure SetEntityClass(const Value: TClass);
    function GetEntityClass: TClass;
  protected
    function GetCriterionTable(ACommand: TDMLCommand): TSQLTable; overload; virtual;
    function GetCriterionTable(ACommand: TDMLCommand; ATable: TSQLTable): TSQLTable; overload; virtual;
  public
    destructor Destroy; override;

    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string; virtual;
    function GetMatchMode: TMatchMode; virtual;
    function GetWhereOperator: TWhereOperator; virtual;

    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property Generator: ISQLGenerator read FGenerator;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache;

{ TAbstractCriterion }

destructor TAbstractCriterion.Destroy;
begin
  FGenerator := nil;
  inherited Destroy;
end;

function TAbstractCriterion.GetCriterionTable(ACommand: TDMLCommand): TSQLTable;
begin
  Result := ACommand.Table;
  if (ACommand is TSelectCommand) then
  begin
    Result := TSelectCommand(ACommand).FindTable(EntityClass);
  end;
end;

function TAbstractCriterion.GetCriterionTable(ACommand: TDMLCommand; ATable: TSQLTable): TSQLTable;
begin
  Result := ATable;
  if (ACommand is TSelectCommand) then
  begin
    Result := TSelectCommand(ACommand).FindCorrespondingTable(ATable);
  end;
end;

function TAbstractCriterion.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TAbstractCriterion.GetMatchMode: TMatchMode;
begin
  Result := mmExact;
end;

function TAbstractCriterion.GetWhereOperator: TWhereOperator;
begin
  Result := woEqual;
end;

procedure TAbstractCriterion.SetEntityClass(const Value: TClass);
begin
  FEntityClass := Value;
end;

function TAbstractCriterion.ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand;
  AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
begin
  FGenerator := AGenerator;
end;

end.
