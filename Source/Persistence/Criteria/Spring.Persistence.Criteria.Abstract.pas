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

unit Spring.Persistence.Criteria.Abstract;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.SQL.Params;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base implementation of <c>ICriteria&lt;T&gt;</c> interface.
  ///	</summary>
  {$ENDREGION}
  TAbstractCriteria<T: class, constructor> = class(TInterfacedObject, ICriteria<T>)
  private
    FEntityClass: TClass;
    FCriterions: IList<ICriterion>;
    FOrders: IList<IOrder>;
    FSession: TSession;
  protected
    constructor Create(ASession: TSession); virtual;

    function GenerateSqlStatement(AParams: IList<TDBParam>): string;
    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>; virtual;
  public
    function Add(ACriterion: ICriterion): ICriteria<T>; virtual;
    function AddOrder(AOrder: IOrder): ICriteria<T>; virtual;

    procedure Clear; virtual;
    function Count: Integer; virtual;
    function ToList: IList<T>;

    property Criterions: IList<ICriterion> read FCriterions;
    property EntityClass: TClass read FEntityClass;
    property Session: TSession read FSession;
  end;

implementation

uses
  SysUtils,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.SQL.Types;

{ TAbstractCriteria }

constructor TAbstractCriteria<T>.Create(ASession: TSession);
begin
  inherited Create;
  FEntityClass := T;
  FSession := ASession;
  FCriterions := TCollections.CreateList<ICriterion>;
  FOrders := TCollections.CreateList<IOrder>;
end;

function TAbstractCriteria<T>.Add(ACriterion: ICriterion): ICriteria<T>;
begin
  if ACriterion.GetEntityClass = nil then
    ACriterion.SetEntityClass(FEntityClass);
  FCriterions.Add(ACriterion);
  Result := Self;
end;

function TAbstractCriteria<T>.AddOrder(AOrder: IOrder): ICriteria<T>;
begin
  FOrders.Add(AOrder);
  Result := Self;
end;

procedure TAbstractCriteria<T>.Clear;
begin
  FCriterions.Clear;
  FOrders.Clear;
end;

function TAbstractCriteria<T>.Count: Integer;
begin
  Result := FCriterions.Count;
end;

function TAbstractCriteria<T>.GenerateSqlStatement(AParams: IList<TDBParam>): string;
var
  LCriterion: ICriterion;
  LWhereField: TSQLWhereField;
  LOrderField: TSQLOrderField;
  LOrder: IOrder;
  LCommand: TSelectCommand;
  LGenerator: ISQLGenerator;
begin
  LCommand := TSelectCommand.Create(FEntityClass);
  LGenerator := TSQLGeneratorRegister.GetGenerator(FSession.Connection.GetQueryLanguage);
  try
    for LCriterion in Criterions do
      LCriterion.ToSqlString(AParams, LCommand, LGenerator, True);

    for LOrder in FOrders do
    begin
      LOrderField := TSQLOrderField.Create(LOrder.GetPropertyName, LCommand.FindTable(LOrder.GetEntityClass));
      LOrderField.OrderType := LOrder.GetOrderType;
      LCommand.OrderByFields.Add(LOrderField);
    end;

    Result := LGenerator.GenerateSelect(LCommand);
  finally
    LCommand.Free;
  end;
end;

function TAbstractCriteria<T>.ToList: IList<T>;
var
  LParams: IList<TDBParam>;
  LSql: string;
begin
  LParams := TCollections.CreateObjectList<TDBParam>(True);
  LSql := GenerateSqlStatement(LParams);
  Result := Session.GetList<T>(LSql, LParams);
end;

function TAbstractCriteria<T>.Page(APage, AItemsPerPage: Integer): IDBPage<T>;
var
  LSql: string;
  LParams: IList<TDBParam>;
begin
  LParams := TCollections.CreateObjectList<TDBParam>;
  LSql := GenerateSqlStatement(LParams);
  Result := FSession.Page<T>(APage, AItemsPerPage, LSql, LParams);
end;

end.


