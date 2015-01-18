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

{$I Spring.inc}

unit Spring.Persistence.Criteria.Abstract;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.SQL.Params;

type
  /// <summary>
  ///   Base implementation of <see cref="Spring.Persistence.Core.Interfaces|ICriteria&lt;T&gt;" />
  ///    interface.
  /// </summary>
  TAbstractCriteria<T: class, constructor> = class(TInterfacedObject, ICriteria<T>)
  private
    fEntityClass: TClass;
    fCriterions: IList<ICriterion>;
    fOrderBy: IList<IOrderBy>;
    FSession: TSession;
  protected
    constructor Create(const session: TSession); virtual;

    function GenerateSqlStatement(const params: IList<TDBParam>): string;
    function Page(page, itemsPerPage: Integer): IDBPage<T>; virtual;
  public
    function Add(const criterion: ICriterion): ICriteria<T>; virtual;
    function Where(const criterion: ICriterion): ICriteria<T>; virtual;
    function OrderBy(const orderBy: IOrderBy): ICriteria<T>; virtual;

    procedure Clear; virtual;
    function Count: Integer; virtual;
    function ToList: IList<T>;

    property Criterions: IList<ICriterion> read fCriterions;
    property EntityClass: TClass read fEntityClass;
    property Session: TSession read FSession;
  end;

implementation

uses
  SysUtils,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.SQL.Types;


{$REGION 'TAbstractCriteria'}

constructor TAbstractCriteria<T>.Create(const session: TSession);
begin
  inherited Create;
  fEntityClass := T;
  FSession := session;
  fCriterions := TCollections.CreateList<ICriterion>;
  fOrderBy := TCollections.CreateList<IOrderBy>;
end;

function TAbstractCriteria<T>.Add(const criterion: ICriterion): ICriteria<T>;
begin
  if criterion.GetEntityClass = nil then
    criterion.SetEntityClass(fEntityClass);
  fCriterions.Add(criterion);
  Result := Self;
end;

procedure TAbstractCriteria<T>.Clear;
begin
  fCriterions.Clear;
  fOrderBy.Clear;
end;

function TAbstractCriteria<T>.Count: Integer;
begin
  Result := fCriterions.Count;
end;

function TAbstractCriteria<T>.GenerateSqlStatement(const params: IList<TDBParam>): string;
var
  LCriterion: ICriterion;
  LWhereField: TSQLWhereField;
  LOrderField: TSQLOrderByField;
  LOrderBy: IOrderBy;
  LCommand: TSelectCommand;
  LGenerator: ISQLGenerator;
begin
  LCommand := TSelectCommand.Create(fEntityClass);
  LGenerator := TSQLGeneratorRegister.GetGenerator(FSession.Connection.GetQueryLanguage);
  try
    for LCriterion in Criterions do
      LCriterion.ToSqlString(params, LCommand, LGenerator, True);

    for LOrderBy in fOrderBy do
    begin
      LOrderField := TSQLOrderByField.Create(LOrderBy.GetPropertyName,
        LCommand.FindTable(LOrderBy.GetEntityClass));
      LOrderField.SortingDirection := LOrderBy.GetSortingDirection;
      LCommand.OrderByFields.Add(LOrderField);
    end;

    Result := LGenerator.GenerateSelect(LCommand);
  finally
    LCommand.Free;
  end;
end;

function TAbstractCriteria<T>.OrderBy(const orderBy: IOrderBy): ICriteria<T>;
begin
  fOrderBy.Add(orderBy);
  Result := Self;
end;

function TAbstractCriteria<T>.Page(page, itemsPerPage: Integer): IDBPage<T>;
var
  LSql: string;
  LParams: IList<TDBParam>;
begin
  LParams := TCollections.CreateObjectList<TDBParam>;
  LSql := GenerateSqlStatement(LParams);
  Result := FSession.Page<T>(page, itemsPerPage, LSql, LParams);
end;

function TAbstractCriteria<T>.ToList: IList<T>;
var
  LParams: IList<TDBParam>;
  LSql: string;
begin
  LParams := TCollections.CreateObjectList<TDBParam>(True);
  LSql := GenerateSqlStatement(LParams);
  Result := TCollections.CreateObjectList<T>(True);
  Result := Session.GetList<T>(LSql, LParams);
end;

function TAbstractCriteria<T>.Where(const criterion: ICriterion): ICriteria<T>;
begin
  Result := Add(criterion);
end;

end.
