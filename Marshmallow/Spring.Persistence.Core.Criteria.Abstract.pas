(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Spring.Persistence.Core.Criteria.Abstract;

interface

{$I sv.inc}

uses
  Spring.Persistence.Core.Interfaces
  ,Spring.Collections
  ,Spring.Persistence.Core.Session
  ,Spring.Persistence.SQL.Params
  ,Rtti
  ;

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
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): ICriteria<T>; virtual;
    function AddOrder(AOrder: IOrder): ICriteria<T>; virtual;

    procedure Clear(); virtual;
    function Count(): Integer; virtual;
    function ToList(): IList<T>;

    property Criterions: IList<ICriterion> read FCriterions;
    property EntityClass: TClass read FEntityClass;
    property Session: TSession read FSession;
  end;

implementation

uses
  SysUtils
  ,Spring.Persistence.SQL.Types
  ,Spring.Persistence.SQL.Commands
  ,Spring.Persistence.SQL.Interfaces
  ,Spring.Persistence.SQL.Register
  ;

{ TAbstractCriteria }

function TAbstractCriteria<T>.Add(ACriterion: ICriterion): ICriteria<T>;
begin
  if (ACriterion.GetEntityClass = nil) then
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

constructor TAbstractCriteria<T>.Create(ASession: TSession);
begin
  inherited Create;
  FEntityClass := T;
  FSession := ASession;
  FCriterions := TCollections.CreateList<ICriterion>();
  FOrders := TCollections.CreateList<IOrder>();
end;

destructor TAbstractCriteria<T>.Destroy;
begin
  inherited Destroy;
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
    begin
      LCriterion.ToSqlString(AParams, LCommand, LGenerator, True);
    end;

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
  LParams := TCollections.CreateObjectList<TDBParam>();
  LSql := GenerateSqlStatement(LParams);
  Result := FSession.Page<T>(APage, AItemsPerPage, LSql, LParams);
end;

end.


