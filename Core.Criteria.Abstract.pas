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
unit Core.Criteria.Abstract;

interface

{$I sv.inc}

uses
  Core.Interfaces
  {$IFDEF USE_SPRING},Spring.Collections{$ENDIF}
  ,Generics.Collections
  ,Core.Session
  ,SQL.Params
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
    FCriterions: TList<ICriterion>;
    FOrders: TList<IOrder>;
    FSession: TSession;
  protected
    constructor Create(AEntityClass: TClass; ASession: TSession); virtual;

    function GenerateSqlStatement(AParams: TObjectList<TDBParam>): string;
    function CreateList(): {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF}; virtual;
    procedure DoFetch(const ACollection: TValue); virtual;
    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>; virtual;
  public
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): ICriteria<T>; virtual;
    function AddOrder(AOrder: IOrder): ICriteria<T>; virtual;

    procedure Clear(); virtual;
    function Count(): Integer; virtual;
    function List(): {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
    procedure Fetch(const ACollection: TValue);

    property Criterions: TList<ICriterion> read FCriterions;
    property EntityClass: TClass read FEntityClass;
    property Session: TSession read FSession;
  end;

implementation

uses
  SysUtils
  ,SQL.Commands.Select
  ,SQL.Commands.Factory
  ,SQL.Types
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

constructor TAbstractCriteria<T>.Create(AEntityClass: TClass; ASession: TSession);
begin
  inherited Create;
  FEntityClass := AEntityClass;
  FSession := ASession;
  FCriterions := TList<ICriterion>.Create();
  FOrders := TList<IOrder>.Create();
end;

destructor TAbstractCriteria<T>.Destroy;
begin
  FCriterions.Free;
  FOrders.Free;
  inherited Destroy;
end;

procedure TAbstractCriteria<T>.DoFetch(const ACollection: TValue);
var
  LParams: TObjectList<TDBParam>;
  LSql: string;
  LResults: IDBResultset;
begin
  LParams := TObjectList<TDBParam>.Create(True);
  try
    LSql := GenerateSqlStatement(LParams);
    LResults := Session.GetResultset(LSql, LParams);
    Session.Fetch<T>(LResults, ACollection);
  finally
    LParams.Free;
  end;
end;

function TAbstractCriteria<T>.CreateList: {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
begin
  {$IFDEF USE_SPRING}
  Result := TCollections.CreateList<T>(True);
  {$ELSE}
  Result := TObjectList<T>.Create(True);
  {$ENDIF}
end;

procedure TAbstractCriteria<T>.Fetch(const ACollection: TValue);
begin
  DoFetch(ACollection);
end;

function TAbstractCriteria<T>.GenerateSqlStatement(AParams: TObjectList<TDBParam>): string;
var
  LCriterion: ICriterion;
  LExecutor: TSelectExecutor;
  LWhereField: TSQLWhereField;
  LOrderField: TSQLOrderField;
  LOrder: IOrder;
begin
  LExecutor := CommandFactory.GetCommand<TSelectExecutor>(FEntityClass, FSession.Connection);
  try
    LExecutor.EntityClass := FEntityClass;
    LExecutor.LazyColumn := nil;

    for LCriterion in Criterions do
    begin
      LCriterion.ToSqlString(AParams, LExecutor.Command, LExecutor.Generator, True);
    end;

    for LOrder in FOrders do
    begin
      LOrderField := TSQLOrderField.Create(LOrder.GetPropertyName, LExecutor.Command.FindTable(LOrder.GetEntityClass));
      LOrderField.OrderType := LOrder.GetOrderType;
      LExecutor.Command.OrderByFields.Add(LOrderField);
    end;

    Result := LExecutor.Generator.GenerateSelect(LExecutor.Command);
  finally
    LExecutor.Free;
  end;
end;

function TAbstractCriteria<T>.List: {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
var
  LCollection: TValue;
begin
  Result := CreateList();
  LCollection := TValue.From<{$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF}>(Result);
  DoFetch(LCollection);
end;

function TAbstractCriteria<T>.Page(APage, AItemsPerPage: Integer): IDBPage<T>;
var
  LSql: string;
  LParams: TObjectList<TDBParam>;
begin
  LParams := TObjectList<TDBParam>.Create();
  try
    LSql := GenerateSqlStatement(LParams);
    Result := FSession.Page<T>(APage, AItemsPerPage, LSql, LParams);
  finally
    LParams.Free;
  end;
end;

end.
