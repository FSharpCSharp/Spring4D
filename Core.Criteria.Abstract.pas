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
  ;

type
  TAbstractCriteria<T: class, constructor> = class(TInterfacedObject, ICriteria<T>)
  private
    FEntityClass: TClass;
    FCriterions: TList<ICriterion>;
    FSession: TSession;
    FParamIndex: Integer;
  protected
    constructor Create(AEntityClass: TClass; ASession: TSession); virtual;

    function GenerateSqlStatement(out AParams: TObjectList<TDBParam>): string;
    function GetParamName(): string;
    function DoList(): {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF}; virtual; abstract;
  public
    destructor Destroy; override;

    function Add(ACriterion: ICriterion): ICriteria<T>; virtual;
    function Count(): Integer; virtual;
    function List(): {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};

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
  FParamIndex := 0;
  ACriterion.SetEntityClass(FEntityClass);
  FCriterions.Add(ACriterion);
  Result := Self;
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
  FParamIndex := 0;
end;

destructor TAbstractCriteria<T>.Destroy;
begin
  FCriterions.Free;
  inherited Destroy;
end;

function TAbstractCriteria<T>.GenerateSqlStatement(out AParams: TObjectList<TDBParam>): string;
var
  LCriterion: ICriterion;
  LCriterionSql: string;
  LExecutor: TSelectExecutor;
  LWhereField: TSQLWhereField;
begin
  LExecutor := CommandFactory.GetCommand<TSelectExecutor>(FEntityClass, FSession.Connection);
  try
    LExecutor.EntityClass := FEntityClass;
    LExecutor.LazyColumn := nil;

    AParams := TObjectList<TDBParam>.Create();
    for LCriterion in Criterions do
    begin
      LCriterionSql := LCriterion.ToSqlString(AParams);
      LWhereField := TSQLWhereField.Create(LCriterionSql, LExecutor.Command.Table);
      LWhereField.WhereOperator := LCriterion.GetWhereOperator;
      LExecutor.Command.WhereFields.Add(LWhereField);
    end;


    Result := LExecutor.Generator.GenerateSelect(LExecutor.Command);

  finally
    LExecutor.Free;
  end;
end;

function TAbstractCriteria<T>.GetParamName: string;
begin
  Result := Format(':%D', [FParamIndex]);
  Inc(FParamIndex);
end;

function TAbstractCriteria<T>.List: {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
begin
  FParamIndex := 0;
  Result := DoList();
end;

end.
