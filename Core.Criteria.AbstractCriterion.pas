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
unit Core.Criteria.AbstractCriterion;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,SQL.Types
  ,SQL.Params
  ,SQL.Commands
  ,SQL.Interfaces
  ,Generics.Collections
  ;

type
  TAbstractCriterion = class(TInterfacedObject, ICriterion)
  private
    FEntityClass: TClass;
    FGenerator: ISQLGenerator;
    procedure SetEntityClass(const Value: TClass);
    function GetEntityClass: TClass;
  protected
    function GetCriterionTable(ACommand: TDMLCommand): TSQLTable; virtual;
  public
    destructor Destroy; override;

    function ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator): string; virtual;
    function GetMatchMode(): TMatchMode; virtual;
    function GetWhereOperator(): TWhereOperator; virtual;


    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property Generator: ISQLGenerator read FGenerator;
  end;

implementation

uses
  Core.EntityCache
  ,SysUtils
  ;


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

function TAbstractCriterion.ToSqlString(AParams: TObjectList<TDBParam>; ACommand: TDMLCommand;
  AGenerator: ISQLGenerator): string;
begin
  FGenerator := AGenerator;
end;

end.
