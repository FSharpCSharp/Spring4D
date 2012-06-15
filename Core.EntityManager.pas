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
unit Core.EntityManager;

interface

uses
  Core.AbstractManager, Core.EntityMap, Core.Interfaces, Generics.Collections, Rtti;

type
  TEntityManager = class(TAbstractManager)
  private
    FEntities: TEntityMap;
    FOldStateEntities: TEntityMap;
  protected
    procedure CascadePersist(AEntity: TObject);
  public
    constructor Create(AConnection: IDBConnection); override;
    destructor Destroy; override;

    procedure Persist(AEntity: TObject);
    function Merge<T>(AEntity: T): T;
    procedure Remove(AEntity: TObject);
    function Find<T>(const AId: TValue): T;
    function FindAll<T: class>(): TObjectList<T>;
    procedure Flush();
    procedure Clear();

  end;


implementation

uses
  SQL.Commands.Insert,
  Core.Exceptions,
  SQL.Commands.Factory,
  Mapping.RttiExplorer;

{ TEntityManager }

procedure TEntityManager.CascadePersist(AEntity: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

procedure TEntityManager.Clear;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

constructor TEntityManager.Create(AConnection: IDBConnection);
begin
  inherited Create(AConnection);
  FEntities := TEntityMap.Create;
  FOldStateEntities := TEntityMap.Create;
end;

destructor TEntityManager.Destroy;
begin
  FEntities.Free;
  FOldStateEntities.Free;
  inherited Destroy;
end;

function TEntityManager.Find<T>(const AId: TValue): T;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.FindAll<T>: TObjectList<T>;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

procedure TEntityManager.Flush;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.Merge<T>(AEntity: T): T;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

procedure TEntityManager.Persist(AEntity: TObject);
var
  LInserter: TInsertExecutor;
begin
  if FEntities.IsMapped(AEntity) then
    raise EEntityAlreadyPersisted.Create(AEntity);

  if FEntities.HasIdValue(AEntity) then
    raise ECannotPersististEntityWithId.Create(AEntity);

  CascadePersist(AEntity);

  LInserter := CommandFactory.GetCommand<TInsertExecutor>(AEntity.ClassType);
  {TODO -oLinas -cGeneral : finish implementing missing methods}
  try
    //if TRttiExplorer.HasSequence(Entity.ClassType{, True}) then
     // LInserter.LoadIdFromSequence(Entity);
    LInserter.Insert(AEntity);
    FEntities.Add(AEntity);
    FOldStateEntities.Add(TRttiExplorer.Clone(AEntity));
  finally
    LInserter.Free;
  end;
end;

procedure TEntityManager.Remove(AEntity: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
