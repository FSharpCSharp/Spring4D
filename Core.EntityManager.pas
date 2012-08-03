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

{$I sv.inc}
interface

uses
  Core.AbstractManager, Core.EntityMap, Core.Interfaces, Generics.Collections, Rtti
  {$IFDEF USE_SPRING}
  ,Spring.Collections
  {$ENDIF}
  ,Mapping.Attributes;

type
  TEntityManager = class(TAbstractManager)
  private
    FEntities: TEntityMap;
    FOldStateEntities: TEntityMap;
  protected
    procedure CascadePersist(AEntity: TObject);

    procedure SetEntityColumns(AEntity: TObject; AColumns: TList<Column>; AResultset: IDBResultset); virtual;

    function GetResultset(const ASql: string; const AParams: array of const): IDBResultset;
    function GetOne<T: class, constructor>(AResultset: IDBResultset): T;

    function GetQueryCount(const ASql: string; const AParams: array of const): Int64;
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

    /// <summary>
    /// Retrieves first and only model from the sql statement
    /// </summary>
    function First<T: class, constructor>(const ASql: string; const AParams: array of const): T;
    function FirstOrDefault<T: class, constructor>(const ASql: string; const AParams: array of const): T;
    /// <summary>
    /// Retrieves multiple models from the sql statement into the ACollection
    /// </summary>
    procedure Fetch<T: class, constructor>(const ASql: string;
      const AParams: array of const; ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF} );

    /// <summary>
    /// Inserts model to the database
    /// </summary>
    procedure Insert(AEntity: TObject); overload;

    procedure Insert<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;

    /// <summary>
    /// Updates model in a database
    /// </summary>
    procedure Update(AEntity: TObject); overload;
    /// <summary>
    /// Updates multiple models in a database
    /// </summary>
    procedure Update<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;
    /// <summary>
    /// Removes model from the database
    /// </summary>
    procedure Delete(AEntity: TObject); overload;

    procedure Delete<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;

    function Page<T: class, constructor>(APage: Integer; AItemsPerPage: Integer;
      const ASql: string; const AParams: array of const): IDBPage<T>;
  end;


implementation

uses
  SQL.Commands.Insert
  ,SQL.Commands.Update
  ,SQL.Commands.Delete
  ,SQL.Commands.Page
  ,SQL.Register
  ,SQL.Interfaces
  ,Core.Exceptions
  ,SQL.Commands.Factory
  ,Mapping.RttiExplorer
  ,SQL.Params
  ,Core.Utils
  ,Core.EntityCache
  ,Core.Base
  ;

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
  FEntities := TEntityMap.Create(False);
  FOldStateEntities := TEntityMap.Create(True);
end;

procedure TEntityManager.Delete(AEntity: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := CommandFactory.GetCommand<TDeleteExecutor>(AEntity.ClassType);

  LDeleter.Connection := Connection;
  LDeleter.EntityClass := AEntity.ClassType;
  LDeleter.Execute(AEntity);

  {TODO -oLinas -cGeneral : remove from entity maps}
 // FEntities.Remove(AEntity);
  FOldStateEntities.Remove(AEntity);
end;

procedure TEntityManager.Delete<T>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF});
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Delete(LEntity);
  end;
end;

destructor TEntityManager.Destroy;
begin
  FEntities.Free;
  FOldStateEntities.Free;
  inherited Destroy;
end;

procedure TEntityManager.Fetch<T>(const ASql: string; const AParams: array of const;
  ACollection: {$IFDEF USE_SPRING} ICollection<T> {$ELSE} TObjectList<T> {$ENDIF});
var
  LResults: IDBResultset;
  LCurrent: T;
begin
  LResults := GetResultset(ASql, AParams);

  while not LResults.IsEmpty do
  begin
    LCurrent := GetOne<T>(LResults);

    ACollection.Add(LCurrent);

    LResults.Next;
  end;
end;

function TEntityManager.Find<T>(const AId: TValue): T;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.FindAll<T>: TObjectList<T>;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.First<T>(const ASql: string; const AParams: array of const): T;
var
  LResults: IDBResultset;
begin
  LResults := GetResultset(ASql, AParams);
  if LResults.IsEmpty then
    raise EORMRecordNotFoundException.Create('Query returned 0 records');

  Result := GetOne<T>(LResults);
end;

function TEntityManager.FirstOrDefault<T>(const ASql: string; const AParams: array of const): T;
begin
  try
    Result := First<T>(ASql, AParams);
  except
    Result := System.Default(T);
  end;
end;

procedure TEntityManager.Flush;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.GetOne<T>(AResultset: IDBResultset): T;
var
  LColumns: TList<Column>;
begin
  Result := T.Create;
  {DONE -oLinas -cGeneral : make a cache for columns. it is not needed to get columns from the class each time}

  LColumns := TEntityCache.GetColumns(TObject(Result).ClassType);
  SetEntityColumns(Result, LColumns, AResultset);

  FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(Result));
end;

function TEntityManager.GetQueryCount(const ASql: string; const AParams: array of const): Int64;
var
  LGenerator: ISQLGenerator;
  LSQL: string;
  LResults: IDBResultset;
begin
  Result := 0;
  LGenerator := TSQLGeneratorRegister.GetCurrentGenerator();
  LSQL := LGenerator.GenerateGetQueryCount(ASql);
  LResults := GetResultset(LSQL, AParams);
  if not LResults.IsEmpty then
  begin
    Result := LResults.GetFieldValue(0);
  end;
end;

function TEntityManager.GetResultset(const ASql: string;
  const AParams: array of const): IDBResultset;
var
  LStmt: IDBStatement;
  LParams: TObjectList<TDBParam>;
begin
  LStmt := Connection.CreateStatement();
  LStmt.SetSQLCommand(ASql);
  LParams := TObjectList<TDBParam>.Create();
  try
    if (Length(AParams) > 0) then
    begin
      ConvertParams(AParams, LParams);
      LStmt.SetParams(LParams);
    end;

    Connection.NotifyExecutionListeners(ASql, LParams);
    Result := LStmt.ExecuteQuery();

  finally
    LParams.Free;
  end;
end;

procedure TEntityManager.Insert(AEntity: TObject);
var
  LInserter: TInsertExecutor;
begin
  LInserter := CommandFactory.GetCommand<TInsertExecutor>(AEntity.ClassType);
  {TODO -oLinas -cGeneral : finish implementing missing methods}
  LInserter.Connection := Connection;
  LInserter.EntityClass := AEntity.ClassType;
  LInserter.Execute(AEntity);

  {TODO -oLinas -cGeneral : add to entity maps}
 // FEntities.Add(AEntity);
  FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntity));
end;

procedure TEntityManager.Insert<T>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF});
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Insert(LEntity);
  end;
end;

function TEntityManager.Merge<T>(AEntity: T): T;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityManager.Page<T>(APage, AItemsPerPage: Integer; const ASql: string;
  const AParams: array of const): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
begin
  LPager := TPager.Create();
  Result := TDriverPageAdapter<T>.Create(LPager);

  LPager.Connection := Connection;
  LPager.Page := APage;
  LPager.ItemsPerPage := AItemsPerPage;
  LPager.TotalItems := GetQueryCount(ASql, AParams);
  LSQL := LPager.BuildSQL(ASql);

  Fetch<T>(LSQL, AParams, Result.Items);
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

  try
    //if TRttiExplorer.HasSequence(Entity.ClassType{, True}) then
     // LInserter.LoadIdFromSequence(Entity);
    LInserter.Execute(AEntity);
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

procedure TEntityManager.SetEntityColumns(AEntity: TObject; AColumns: TList<Column>; AResultset: IDBResultset);
var
  LCol: Column;
  LVal: Variant;
begin
  for LCol in AColumns do
  begin
    LVal := AResultset.GetFieldValue(LCol.Name);

    TRttiExplorer.SetMemberValue(AEntity, LCol, TUtils.FromVariant(LVal));
  end;
end;

procedure TEntityManager.Update(AEntity: TObject);
var
  LUpdater: TUpdateExecutor;
begin
  LUpdater := CommandFactory.GetCommand<TUpdateExecutor>(AEntity.ClassType);
  {TODO -oLinas -cGeneral : finish implementing missing methods}
  LUpdater.Connection := Connection;
  LUpdater.EntityClass := AEntity.ClassType;
  LUpdater.EntityMap := FOldStateEntities;
  LUpdater.Execute(AEntity);

  {TODO -oLinas -cGeneral : update entity maps}
 // FEntities.Add(AEntity);
  FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntity));
end;

procedure TEntityManager.Update<T>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF});
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Update(LEntity);
  end;
end;

end.
