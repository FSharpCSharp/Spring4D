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
  Core.AbstractManager, Core.EntityMap, Core.Interfaces, Generics.Collections, Rtti, TypInfo
  {$IFDEF USE_SPRING}
  ,Spring.Collections
  {$ENDIF}
  ,Mapping.Attributes;

const
  IID_GETIMPLEMENTOR: TGUID = '{4C12C697-6FE2-4263-A2D8-85034F0D0E01}';

type
  TEntityManager = class(TAbstractManager)
  private
    FOldStateEntities: TEntityMap;
  protected
    procedure SetEntityColumns(AEntity: TObject; AColumns: TList<ColumnAttribute>; AResultset: IDBResultset); overload; virtual;
    procedure SetEntityColumns(AEntity: TObject; AColumns: TList<ManyValuedAssociation>; AResultset: IDBResultset); overload; virtual;
    procedure SetLazyColumns(AEntity: TObject);

    procedure DoSetEntity(var AEntityToCreate: TObject; AResultset: IDBResultset; ARealEntity: TObject); virtual;
    function GetResultset(const ASql: string; const AParams: array of const): IDBResultset;
    function GetOne<T: class, constructor>(AResultset: IDBResultset; AEntity: TObject): T; overload;
    function GetOne(AResultset: IDBResultset; AClass: TClass): TObject; overload;
    function GetObjectList<T: class, constructor>(AResultset: IDBResultset): T;
    procedure SetInterfaceList<T>(var AValue: T; AResultset: IDBResultset);
    procedure SetOne<T>(var AValue: T; AResultset: IDBResultset; AEntity: TObject);
    function DoGetLazy<T>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute; out AIsEnumerable: Boolean): IDBResultset;

    function GetSelector(AClass: TClass): TObject;

    function GetQueryCount(const ASql: string; const AParams: array of const): Int64;
  public
    constructor Create(AConnection: IDBConnection); override;
    destructor Destroy; override;

    function GetLazyValueClass<T: class, constructor>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute): T;
    procedure SetLazyValue<T>(var AValue: T; const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute);

    /// <summary>
    /// Executes sql statement which does not return resultset
    /// </summary>
    function Execute(const ASql: string; const AParams: array of const): NativeUInt;
    /// <summary>
    /// Executes given sql statement and returns first column value. SQL statement should be like this:
    ///  SELECT COUNT(*) FROM TABLE;
    /// </summary>
    function ExecuteScalar<T>(const ASql: string; const AParams: array of const): T;

    /// <summary>
    /// Retrieves first and only model from the sql statement
    /// </summary>
    function First<T: class, constructor>(const ASql: string; const AParams: array of const): T;
    function FirstOrDefault<T: class, constructor>(const ASql: string; const AParams: array of const): T;

    function Single<T: class, constructor>(const ASql: string; const AParams: array of const): T;
    function SingleOrDefault<T: class, constructor>(const ASql: string; const AParams: array of const): T;

    /// <summary>
    /// Retrieves multiple models from the sql statement into the ACollection
    /// </summary>
    procedure Fetch<T: class, constructor>(const ASql: string;
      const AParams: array of const; ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF} ); overload;

    procedure Fetch<T: class, constructor>(AResultset: IDBResultset; ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF}); overload;
    function Fetch<T: class, constructor>(AResultset: IDBResultset): {$IFDEF USE_SPRING} Spring.Collections.IList<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF}; overload;
    /// <summary>
    /// Retrieves multiple models from the sql statement
    /// </summary>
    function Fetch<T: class, constructor>(const ASql: string;
      const AParams: array of const): {$IFDEF USE_SPRING} Spring.Collections.IList<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF}; overload;
    /// <summary>
    /// Retrieves single model from the database based on its primary key value.
    /// If record not found, nil is returned
    /// </summary>
    function FindOne<T: class, constructor>(const AID: TValue): T;
    /// <summary>
    /// Retrieves all models from PODO database table
    /// </summary>
    function FindAll<T: class, constructor>(): {$IFDEF USE_SPRING} Spring.Collections.IList<T>
                                                  {$ELSE} TObjectList<T> {$ENDIF};

    /// <summary>
    /// Inserts model to the database
    /// </summary>
    procedure Insert(AEntity: TObject); overload;
    /// <summary>
    /// Inserts models to the database
    /// </summary>
    procedure Insert<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;
    /// <summary>
    /// Checks if given entity is newly created (does not exist in the database yet)
    /// </summary>
    function IsNew(AEntity: TObject): Boolean;

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
    /// <summary>
    /// Removes models from the database
    /// </summary>
    procedure Delete<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;
    /// <summary>
    /// Fetches data in pages. You do not need to write custom sql for this, just use ordinary sql.
    /// All the work will be done for you.
    /// </summary>
    function Page<T: class, constructor>(APage: Integer; AItemsPerPage: Integer;
      const ASql: string; const AParams: array of const): IDBPage<T>;
    /// <summary>
    /// Saves the entity to the database. It will do update or the insert based on the entity state.
    /// </summary>
    procedure Save(AEntity: TObject); overload;
    /// <summary>
    /// Saves entities to the database. It will do update or the insert based on the entity state.
    /// </summary>
    procedure Save<T: class, constructor>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF}); overload;
  end;


implementation

uses
  SQL.Commands.Insert
  ,SQL.Commands.Select
  ,SQL.Commands.Update
  ,SQL.Commands.Delete
  ,SQL.Commands.Page
  ,SQL.Register
  ,SQL.Interfaces
  ,Core.Exceptions
  ,SQL.Commands.Factory
  ,Mapping.RttiExplorer
  ,Core.Reflection
  ,SQL.Params
  ,Core.Utils
  ,Core.EntityCache
  ,Core.Base
  ,SysUtils
  ;

{ TEntityManager }

constructor TEntityManager.Create(AConnection: IDBConnection);
begin
  inherited Create(AConnection);
  FOldStateEntities := TEntityMap.Create(True);
end;

procedure TEntityManager.Delete(AEntity: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := CommandFactory.GetCommand<TDeleteExecutor>(AEntity.ClassType, Connection);
  try
    LDeleter.EntityClass := AEntity.ClassType;
    LDeleter.Execute(AEntity);
  finally
    LDeleter.Free;
  end;

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
  FOldStateEntities.Free;
  inherited Destroy;
end;

function TEntityManager.DoGetLazy<T>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute; out AIsEnumerable: Boolean): IDBResultset;
var
  LSelecter: TSelectExecutor;
  LBaseEntityClass, LEntityClass: TClass;
  LEnumMethod: TRttiMethod;
begin
  LBaseEntityClass := AEntity.ClassType;
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
  begin
    //we are fetching from the same table - AEntity
    LEntityClass := LBaseEntityClass;
  end;

  LSelecter := GetSelector(LEntityClass) as TSelectExecutor;
  try
    LSelecter.EntityClass := LEntityClass;
   // LSelecter.Connection := Connection;
    LSelecter.ID := AID;
    LSelecter.LazyColumn := AColumn;

    AIsEnumerable := TUtils.IsEnumerable(TypeInfo(T), LEnumMethod);

    if AIsEnumerable then
      LSelecter.SelectType := stObjectList
    else
      LSelecter.SelectType := stOne;

    Result := LSelecter.Select(AEntity, LBaseEntityClass);
  finally
    LSelecter.Free;
  end;
end;

procedure TEntityManager.DoSetEntity(var AEntityToCreate: TObject; AResultset: IDBResultset; ARealEntity: TObject);
var
  LColumns: TList<ColumnAttribute>;
  LResult, LValue: TValue;
  LVal: Variant;
  LObj: TObject;
begin
  {TODO -oLinas -cGeneral : if AEntity class type is not our real Entity type, simply just set value}
  if not TEntityCache.Get(AEntityToCreate.ClassType).IsTableEntity and Assigned(ARealEntity) then
  begin
    if not AResultset.IsEmpty then
    begin
      LVal := AResultset.GetFieldValue(0);
      LValue := TUtils.FromVariant(LVal);

      if TUtils.TryConvert(LValue, Self,
        TRttiExplorer.GetRttiType(AEntityToCreate.ClassType), ARealEntity, LResult) then
      begin
        if AEntityToCreate <> nil then
          FreeAndNil(AEntityToCreate);
        AEntityToCreate := LResult.AsObject;
        if LValue.IsObject then
        begin
          LObj := LValue.AsObject;
          if Assigned(LObj) then
            LObj.Free;
        end;
      end;
    end;
  end
  else
  begin
    LColumns := TEntityCache.GetColumns(AEntityToCreate.ClassType);
    SetEntityColumns(AEntityToCreate, LColumns, AResultset);
    //we need to set internal values for the lazy type field
    SetLazyColumns(AEntityToCreate);
    FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntityToCreate));
  end;
end;

function TEntityManager.Execute(const ASql: string; const AParams: array of const): NativeUInt;
var
  LStatement: IDBStatement;
begin
  LStatement := Connection.CreateStatement;
  LStatement.SetSQLCommand(ASql);
  if Length(AParams) > 0 then
    LStatement.SetParams(AParams);

  Result := LStatement.Execute;
end;

function TEntityManager.ExecuteScalar<T>(const ASql: string; const AParams: array of const): T;
var
  LResults: IDBResultset;
  LVal: Variant;
  LValue: TValue;
begin
  Result := System.Default(T);
  LResults := GetResultset(ASql, AParams);
  if not LResults.IsEmpty then
  begin
    LVal := LResults.GetFieldValue(0);
    LValue := TUtils.FromVariant(LVal);
    Result := LValue.AsType<T>;
  end;
end;

procedure TEntityManager.Fetch<T>(const ASql: string; const AParams: array of const;
  ACollection: {$IFDEF USE_SPRING} ICollection<T> {$ELSE} TObjectList<T> {$ENDIF});
var
  LResults: IDBResultset;
begin
  LResults := GetResultset(ASql, AParams);

  Fetch<T>(LResults, ACollection);
end;

function TEntityManager.Fetch<T>(const ASql: string; const AParams: array of const): {$IFDEF USE_SPRING} Spring.Collections.IList<T>
  {$ELSE} TObjectList<T> {$ENDIF};
begin
  {$IFDEF USE_SPRING}
  Result := TCollections.CreateList<T>(True);
  {$ELSE}
  Result := TObjectList<T>.Create(True);
  {$ENDIF}
  Fetch<T>(ASql, AParams, Result);
end;

procedure TEntityManager.Fetch<T>(AResultset: IDBResultset; ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
  {$ELSE} TObjectList<T> {$ENDIF});
var
  LCurrent: T;
begin
  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne<T>(AResultset, nil);

    ACollection.Add(LCurrent);

    AResultset.Next;
  end;
end;

function TEntityManager.Fetch<T>(AResultset: IDBResultset): {$IFDEF USE_SPRING} Spring.Collections.IList<T>
  {$ELSE} TObjectList<T> {$ENDIF};
begin
  {$IFDEF USE_SPRING}
  Result := TCollections.CreateList<T>(True);
  {$ELSE}
  Result := TObjectList<T>.Create(True);
  {$ENDIF}
  Fetch<T>(AResultset, Result);
end;

function TEntityManager.FindAll<T>: {$IFDEF USE_SPRING} Spring.Collections.IList<T>
  {$ELSE} TObjectList<T> {$ENDIF};
var
  LEntityClass: TClass;
  LSelecter: TSelectExecutor;
  LResults: IDBResultset;
begin
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
  begin
    //we are fetching from the same table - AEntity
    LEntityClass := T;
  end;

  LSelecter := GetSelector(LEntityClass) as TSelectExecutor;
  try
    LSelecter.EntityClass := LEntityClass;
    LSelecter.LazyColumn := nil;
    LResults := LSelecter.SelectAll(nil, LEntityClass);
    Result := Fetch<T>(LResults);
  finally
    LSelecter.Free;
  end;
end;

function TEntityManager.FindOne<T>(const AID: TValue): T;
var
  LSelecter: TSelectExecutor;
  LEntityClass: TClass;
  LResults: IDBResultset;
begin
  Result := System.Default(T);
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
  begin
    //we are fetching from the same table - AEntity
    LEntityClass := T;
  end;

  LSelecter := GetSelector(LEntityClass) as TSelectExecutor;
  try
    LSelecter.EntityClass := LEntityClass;
    LSelecter.ID := AID;
    LSelecter.LazyColumn := nil;
    LResults := LSelecter.Select(nil, LEntityClass);
    if not LResults.IsEmpty then
    begin
      Result := GetOne<T>(LResults, nil);
    end;
  finally
    LSelecter.Free;
  end;
end;

function TEntityManager.First<T>(const ASql: string; const AParams: array of const): T;
var
  LResults: IDBResultset;
begin
  LResults := GetResultset(ASql, AParams);
  if LResults.IsEmpty then
    raise EORMRecordNotFoundException.Create('Query returned 0 records');

  Result := GetOne<T>(LResults, nil);
end;

function TEntityManager.FirstOrDefault<T>(const ASql: string; const AParams: array of const): T;
begin
  try
    Result := First<T>(ASql, AParams);
  except
    Result := System.Default(T);
  end;
end;



procedure TEntityManager.SetInterfaceList<T>(var AValue: T; AResultset: IDBResultset);
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LAddParameters: TArray<TRttiParameter>;
  LValue: TValue;
begin
  if not (PTypeInfo(TypeInfo(T)).Kind = tkInterface) then
    raise EORMUnsupportedType.Create('List must be Spring interface IList<T>.');

  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
    raise EORMUnsupportedType.Create('List must be Spring interface IList<T>.');

  if not TRttiExplorer.TryGetBasicMethod('Add', TypeInfo(T), LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create('Container does not have Add method');


  LAddParameters := LAddMethod.GetParameters;
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create('Container''s Add method must have only one parameter.');

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
    else
      raise EORMContainerItemTypeNotSupported.Create('Container''s items type not supported');
  end;

  LValue := TValue.From<T>(AValue);

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne(AResultset, LEntityClass);

    LAddMethod.Invoke(LValue, [LCurrent]);

    AResultset.Next;
  end;
end;

procedure TEntityManager.SetLazyValue<T>(var AValue: T; const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute);
var
  IsEnumerable: Boolean;
  LResults: IDBResultset;
begin
  case PTypeInfo(TypeInfo(T)).Kind of
    tkClass, tkClassRef, tkPointer, tkRecord, tkUnknown:
    begin
      raise EORMUnsupportedType.CreateFmt('Unsupported type for lazy value: %S.', [string(PTypeInfo(TypeInfo(T)).Name)]);
    end;
  end;

  if not Assigned(AEntity) or AID.IsEmpty then
  begin
    Exit();
  end;

  LResults := DoGetLazy<T>(AID, AEntity, AColumn, IsEnumerable);

  if IsEnumerable then
    SetInterfaceList<T>(AValue, LResults)
  else
    SetOne<T>(AValue, LResults, AEntity);
end;

procedure TEntityManager.SetOne<T>(var AValue: T; AResultset: IDBResultset; AEntity: TObject);
var
  LValue, LConverted: TValue;
  LType: TRttiType;
  LColumn: ColumnAttribute;
  LVal: Variant;
begin
  LType := TRttiExplorer.GetEntityRttiType(TypeInfo(T));
  //{TODO -oLinas -cGeneral : maybe introduce new attribute for specifying simple lazy types. Maybe with SQL parameter}

  if TRttiExplorer.TryGetColumnByMemberName(AEntity.ClassType, LType.Name, LColumn) then
  begin
    if not AResultset.IsEmpty then
    begin
      LVal := AResultset.GetFieldValue(LColumn.Name);
      LValue := TUtils.FromVariant(LVal);
      TRttiExplorer.SetMemberValue(Self, AEntity, LColumn, LValue);
    end;
  end;
end;

function TEntityManager.GetLazyValueClass<T>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute): T;
var
  IsEnumerable: Boolean;
  LResults: IDBResultset;
begin
  if not Assigned(AEntity) or AID.IsEmpty then
    Exit(System.Default(T));


  LResults := DoGetLazy<T>(AID, AEntity, AColumn, IsEnumerable);

  if IsEnumerable then
    Result := GetObjectList<T>(LResults)
  else
    Result := GetOne<T>(LResults, AEntity);
end;

function TEntityManager.GetObjectList<T>(AResultset: IDBResultset): T;
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LProp: TRttiProperty;
  LAddParameters: TArray<TRttiParameter>;
begin
  Result := T.Create;

  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
    LEntityClass := T;

  if not TRttiExplorer.TryGetBasicMethod('Add', TypeInfo(T), LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create('Container does not have Add method');

  LAddParameters := LAddMethod.GetParameters;
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create('Container''s Add method must have only one parameter.');

  if Result.TryGetProperty('OwnsObjects', LProp) then
    LProp.SetValue(TObject(Result), True);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
    else
      raise EORMContainerItemTypeNotSupported.Create('Container''s items type not supported');
  end;

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne(AResultset, LEntityClass);

    LAddMethod.Invoke(Result, [LCurrent]);

    AResultset.Next;
  end;
end;

function TEntityManager.GetOne(AResultset: IDBResultset; AClass: TClass): TObject;
begin
  Result := AClass.Create;
  DoSetEntity(Result, AResultset, nil);
end;

function TEntityManager.GetOne<T>(AResultset: IDBResultset; AEntity: TObject): T;
begin
  Result := T.Create;
  DoSetEntity(TObject(Result), AResultset, AEntity);
end;

function TEntityManager.GetQueryCount(const ASql: string; const AParams: array of const): Int64;
var
  LGenerator: ISQLGenerator;
  LSQL: string;
  LResults: IDBResultset;
begin
  Result := 0;
  LGenerator := TSQLGeneratorRegister.GetGenerator(Connection.GetQueryLanguage);
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

function TEntityManager.GetSelector(AClass: TClass): TObject;
begin
  Result := CommandFactory.GetCommand<TSelectExecutor>(AClass, Connection);
end;

procedure TEntityManager.Insert(AEntity: TObject);
var
  LInserter: TInsertExecutor;
begin
  LInserter := CommandFactory.GetCommand<TInsertExecutor>(AEntity.ClassType, Connection);
  try
    LInserter.EntityClass := AEntity.ClassType;
    LInserter.Execute(AEntity);

    SetLazyColumns(AEntity);
    FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntity));
  finally
    LInserter.Free;
  end;
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

function TEntityManager.IsNew(AEntity: TObject): Boolean;
begin
  Result := not FOldStateEntities.IsMapped(AEntity);
end;

function TEntityManager.Page<T>(APage, AItemsPerPage: Integer; const ASql: string;
  const AParams: array of const): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
begin
  LPager := TPager.Create(Connection);
  Result := TDriverPageAdapter<T>.Create(LPager);
  LPager.Page := APage;
  LPager.ItemsPerPage := AItemsPerPage;
  LPager.TotalItems := GetQueryCount(ASql, AParams);
  LSQL := LPager.BuildSQL(ASql);

  Fetch<T>(LSQL, AParams, Result.Items);
end;

procedure TEntityManager.Save(AEntity: TObject);
begin
  if IsNew(AEntity) then
    Insert(AEntity)
  else
    Update(AEntity);
end;

procedure TEntityManager.Save<T>(ACollection: {$IFDEF USE_SPRING} Spring.Collections.ICollection<T>
      {$ELSE} TObjectList<T> {$ENDIF});
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Save(LEntity);
  end;
end;

procedure TEntityManager.SetEntityColumns(AEntity: TObject; AColumns: TList<ManyValuedAssociation>;
  AResultset: IDBResultset);
var
  LCol: ManyValuedAssociation;
  LVal: Variant;
  LValue: TValue;
begin
  for LCol in AColumns do
  begin
    LVal := AResultset.GetFieldValue(LCol.MappedBy);
    LValue := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, AEntity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TEntityManager.SetLazyColumns(AEntity: TObject);
var
  LCol: ManyValuedAssociation;
  LValue: TValue;
begin
  for LCol in TEntityCache.Get(AEntity.ClassType).ManyValuedColumns do
  begin
    LValue := TRttiExplorer.GetMemberValue(AEntity, LCol.MappedBy); //get foreign key value
    TRttiExplorer.SetMemberValue(Self, AEntity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TEntityManager.SetEntityColumns(AEntity: TObject; AColumns: TList<ColumnAttribute>; AResultset: IDBResultset);
var
  LCol, LPrimaryKeyColumn: ColumnAttribute;
  LVal: Variant;
  LValue, LPrimaryKey: TValue;
  LTypeInfo: PTypeInfo;
begin
  LPrimaryKeyColumn := TEntityCache.Get(AEntity.ClassType).PrimaryKeyColumn;
  //we must set primary key value first
  if LPrimaryKeyColumn <> nil then
  begin
    LVal := AResultset.GetFieldValue(LPrimaryKeyColumn.Name);
    LPrimaryKey := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, AEntity, LPrimaryKeyColumn, LPrimaryKey);
  end;

  for LCol in AColumns do
  begin
    if (cpPrimaryKey in LCol.Properties) then
      Continue;

    LTypeInfo := LCol.GetTypeInfo(AEntity.ClassInfo);
    if (LTypeInfo <> nil) and (TUtils.IsLazyType(LTypeInfo)) then
    begin
      LValue := LPrimaryKey;
    end
    else
    begin
      try
        LVal := AResultset.GetFieldValue(LCol.Name);
      except
        raise EORMColumnNotFound.CreateFmt('Column "%S" not found.', [LCol.Name]);
      end;
      LValue := TUtils.FromVariant(LVal);
    end;

    TRttiExplorer.SetMemberValue(Self, AEntity, LCol, LValue);
  end;
end;

function TEntityManager.Single<T>(const ASql: string; const AParams: array of const): T;
begin
  Result := First<T>(ASql, AParams);
end;

function TEntityManager.SingleOrDefault<T>(const ASql: string; const AParams: array of const): T;
begin
  Result := FirstOrDefault<T>(ASql, AParams);
end;

procedure TEntityManager.Update(AEntity: TObject);
var
  LUpdater: TUpdateExecutor;
begin
  LUpdater := CommandFactory.GetCommand<TUpdateExecutor>(AEntity.ClassType, Connection);
  try
    LUpdater.EntityClass := AEntity.ClassType;
    LUpdater.EntityMap := FOldStateEntities;
    LUpdater.Execute(AEntity);

    SetLazyColumns(AEntity);
    FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntity));
  finally
    LUpdater.Free;
  end;
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
