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

unit Spring.Persistence.Core.AbstractSession;

interface

uses
  Rtti,
  TypInfo,
  Spring.Collections,
  Spring.Persistence.Core.AbstractManager,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update,
  Spring.Persistence.SQL.Params;

type
  TAbstractSession = class(TAbstractManager)
  protected
    procedure SetEntityColumns(const entity: TObject;
      const columns: TColumnDataList; const resultSet: IDBResultSet); overload; virtual;
    procedure SetEntityColumns(const entity: TObject;
      const columns: IList<ManyValuedAssociation>; const resultSet: IDBResultSet); overload; virtual;
    procedure SetLazyColumns(const entity: TObject; const entityData: TEntityData);
    procedure SetAssociations(const entity: TObject;
      const resultSet: IDBResultSet; const entityData: TEntityData); virtual;

    procedure DoSetEntity(var entityToCreate: TObject;
      const resultSet: IDBResultSet; const realEntity: TObject); virtual;
    procedure DoSetEntityValues(const entityToCreate: TObject;
      const resultSet: IDBResultSet; const columns: TColumnDataList;
      const entityData: TEntityData); virtual;
    procedure DoFetch<T: class, constructor>(const resultSet: IDBResultSet;
      const collection: TValue);

    function GetOne<T: class, constructor>(const resultSet: IDBResultSet;
      const entity: TObject): T; overload;
    function GetOne(const resultSet: IDBResultSet; classType: TClass): TObject; overload;
    function GetObjectList<T: class, constructor>(const resultSet: IDBResultSet): T;

    procedure SetInterfaceList<T>(var value: T; const resultSet: IDBResultSet); overload;
    procedure SetInterfaceList(var value: IInterface;
      const resultSet: IDBResultSet; classInfo: PTypeInfo); overload;
    procedure SetSimpleInterfaceList(var value: IInterface;
      const resultSet: IDBResultSet; classInfo: PTypeInfo);
    procedure SetOne<T>(var value: T; const resultSet: IDBResultSet;
      const entity: TObject);

    function DoGetLazy<T>(const id: TValue; const entity: TObject;
      const column: ColumnAttribute): IDBResultSet;

    /// <summary>
    ///   Retrieves multiple models from the <c>resultset</c>.
    /// </summary>
    function GetListFromResultset<T: class, constructor>(
      const resultSet: IDBResultSet): IList<T>;
    function GetList<T: class, constructor>(const query: string;
      const params: IList<TDBParam>): IList<T>; overload;

    function GetResultsetById(entityClass: TClass; const id: TValue; foreignEntityClass: TClass = nil; const selectColumn: ColumnAttribute = nil): IDBResultSet;

    procedure AttachEntity(const entity: TObject); virtual; abstract;
    procedure DetachEntity(const entity: TObject); virtual; abstract;

    procedure DoInsert(const entity, executor: TObject); virtual;
    procedure DoUpdate(const entity, executor: TObject); virtual;
    procedure DoDelete(const entity, executor: TObject); virtual;

    function GetInsertCommandExecutor(entityClass: TClass): TInsertExecutor; virtual;
    function GetUpdateCommandExecutor(entityClass: TClass): TUpdateExecutor; virtual;
    function GetSelectCommandExecutor(entityClass: TClass): TSelectExecutor; virtual;
    function GetSelectByIdCommandExecutor(entityClass: TClass; const id: TValue; const selectColumn: ColumnAttribute = nil): TSelectExecutor; virtual;
    function GetDeleteCommandExecutor(entityClass: TClass): TDeleteExecutor; virtual;
  public
    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultset(const sqlStatement: string;
      const params: array of const): IDBResultSet; overload;

    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultset(const sqlStatement: string;
      const params: IList<TDBParam>): IDBResultSet; overload;

    /// <summary>
    ///   Retrieves multiple models from the sql statement into the Collection
    ///   ( <c>TObjectList&lt;T&gt;</c> or Spring <c>ICollection&lt;T&gt;</c>).
    /// </summary>
    procedure Fetch<T: class, constructor>(const sqlStatement: string;
      const params: array of const; const collection: ICollection<T>); overload;
    /// <summary>
    ///   Retrieves multiple models from the <c>Resultset</c> into the
    ///   Collection (<c>TObjectList&lt;T&gt;</c> or Spring <c>
    ///   ICollection&lt;T&gt;).</c>
    /// </summary>
    procedure Fetch<T: class, constructor>(const resultSet: IDBResultSet;
      const collection: ICollection<T>); overload;

    /// <summary>
    ///   Retrieves multiple models from the <c>Resultset</c> into the any
    ///   Collection. Collection must contain <c>Add</c> method with single
    ///   parameter.
    /// </summary>
    procedure Fetch<T: class, constructor>(const resultSet: IDBResultSet;
      const collection: TValue); overload;

    function GetLazyValueClass<T: class, constructor>(const id: TValue;
      const entity: TObject; const column: ColumnAttribute): T;
    procedure SetLazyValue<T>(var value: T; const id: TValue;
      const entity: TObject; const column: ColumnAttribute);
  end;

implementation

uses
  Spring,
  Spring.Persistence.Core.CollectionAdapterResolver,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TAbstractSession'}

procedure TAbstractSession.DoDelete(const entity, executor: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := executor as TDeleteExecutor;
  LDeleter.Execute(entity);
  DetachEntity(entity);
end;

procedure TAbstractSession.DoFetch<T>(const resultset: IDBResultSet;
  const collection: TValue);
var
  LCurrent: T;
  LCollectionAdapter: ICollectionAdapter<T>;
begin
  LCollectionAdapter := TCollectionAdapterResolver.Resolve<T>(collection);
  if not LCollectionAdapter.IsAddSupported then
    raise EORMContainerDoesNotHaveAddMethod.Create('Container does not have "Add" method.');

  while not resultSet.IsEmpty do
  begin
    LCurrent := GetOne<T>(resultSet, nil);
    LCollectionAdapter.Add(LCurrent);
    resultSet.Next;
  end;
end;

function TAbstractSession.DoGetLazy<T>(const id: TValue; const entity: TObject;
  const column: ColumnAttribute): IDBResultSet;
var
  LBaseEntityClass, LEntityToLoadClass: TClass;
begin
  LBaseEntityClass := entity.ClassType;
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityToLoadClass) then
    LEntityToLoadClass := LBaseEntityClass; // we are fetching from the same table - AEntity

  if LEntityToLoadClass = LBaseEntityClass then
    LBaseEntityClass := nil;

  Result := GetResultsetById(LEntityToLoadClass, id, LBaseEntityClass, column);
end;

procedure TAbstractSession.DoInsert(const entity, executor: TObject);
var
  LInserter: TInsertExecutor;
begin
  LInserter := executor as TInsertExecutor;
  LInserter.Execute(entity);
  SetLazyColumns(entity, TEntityCache.Get(entity.ClassType));
  AttachEntity(entity);
end;

procedure TAbstractSession.DoSetEntity(var entityToCreate: TObject;
  const resultSet: IDBResultSet; const realEntity: TObject);
var
  LEntityData: TEntityData;
  LResult, LValue: TValue;
  LVal: Variant;
begin
  LEntityData := TEntityCache.Get(entityToCreate.ClassType);
  {TODO -oLinas -cGeneral : if entityToCreate class type is not our annotated ORM Entity type (it can be e.g. TPicture, TStream, etc.), simply just set value}
  if not LEntityData.IsTableEntity and Assigned(realEntity) then
  begin
    if not resultSet.IsEmpty then
    begin
      LVal := resultSet.GetFieldValue(0);
      LValue := TUtils.FromVariant(LVal);

      if TUtils.TryConvert(LValue, Self,
        TRttiExplorer.GetRttiType(entityToCreate.ClassType), realEntity, LResult) then
      begin
        if entityToCreate <> nil then
          entityToCreate.Free;
        entityToCreate := LResult.AsObject;
        FreeValueObject(LValue);
      end;
    end;
  end
  else
  begin
    DoSetEntityValues(entityToCreate, resultSet, LEntityData.ColumnsData, LEntityData);
  end;
end;

procedure TAbstractSession.DoSetEntityValues(const entityToCreate: TObject;
  const resultSet: IDBResultSet; const columns: TColumnDataList;
  const entityData: TEntityData);
var
  LEntityData: TEntityData;
begin
  SetEntityColumns(entityToCreate, columns, resultSet);
  //we need to set internal values for the lazy type field
  LEntityData := entityData;
  if entityToCreate.ClassType <> entityData.EntityClass then
    LEntityData := TEntityCache.Get(entityToCreate.ClassType);

  SetLazyColumns(entityToCreate, LEntityData);

  SetAssociations(entityToCreate, resultSet, LEntityData);

  AttachEntity(entityToCreate);
end;

procedure TAbstractSession.DoUpdate(const entity, executor: TObject);
var
  LUpdater: TUpdateExecutor;
begin
  LUpdater := executor as TUpdateExecutor;
  LUpdater.Execute(entity);
  SetLazyColumns(entity, TEntityCache.Get(entity.ClassType));
  AttachEntity(entity);
end;

procedure TAbstractSession.Fetch<T>(const sqlStatement: string;
  const params: array of const; const collection: ICollection<T>);
var
  LResults: IDBResultSet;
begin
  LResults := GetResultset(sqlStatement, params);
  Fetch<T>(LResults, collection);
end;

procedure TAbstractSession.Fetch<T>(const resultSet: IDBResultSet;
  const collection: ICollection<T>);
var
  LCollection: TValue;
begin
  LCollection := TValue.From(collection);
  DoFetch<T>(resultSet, LCollection);
end;

procedure TAbstractSession.Fetch<T>(const resultSet: IDBResultSet;
  const collection: TValue);
begin
  DoFetch<T>(resultSet, collection);
end;

function TAbstractSession.GetDeleteCommandExecutor(
  entityClass: TClass): TDeleteExecutor;
begin
  Result := TDeleteExecutor.Create;
  Result.Connection := Connection;
  Result.EntityClass := entityClass;
  Result.Build(entityClass);
end;

function TAbstractSession.GetInsertCommandExecutor(
  entityClass: TClass): TInsertExecutor;
begin
  Result := TInsertExecutor.Create;
  Result.Connection := Connection;
  Result.EntityClass := entityClass;
  Result.Build(entityClass);
end;

function TAbstractSession.GetLazyValueClass<T>(const id: TValue;
  const entity: TObject; const column: ColumnAttribute): T;
var
  LResults: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(System.Default(T));

  LResults := DoGetLazy<T>(id, entity, column);

  if TUtils.IsEnumerable(TypeInfo(T)) then
    Result := GetObjectList<T>(LResults)
  else
    Result := GetOne<T>(LResults, entity);  {TODO -oOwner -cGeneral : get one with arg entity is needed only for lazy loading}
end;

function TAbstractSession.GetList<T>(const query: string;
  const params: IList<TDBParam>): IList<T>;
var
  LResults: IDBResultSet;
begin
  Result := TCollections.CreateObjectList<T>(True);
  LResults := GetResultset(query, params);
  Fetch<T>(LResults, Result);
end;

function TAbstractSession.GetListFromResultset<T>(
  const resultSet: IDBResultSet): IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(True);
  Fetch<T>(resultSet, Result);
end;

function TAbstractSession.GetObjectList<T>(const resultSet: IDBResultSet): T;
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

  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, TypeInfo(T), LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LAddParameters := LAddMethod.GetParameters;
  if Length(LAddParameters) <> 1 then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  if Result.TryGetProperty(METHODNAME_CONTAINER_OWNSOBJECTS, LProp) then
    LProp.SetValue(TObject(Result), True);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
  else
    raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  while not resultSet.IsEmpty do
  begin
    LCurrent := GetOne(resultSet, LEntityClass);
    LAddMethod.Invoke(Result, [LCurrent]);
    resultSet.Next;
  end;
end;

function TAbstractSession.GetOne(const resultSet: IDBResultSet;
  classType: TClass): TObject;
begin
  Result := TRttiExplorer.CreateType(classType);
  DoSetEntity(Result, resultSet, nil);
end;

function TAbstractSession.GetOne<T>(const resultSet: IDBResultSet;
  const entity: TObject): T;
begin
  Result := T.Create;
  DoSetEntity(TObject(Result), resultSet, entity);
end;

function TAbstractSession.GetResultset(const sqlStatement: string;
  const params: array of const): IDBResultSet;
var
  LParams: IList<TDBParam>;
begin
  LParams := TCollections.CreateObjectList<TDBParam>;
  if Length(params) > 0 then
    ConvertParams(params, LParams);
  Result := GetResultset(sqlStatement, LParams);
end;

function TAbstractSession.GetResultset(const sqlStatement: string;
  const params: IList<TDBParam>): IDBResultSet;
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(params), 'Parameters must be assigned');
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(sqlStatement);

  if not params.IsEmpty then
    LStmt.SetParams(params);
  Result := LStmt.ExecuteQuery;
end;

function TAbstractSession.GetResultsetById(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass; const selectColumn: ColumnAttribute): IDBResultSet;
var
  LSelecter: TSelectExecutor;
begin
  LSelecter := GetSelectByIdCommandExecutor(entityClass, id, selectColumn);
  try
    LSelecter.ForeignEntityClass := foreignEntityClass;
    Result := LSelecter.Select;
  finally
    LSelecter.Free;
  end;
end;

function TAbstractSession.GetSelectByIdCommandExecutor(entityClass: TClass;
  const id: TValue; const selectColumn: ColumnAttribute): TSelectExecutor;
begin
  Result := TSelectExecutor.Create(id, selectColumn);
  Result.Connection := Connection;
  Result.EntityClass := entityClass;
  Result.Build(entityClass);
end;

function TAbstractSession.GetSelectCommandExecutor(
  entityClass: TClass): TSelectExecutor;
begin
  Result := TSelectExecutor.Create;
  Result.Connection := Connection;
  Result.EntityClass := entityClass;
  Result.Build(entityClass);
end;

function TAbstractSession.GetUpdateCommandExecutor(
  entityClass: TClass): TUpdateExecutor;
begin
  Result := TUpdateExecutor.Create;
  Result.Connection := Connection;
  Result.EntityClass := entityClass;
  Result.Build(entityClass);
end;

procedure TAbstractSession.SetAssociations(const entity: TObject;
  const resultSet: IDBResultSet; const entityData: TEntityData);
var
  LCol: TORMAttribute;
  LManyToOne: TManyToOneRelation;
begin
  if entityData.HasManyToOneRelations then
  begin
    LManyToOne := TManyToOneRelation.Create;
    try
      for LCol in entityData.ManyToOneColumns do
      begin
        LManyToOne.SetAssociation(LCol, entity, resultSet);
        DoSetEntityValues(LManyToOne.NewEntity, resultSet, LManyToOne.NewColumns, entityData);
      end;
    finally
      LManyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityColumns(const entity: TObject;
  const columns: TColumnDataList; const resultSet: IDBResultSet);
var
  LCol: TColumnData;
  LVal: Variant;
  LValue, LPrimaryKey: TValue;
  LTypeInfo: PTypeInfo;
  i: Integer;
begin
  if columns.TryGetPrimaryKeyColumn(LCol) then
  begin
    try
      LVal := resultSet.GetFieldValue(LCol.Name);
    except
      raise EORMColumnNotFound.CreateFmt(EXCEPTION_PRIMARYKEY_NOTFOUND, [LCol.Name]);
    end;
    LPrimaryKey := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, entity, LCol.ClassMemberName, LPrimaryKey);
  end;

  for i := 0 to columns.Count - 1 do
  begin
    LCol := columns[i];
    if LCol.IsPrimaryKey then
      Continue;

    LTypeInfo := LCol.ColTypeInfo; // GetTypeInfo(AEntity.ClassInfo);
    if Assigned(LTypeInfo) and TUtils.IsLazyType(LTypeInfo) then
      LValue := LPrimaryKey // assign primary key value to lazy type, later convert procedure will assign it to lazy type's private field
    else
    begin
      try
        LVal := resultSet.GetFieldValue(LCol.Name);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [LCol.Name]);
      end;
      LValue := TUtils.ColumnFromVariant(LVal, LCol, Self, entity);
    end;

    TRttiExplorer.SetMemberValue(Self, entity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TAbstractSession.SetEntityColumns(const entity: TObject;
  const columns: IList<ManyValuedAssociation>; const resultSet: IDBResultSet);
var
  LCol: ManyValuedAssociation;
  LVal: Variant;
  LValue: TValue;
begin
  for LCol in columns do
  begin
    LVal := resultSet.GetFieldValue(LCol.MappedBy);
    LValue := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, entity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TAbstractSession.SetInterfaceList(var value: IInterface;
  const resultSet: IDBResultSet; classInfo: PTypeInfo);
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LAddParameters: TArray<TRttiParameter>;
  LValue: TValue;
begin
  if not (classInfo.Kind = tkInterface) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetEntityClass(classInfo, LEntityClass) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, classInfo, LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LAddParameters := LAddMethod.GetParameters;
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
  else
    raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  LValue := TValue.From(value);

  while not resultSet.IsEmpty do
  begin
    LCurrent := GetOne(resultSet, LEntityClass);
    LAddMethod.Invoke(LValue, [LCurrent]);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetInterfaceList<T>(var value: T;
  const resultSet: IDBResultSet);
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LAddParameters: TArray<TRttiParameter>;
  LValue: TValue;
begin
  if not (PTypeInfo(TypeInfo(T)).Kind = tkInterface) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, TypeInfo(T), LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LAddParameters := LAddMethod.GetParameters;
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
  else
    raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  LValue := TValue.From<T>(value);

  while not resultSet.IsEmpty do
  begin
    LCurrent := GetOne(resultSet, LEntityClass);
    LAddMethod.Invoke(LValue, [LCurrent]);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetLazyColumns(const entity: TObject;
  const entityData: TEntityData);
var
  LCol: ManyValuedAssociation;
  LValue: TValue;
  LColumns: IList<OneToManyAttribute>;
begin
  if not entityData.HasOneToManyRelations then
    Exit;
  LColumns := entityData.OneToManyColumns;
  for LCol in LColumns do
  begin
    LValue := TRttiExplorer.GetMemberValue(entity, LCol.MappedBy); //get foreign key value
    TRttiExplorer.SetMemberValue(Self, entity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TAbstractSession.SetLazyValue<T>(var value: T; const id: TValue;
  const entity: TObject; const column: ColumnAttribute);
var
  LResults: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit;

  case PTypeInfo(TypeInfo(T)).Kind of
    tkClass, tkClassRef, tkPointer, tkRecord, tkUnknown:
      raise EORMUnsupportedType.CreateFmt(EXCEPTION_UNSUPPORTED_LAZY_TYPE, [
        PTypeInfo(TypeInfo(T)).TypeName]);
  end;

  LResults := DoGetLazy<T>(id, entity, column);

  if TUtils.IsEnumerable(TypeInfo(T)) then
    SetInterfaceList<T>(value, LResults)
  else
    SetOne<T>(value, LResults, entity);
end;

procedure TAbstractSession.SetOne<T>(var value: T;
  const resultSet: IDBResultSet; const entity: TObject);
var
  LValue, LConverted: TValue;
  LType: TRttiType;
  LColumn: ColumnAttribute;
  LVal: Variant;
begin
  LType := TRttiExplorer.GetEntityRttiType(TypeInfo(T));

  if TRttiExplorer.TryGetColumnByMemberName(entity.ClassType, LType.Name, LColumn)
    and not resultSet.IsEmpty then
  begin
    LVal := resultSet.GetFieldValue(LColumn.Name);
    LValue := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, entity, LColumn, LValue);
  end;
end;

procedure TAbstractSession.SetSimpleInterfaceList(var value: IInterface;
  const resultSet: IDBResultSet; classInfo: PTypeInfo);
var
  LAddMethod: TRttiMethod;
  LValue, LCurrent: TValue;
  LIndex: Integer;
begin
  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, classInfo, LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LValue := TValue.From(value);
  LIndex := 0;
  while not resultSet.IsEmpty do
  begin
    LCurrent := TUtils.FromVariant( resultSet.GetFieldValue(LIndex) );
    LAddMethod.Invoke(LValue, [LCurrent]);
    resultSet.Next;
    Inc(LIndex);
  end;
end;

{$ENDREGION}


end.
