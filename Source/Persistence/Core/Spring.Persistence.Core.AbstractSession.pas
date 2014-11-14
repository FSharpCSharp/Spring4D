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
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.AbstractManager,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update,
  Spring.Persistence.SQL.Params,
  Spring.Reflection
  ;

type
  TAbstractSession = class(TAbstractManager)
  protected
    procedure SetEntityFromColumns(const entity: TObject;
      const columns: TColumnDataList; const resultSet: IDBResultSet); overload; virtual;
    procedure SetEntityFromColumns(const entity: TObject;
      const columns: IList<ManyValuedAssociation>; const resultSet: IDBResultSet); overload; virtual;
    procedure SetLazyColumns(const entity: TObject; const entityData: TEntityData);
    procedure SetAssociations(const entity: TObject;
      const resultSet: IDBResultSet; const entityData: TEntityData); virtual;

    procedure DoMapEntity(var entityToMap: TObject;
      const resultSet: IDBResultSet; const realEntity: TObject); virtual;
    procedure DoMapEntityFromColumns(const entityToMap: TObject;
      const resultSet: IDBResultSet; const columns: TColumnDataList;
      const entityData: TEntityData); virtual;
    /// <summary>
    ///   Retrieves multiple models from the <c>Resultset</c> into Spring <c>
    ///   ICollection&lt;T&gt;).</c>
    /// </summary>
    procedure FetchFromQueryText(const sqlStatement: string; const params: array of const; const collection: IObjectList; classType: TClass); overload;
    procedure FetchFromQueryText(const sqlStatement: string; const params: IList<TDBParam>; const collection: IObjectList; classType: TClass); overload;

    procedure MapFromResultSetToCollection(const resultSet: IDBResultSet; const collection: IObjectList; classType: TClass);

    function MapEntityFromResultSetRow(const resultSet: IDBResultSet; classInfo: PTypeInfo): TObject; overload;
    function MapEntityFromResultSetRow(const resultSet: IDBResultSet; classInfo: PTypeInfo; realEntity: TObject): TObject; overload;

    function GetObjectList<T: class, constructor>(const resultSet: IDBResultSet): T;

    procedure SetInterfaceList<T>(var value: T; const resultSet: IDBResultSet); overload;
    procedure SetInterfaceList(var value: IInterface;
      const resultSet: IDBResultSet; classInfo: PTypeInfo); overload;
    procedure SetSimpleInterfaceList(var value: IInterface;
      const resultSet: IDBResultSet; classInfo: PTypeInfo);
    procedure SetOne<T>(var value: T; const resultSet: IDBResultSet;
      const entity: TObject);

    function DoGetLazy(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classInfo: Pointer): IDBResultSet;

    function GetLazyValueAsInterface(const id: TValue; const entity: TObject; const column: ColumnAttribute; interfaceType: PTypeInfo): IInterface;
    function GetLazyValueAsObject(const id: TValue; const entity: TObject; const column: ColumnAttribute; classInfo: PTypeInfo): TObject;

    function ResolveLazyValue(const id: TValue; const entity: TObject; columnMemberName: string; lazyTypeInfo: PTypeInfo): TValue;
    function ResolveLazyInterface(const id: TValue; const entity: TObject; lazyKind: TLazyKind; interfaceType: TRttiType; columnMemberName: string): TValue;
    function ResolveLazyClass(const id: TValue; const entity: TObject; lazyKind: TLazyKind; classType: TRttiType; columnMemberName: string): TValue;

    function GetResultSetById(entityClass: TClass; const id: TValue; foreignEntityClass: TClass = nil; const selectColumn: ColumnAttribute = nil): IDBResultSet;
    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultSet(const sqlStatement: string;
      const params: array of const): IDBResultSet; overload;
    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultSet(const sqlStatement: string;
      const params: IList<TDBParam>): IDBResultSet; overload;

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
  end;

implementation

uses
  SysUtils,
  Spring.Helpers,
  Spring.Persistence.Core.CollectionAdapterResolver,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection.Activator;


{$REGION 'TAbstractSession'}

procedure TAbstractSession.DoDelete(const entity, executor: TObject);
var
  deleter: TDeleteExecutor;
begin
  deleter := executor as TDeleteExecutor;
  deleter.Execute(entity);
  DetachEntity(entity);
end;

procedure TAbstractSession.MapFromResultSetToCollection(
  const resultSet: IDBResultSet; const collection: IObjectList; classType: TClass);
var
  entity: TObject;
begin
  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, classType.ClassInfo);
    collection.Add(entity);
    resultSet.Next;
  end;
end;

function TAbstractSession.DoGetLazy(const id: TValue; const entity: TObject;
  const column: ColumnAttribute; classInfo: Pointer): IDBResultSet;
var
  baseEntityClass, entityToLoadClass: TClass;
  rttiType: TRttiType;
  types: TArray<TRttiType>;
begin
  baseEntityClass := entity.ClassType;
  rttiType := TType.GetType(classInfo);
  types := rttiType.GetGenericArguments;
  if Length(types) > 0 then
    rttiType := types[High(types)];
  entityToLoadClass := rttiType.AsInstance.MetaclassType;

  if not TEntityCache.IsValidEntity(entityToLoadClass) then
    entityToLoadClass := baseEntityClass;

  if entityToLoadClass = baseEntityClass then
    baseEntityClass := nil;

  Result := GetResultSetById(entityToLoadClass, id, baseEntityClass, column);
end;

procedure TAbstractSession.DoInsert(const entity, executor: TObject);
var
  inserter: TInsertExecutor;
begin
  inserter := executor as TInsertExecutor;
  inserter.Execute(entity);
  SetLazyColumns(entity, TEntityCache.Get(entity.ClassType));
  AttachEntity(entity);
end;

procedure TAbstractSession.DoMapEntity(var entityToMap: TObject;
  const resultSet: IDBResultSet; const realEntity: TObject);
var
  entityData: TEntityData;
  fieldValue: Variant;
  value, result: TValue;
begin
  entityData := TEntityCache.Get(entityToMap.ClassType);
  {TODO -oLinas -cGeneral : if entityToCreate class type is not our annotated ORM Entity type (it can be e.g. TPicture, TStream, etc.), simply just set value}
  if not entityData.IsTableEntity and Assigned(realEntity) then
  begin
    if not resultSet.IsEmpty then
    begin
      fieldValue := resultSet.GetFieldValue(0);
      value := TUtils.FromVariant(fieldValue);

      if TUtils.TryConvert(value, Self,
        TType.GetType(entityToMap.ClassType), realEntity, result) then
      begin
        if entityToMap <> nil then
          entityToMap.Free;
        entityToMap := result.AsObject;
        TFinalizer.FinalizeInstance(value);
      end;
    end;
  end
  else
    DoMapEntityFromColumns(entityToMap, resultSet, entityData.ColumnsData, entityData);
end;

procedure TAbstractSession.DoMapEntityFromColumns(const entityToMap: TObject;
  const resultSet: IDBResultSet; const columns: TColumnDataList;
  const entityData: TEntityData);
var
  data: TEntityData;
begin
  SetEntityFromColumns(entityToMap, columns, resultSet);
  //we need to set internal values for the lazy type field
  data := entityData;
  if entityToMap.ClassType <> entityData.EntityClass then
    data := TEntityCache.Get(entityToMap.ClassType);

  SetLazyColumns(entityToMap, data);

  SetAssociations(entityToMap, resultSet, data);

  AttachEntity(entityToMap);
end;

procedure TAbstractSession.DoUpdate(const entity, executor: TObject);
var
  updater: TUpdateExecutor;
begin
  updater := executor as TUpdateExecutor;
  updater.Execute(entity);
  SetLazyColumns(entity, TEntityCache.Get(entity.ClassType));
  AttachEntity(entity);
end;

procedure TAbstractSession.FetchFromQueryText(const sqlStatement: string;
  const params: IList<TDBParam>; const collection: IObjectList; classType: TClass);
var
  results: IDBResultSet;
begin
  results := GetResultSet(sqlStatement, params);
  MapFromResultSetToCollection(results, collection, classType);
end;

procedure TAbstractSession.FetchFromQueryText(const sqlStatement: string;
  const params: array of const; const collection: IObjectList; classType: TClass);
var
  results: IDBResultSet;
begin
  results := GetResultSet(sqlStatement, params);
  MapFromResultSetToCollection(results, collection, classType);
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

function TAbstractSession.GetLazyValueAsInterface(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  interfaceType: PTypeInfo): IInterface;
var
  results: IDBResultSet;
begin
  Result := nil;
  if not Assigned(entity) or id.IsEmpty then
    Exit;

  results := DoGetLazy(id, entity, column, interfaceType);
  if TUtils.IsEnumerable(interfaceType) then
  begin
    Result := TCollections.CreateObjectList<TObject>(True);
    SetInterfaceList(Result, results, interfaceType);
  end
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported ORM lazy type: %s', [interfaceType.Name]);
end;

function TAbstractSession.GetLazyValueAsObject(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  classInfo: PTypeInfo): TObject;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := DoGetLazy(id, entity, column, classInfo);
  Result := MapEntityFromResultsetRow(results, classInfo, entity);
end;

function TAbstractSession.ResolveLazyClass(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; classType: TRttiType;
  columnMemberName: string): TValue;
var
  LId: TValue;
  LEntity: TObject;
  factory: TFunc<TObject>;
  LClassType: TRttiType;
  LColumn: ColumnAttribute;
begin
  LId := id;
  LEntity := entity;
  LClassType := classType;
  LColumn := TEntityCache.Get(entity.ClassType).ColumnByMemberName(columnMemberName);
  factory :=
    function: TObject
    begin
      Result := GetLazyValueAsObject(LId, LEntity, LColumn, LClassType.Handle);
    end;
  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<TObject>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<TObject>>(Spring.Lazy<TObject>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(factory));
  end;
  {TODO -Linas : resolve lazy class }
end;

function TAbstractSession.ResolveLazyInterface(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; interfaceType: TRttiType; columnMemberName: string): TValue;
var
  LId: TValue;
  LEntity: TObject;
  factory: TFunc<IInterface>;
  LInterfaceType: TRttiType;
  LColumn: ColumnAttribute;
begin
  LId := id;
  LEntity := entity;
  LInterfaceType := interfaceType;
  LColumn := TEntityCache.Get(entity.ClassType).ColumnByMemberName(columnMemberName);
  factory :=
    function: IInterface
    begin
      Result := GetLazyValueAsInterface(LId, LEntity, LColumn, LInterfaceType.Handle);
    end;
  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<IInterface>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<IInterface>>(Spring.Lazy<IInterface>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(factory));
  end;
end;

function TAbstractSession.ResolveLazyValue(const id: TValue; const entity: TObject; columnMemberName: string; lazyTypeInfo: PTypeInfo): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
begin
  lazyKind := TType.GetLazyKind(lazyTypeInfo);
  targetType := TType.GetType(lazyTypeInfo).GetGenericArguments[0];
  Result := TValue.Empty;
  case targetType.TypeKind of
    tkClass: Result := ResolveLazyClass(id, entity, lazyKind, targetType, columnMemberName);
    tkInterface: Result := ResolveLazyInterface(id, entity, lazyKind, targetType, columnMemberName);
    tkRecord: {TODO -Linas : resolve records, e.g. Nullable<T>};
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported target type', [targetType.Name]);
  end;
  if not Result.IsEmpty then
    TValueData(Result).FTypeInfo := lazyTypeInfo;
end;

function TAbstractSession.GetObjectList<T>(const resultSet: IDBResultSet): T;
var
  entityClass: TClass;
  addMethod: TRttiMethod;
  params: TArray<TRttiParameter>;
  entity: TObject;
begin
  Result := T.Create;

  entityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));

  if not TType.GetType(TypeInfo(T)).Methods.TryGetSingle(addMethod,
    TMethodFilters.IsNamed(METHODNAME_CONTAINER_ADD)
    and TMethodFilters.HasParameterTypes([entityClass.ClassInfo])) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  TType.SetPropertyValue(Result, METHODNAME_CONTAINER_OWNSOBJECTS, True);

  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, entityClass.ClassInfo);
    addMethod.Invoke(Result, [entity]);
    resultSet.Next;
  end;
end;

function TAbstractSession.MapEntityFromResultSetRow(const resultSet: IDBResultSet;
  classInfo: PTypeInfo; realEntity: TObject): TObject;
begin
  Result := TActivator.CreateInstance(classInfo).AsObject;
  DoMapEntity(Result, resultSet, realEntity);
end;

function TAbstractSession.MapEntityFromResultSetRow(const resultSet: IDBResultSet;
  classInfo: PTypeInfo): TObject;
begin
  Result := TActivator.CreateInstance(classInfo).AsObject;
  DoMapEntity(Result, resultSet, nil);
end;

function TAbstractSession.GetResultSet(const sqlStatement: string;
  const params: array of const): IDBResultSet;
var
  parameters: IList<TDBParam>;
begin
  parameters := TCollections.CreateObjectList<TDBParam>;
  if Length(params) > 0 then
    ConvertParams(params, parameters);
  Result := GetResultSet(sqlStatement, parameters);
end;

function TAbstractSession.GetResultSet(const sqlStatement: string;
  const params: IList<TDBParam>): IDBResultSet;
var
  statement: IDBStatement;
begin
  Assert(Assigned(params), 'Parameters must be assigned');
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(sqlStatement);

  if not params.IsEmpty then
    statement.SetParams(params);
  Result := statement.ExecuteQuery;
end;

function TAbstractSession.GetResultSetById(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute): IDBResultSet;
var
  selecter: TSelectExecutor;
begin
  selecter := GetSelectByIdCommandExecutor(entityClass, id, selectColumn);
  try
    selecter.ForeignEntityClass := foreignEntityClass;
    Result := selecter.Select;
  finally
    selecter.Free;
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
  manyToOne: TManyToOneRelation;
  column: TORMAttribute;
begin
  if entityData.HasManyToOneRelations then
  begin
    manyToOne := TManyToOneRelation.Create;
    try
      for column in entityData.ManyToOneColumns do
      begin
        manyToOne.SetAssociation(column, entity, resultSet);
        DoMapEntityFromColumns(manyToOne.NewEntity, resultSet, manyToOne.NewColumns, entityData);
      end;
    finally
      manyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityFromColumns(const entity: TObject;
  const columns: TColumnDataList; const resultSet: IDBResultSet);
var
  columnData: TColumnData;
  fieldValue: Variant;
  primaryKeyValue, value: TValue;
  i: Integer;
  columnTypeInfo: PTypeInfo;
begin
  if columns.TryGetPrimaryKeyColumn(columnData) then
  begin
    try
      fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
    except
      raise EORMColumnNotFound.CreateFmt(EXCEPTION_PRIMARYKEY_NOTFOUND, [columnData.ColumnName]);
    end;
    primaryKeyValue := TUtils.FromVariant(fieldValue);
    TRttiExplorer.SetMemberValue(Self, entity, columnData.MemberName, primaryKeyValue);
  end;

  for i := 0 to columns.Count - 1 do
  begin
    columnData := columns[i];
    if columnData.IsPrimaryKey then
      Continue;

    columnTypeInfo := columnData.TypeInfo;
    if Assigned(columnTypeInfo) and TUtils.IsLazyType(columnTypeInfo) then
    begin
      //value := primaryKeyValue // assign primary key value to lazy type, later convert procedure will assign it to lazy type's private field
      value := ResolveLazyValue(primaryKeyValue, entity, columnData.MemberName, columnTypeInfo);
      TRttiExplorer.SetMemberValueSimple(entity, columnData.MemberName, value);
    end
    else
    begin
      try
        fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [columnData.ColumnName]);
      end;
      value := TUtils.ColumnFromVariant(fieldValue, columnData, Self, entity);
      TRttiExplorer.SetMemberValue(Self, entity, columnData.MemberName, value);
    end;
  end;
end;

procedure TAbstractSession.SetEntityFromColumns(const entity: TObject;
  const columns: IList<ManyValuedAssociation>; const resultSet: IDBResultSet);
var
  column: ManyValuedAssociation;
  fieldValue: Variant;
  value: TValue;
begin
  for column in columns do
  begin
    fieldValue := resultSet.GetFieldValue(column.MappedBy);
    value := TUtils.FromVariant(fieldValue);
    TRttiExplorer.SetMemberValue(Self, entity, column.MemberName, value);
  end;
end;

procedure TAbstractSession.SetInterfaceList(var value: IInterface;
  const resultSet: IDBResultSet; classInfo: PTypeInfo);
var
  entityClass: TClass;
  addMethod: TRttiMethod;
  entity: TObject;
  list: TValue;
begin
  if classInfo.Kind <> tkInterface then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  entityClass := TRttiExplorer.GetEntityClass(classInfo);

  if not TType.GetType(classInfo).Methods.TryGetFirst(addMethod,
    TMethodFilters.IsNamed(METHODNAME_CONTAINER_ADD)
    and TMethodFilters.HasParameterTypes([entityClass.ClassInfo])) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  list := TValue.From(value);
  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, entityClass.ClassInfo);
    addMethod.Invoke(list, [entity]);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetInterfaceList<T>(var value: T;
  const resultSet: IDBResultSet);
var
  entityClass: TClass;
  collection: IObjectList;
  entity: TObject;
begin
  if PTypeInfo(TypeInfo(T)).Kind <> tkInterface then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  entityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));
  collection := TValue.From<T>(value).AsInterface as IObjectList;
  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, entityClass.ClassInfo);
    collection.Add(entity);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetLazyColumns(const entity: TObject;
  const entityData: TEntityData);
var
  columns: IList<OneToManyAttribute>;
  column: ManyValuedAssociation;
  value, foreignKeyValue: TValue;
begin
  if not entityData.HasOneToManyRelations then
    Exit;
  columns := entityData.OneToManyColumns;
  for column in columns do
  begin
    foreignKeyValue := TRttiExplorer.GetMemberValue(entity, column.MappedBy);
    value := ResolveLazyValue(foreignKeyValue, entity, column.MemberName, column.MemberType);
    TRttiExplorer.SetMemberValueSimple(entity, column.MemberName, value);
//    value := TRttiExplorer.GetMemberValue(entity, column.MappedBy); //get foreign key value
//    TRttiExplorer.SetMemberValue(Self, entity, column.MemberName, value);
  end;
end;

procedure TAbstractSession.SetOne<T>(var value: T;
  const resultSet: IDBResultSet; const entity: TObject);
var
  entityType: TRttiType;
  column: ColumnAttribute;
  fieldValue: Variant;
begin
  entityType := TRttiExplorer.GetEntityRttiType(TypeInfo(T));

  if TEntityCache.TryGetColumnByMemberName(entity.ClassType, entityType.Name, column)
    and not resultSet.IsEmpty then
  begin
    fieldValue := resultSet.GetFieldValue(column.ColumnName);
    value := TUtils.FromVariant(fieldValue).AsType<T>;
//    TRttiExplorer.SetMemberValue(Self, entity, LColumn, LValue);
  end;
end;

procedure TAbstractSession.SetSimpleInterfaceList(var value: IInterface;
  const resultSet: IDBResultSet; classInfo: PTypeInfo);
var
  addMethod: TRttiMethod;
  list, item: TValue;
  fieldValue: Variant;
  index: Integer;
begin
  if not TType.GetType(classInfo).Methods.TryGetFirst(addMethod,
    TMethodFilters.IsNamed(METHODNAME_CONTAINER_ADD)) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  list := TValue.From(value);
  index := 0;
  while not resultSet.IsEmpty do
  begin
    fieldValue := resultSet.GetFieldValue(index);
    item := TUtils.FromVariant(fieldValue);
    addMethod.Invoke(list, [item]);
    resultSet.Next;
    Inc(index);
  end;
end;

{$ENDREGION}


end.
