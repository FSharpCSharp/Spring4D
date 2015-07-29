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
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params,
  Spring.Reflection;

type
  TAbstractSession = class
  private
    fConnection: IDBConnection;
    fRowMappers: IDictionary<TClass,IRowMapper<TObject>>;
    fOldStateEntities: IEntityMap;
  protected
    function ColumnFromVariant(const value: Variant; const column: TColumnData; const entity: TObject): TValue;

    procedure SetEntityFromColumns(const entity: IEntityWrapper; const resultSet: IDBResultSet); virtual;
    procedure SetLazyColumns(const entity: IEntityWrapper);
    procedure SetAssociations(const entity: IEntityWrapper;
      const resultSet: IDBResultSet); virtual;

    function DoMapEntity(const resultSet: IDBResultSet; entityClass: TClass): TObject;
    procedure DoMapEntityFromColumns(const entityToMap: IEntityWrapper;
      const resultSet: IDBResultSet); virtual;
    function DoMapObjectInEntity(const resultSet: IDBResultSet;
      const baseEntity: TObject; classType: PTypeInfo): TObject;

    /// <summary>
    ///   Retrieves multiple models from the <c>Resultset</c> into Spring <c>
    ///   ICollection&lt;T&gt;).</c>
    /// </summary>
    procedure FetchFromQueryText(const sqlStatement: string; const params: array of const; const collection: IObjectList; classType: TClass); overload;
    procedure FetchFromQueryText(const sqlStatement: string; const params: IList<TDBParam>; const collection: IObjectList; classType: TClass); overload;

    procedure MapFromResultSetToCollection(const resultSet: IDBResultSet; const collection: IObjectList; classType: TClass);

    /// <summary>
    /// Maps resultset row into the entity
    /// </summary>
    function MapEntityFromResultSetRow(const resultSet: IDBResultSet;
      entityClass: TClass): TObject;

    procedure SetInterfaceListOfObjects(const value: IObjectList;
      const resultSet: IDBResultSet; typeInfo: PTypeInfo);
    procedure SetInterfaceListOfPrimitives(const value: IInterface;
      const resultSet: IDBResultSet; typeInfo: PTypeInfo);

    function DoGetLazy(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classInfo: PTypeInfo): IDBResultSet;

    function GetLazyValueAsInterface(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; interfaceType: PTypeInfo): IInterface;
    function GetLazyValueAsObject(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classType: PTypeInfo): TObject;

    function ResolveLazyValue(const entity: IEntityWrapper;
      const columnMember: TRttiMember; lazyTypeInfo: PTypeInfo): TValue;
    function ResolveLazyInterface(const id: TValue; const entity: TObject;
      lazyKind: TLazyKind; interfaceType: TRttiType; const column: ColumnAttribute): TValue;
    function ResolveLazyClass(const id: TValue; const entity: TObject; lazyKind: TLazyKind;
      classType: TRttiType; const column: ColumnAttribute): TValue;
    function ResolveLazyRecord(const id: TValue; const entity: TObject; lazyKind: TLazyKind;
      recordType: TRttiType; const column: ColumnAttribute): TValue;

    function GetResultSetById(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil; const selectColumn: ColumnAttribute = nil): IDBResultSet;

    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultSet(const sqlStatement: string;
      const params: array of const): IDBResultSet; overload;

    /// <summary>
    ///   Gets the <c>Resultset</c> from SQL statement.
    /// </summary>
    function GetResultSet(const sqlStatement: string;
      const params: IEnumerable<TDBParam>): IDBResultSet; overload;

    procedure AttachEntity(const entity: IEntityWrapper); virtual;
    procedure DetachEntity(const entity: TObject);  virtual;

    procedure DoInsert(const entity: TObject; const inserter: IInsertCommand); virtual;
    procedure DoUpdate(const entity: TObject; const updater: IUpdateCommand); virtual;
    procedure DoDelete(const entity: TObject; const deleter: IDeleteCommand); virtual;

    function GetInsertCommandExecutor(entityClass: TClass): IInsertCommand; virtual;
    function GetUpdateCommandExecutor(entityClass: TClass): IUpdateCommand; virtual;
    function GetDeleteCommandExecutor(entityClass: TClass): IDeleteCommand; virtual;
    function GetSelectCommandExecutor(entityClass: TClass): ISelectCommand; virtual;
    function GetSelectByIdCommandExecutor(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil;
      const selectColumn: ColumnAttribute = nil): ISelectCommand; virtual;

    procedure RegisterNonGenericRowMapper(entityClass: TClass;
      const rowMapper: IRowMapper<TObject>);
    procedure UnregisterNonGenericRowMapper(entityClass: TClass);

    procedure UpdateForeignKeysFor(
      const foreignKeyEntity, primaryKeyEntity: IEntityWrapper);
  public
    constructor Create(const connection: IDBConnection); overload; virtual;
    constructor Create(const connection: IDBConnection;
      const entityMap: IEntityMap); reintroduce; overload;
    destructor Destroy; override;

    property Connection: IDBConnection read fConnection;
    property OldStateEntities: IEntityMap read fOldStateEntities;
  end;

implementation

uses
  Classes,
  SysUtils,
  Variants,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.EntityMap,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update;


{$REGION 'TAbstractSession'}

constructor TAbstractSession.Create(const connection: IDBConnection);
begin
  inherited Create;
  fConnection := connection;
  fRowMappers := TCollections.CreateDictionary<TClass,IRowMapper<TObject>>;
  if fOldStateEntities = nil then
    fOldStateEntities := TEntityMap.Create;
end;

constructor TAbstractSession.Create(const connection: IDBConnection;
  const entityMap: IEntityMap);
begin
  fOldStateEntities := entityMap;
  Create(connection);
end;

destructor TAbstractSession.Destroy;
begin
  fOldStateEntities := nil;
  inherited;
end;

procedure TAbstractSession.AttachEntity(const entity: IEntityWrapper);
begin
  fOldStateEntities.AddOrReplace(entity);
end;

procedure TAbstractSession.DetachEntity(const entity: TObject);
begin
  fOldStateEntities.Remove(entity);
end;

function TAbstractSession.ColumnFromVariant(const value: Variant;
  const column: TColumnData; const entity: TObject): TValue;
var
  results: IDBResultSet;
  list, convertedValue: TValue;
begin
  case VarType(value) of
    // TODO: find out if this works at all
    varUnknown:
    begin
      results := IInterface(value) as IDBResultSet;
      if TType.GetType(column.TypeInfo).HasMethod('GetEnumerator') then
      begin
        list := TRttiExplorer.GetMemberValueDeep(entity, column.Member);
        if TType.GetType(column.TypeInfo).GetGenericArguments[0].IsInstance then
          SetInterfaceListOfObjects(list.AsInterface as IObjectList, results, column.TypeInfo)
        else
          SetInterfaceListOfPrimitives(list.AsInterface, results, column.TypeInfo);
        Result := list;
      end
      else
        Result := DoMapEntity(results, GetTypeData(column.TypeInfo).ClassType);
    end
  else
    Result := TValue.FromVariant(value);
    if not Result.IsEmpty then
      if Result.TryConvert(column.TypeInfo, convertedValue) then
        Result := convertedValue;
  end;
end;

procedure TAbstractSession.DoDelete(const entity: TObject;
  const deleter: IDeleteCommand);
begin
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
    entity := MapEntityFromResultSetRow(resultSet, classType);
    collection.Add(entity);
    resultSet.Next;
  end;
end;

function TAbstractSession.DoGetLazy(const id: TValue; const entity: TObject;
  const column: ColumnAttribute; classInfo: PTypeInfo): IDBResultSet;
var
  baseEntityClass, entityToLoadClass: TClass;
  rttiType: TRttiType;
begin
  baseEntityClass := entity.ClassType;
  rttiType := TType.GetType(classInfo);
  if rttiType.IsGenericTypeOf('IEnumerable<>') then
    rttiType := rttiType.GetGenericArguments[0];
  entityToLoadClass := rttiType.AsInstance.MetaclassType;

  if not TEntityCache.IsValidEntity(entityToLoadClass) then
    entityToLoadClass := baseEntityClass;

  if entityToLoadClass = baseEntityClass then
    baseEntityClass := nil;

  Result := GetResultSetById(entityToLoadClass, id, baseEntityClass, column);
end;

procedure TAbstractSession.DoInsert(const entity: TObject;
  const inserter: IInsertCommand);
var
  entityWrapper: IEntityWrapper;
begin
  entityWrapper := TEntityWrapper.Create(entity);
  inserter.Execute(entity);
  AttachEntity(entityWrapper);
end;

function TAbstractSession.DoMapEntity(const resultSet: IDBResultSet;
  entityClass: TClass): TObject;
var
  entityWrapper: IEntityWrapper;
begin
  Result := TActivator.CreateInstance(entityClass);
  entityWrapper := TEntityWrapper.Create(Result);
  DoMapEntityFromColumns(entityWrapper, resultSet);
end;

procedure TAbstractSession.DoMapEntityFromColumns(const entityToMap: IEntityWrapper;
  const resultSet: IDBResultSet);
begin
  SetEntityFromColumns(entityToMap, resultSet);
  SetLazyColumns(entityToMap);
  SetAssociations(entityToMap, resultSet);
  AttachEntity(entityToMap);
end;

function TAbstractSession.DoMapObjectInEntity(const resultSet: IDBResultSet;
  const baseEntity: TObject; classType: PTypeInfo): TObject;

  function Convert(const value: Variant): TValue;
  var
    stream: TMemoryStream;
    p: Pointer;
  begin
    if VarIsArray(value) then
    begin
      stream := TMemoryStream.Create;
      p := VarArrayLock(value);
      try
        stream.Write(p^, VarArrayHighBound(value, VarArrayDimCount(value)) + 1);
      finally
        VarArrayUnlock(value);
      end;
      Result := stream;
    end
    else
      Result := TValue.FromVariant(value);
  end;

var
  fieldValue: Variant;
  value, convertedValue: TValue;
begin
  Result := nil;
  if not resultSet.IsEmpty then
  begin
    fieldValue := resultSet.GetFieldValue(0);
    value := Convert(fieldValue);
    try
      if value.TryConvert(classType, convertedValue) then
        Result := convertedValue.AsObject;
    finally
      value.Free;
    end;     
  end;
end;

procedure TAbstractSession.DoUpdate(const entity: TObject;
  const updater: IUpdateCommand);
var
  entityWrapper: IEntityWrapper;
begin
  entityWrapper := TEntityWrapper.Create(entity);
  updater.Execute(entity);
  AttachEntity(entityWrapper);
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
  entityClass: TClass): IDeleteCommand;
begin
  Result := TDeleteExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetInsertCommandExecutor(
  entityClass: TClass): IInsertCommand;
begin
  Result := TInsertExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetLazyValueAsInterface(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  interfaceType: PTypeInfo): IInterface;
var
  results: IDBResultSet;
begin
  if not TType.GetType(interfaceType).IsGenericTypeOf('IEnumerable<>') then
    raise EORMUnsupportedType.CreateFmt('Unsupported ORM lazy type: %s', [interfaceType.TypeName]);

  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := DoGetLazy(id, entity, column, interfaceType);
  Result := TCollections.CreateObjectList<TObject>(True);
  SetInterfaceListOfObjects(Result as IObjectList, results, interfaceType);
end;

function TAbstractSession.GetLazyValueAsObject(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  classType: PTypeInfo): TObject;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := DoGetLazy(id, entity, column, classType);
  Result := DoMapObjectInEntity(results, entity, classType);
end;

procedure TAbstractSession.RegisterNonGenericRowMapper(entityClass: TClass;
  const rowMapper: IRowMapper<TObject>);
begin
  if fRowMappers.ContainsKey(entityClass) then
    raise EORMRowMapperAlreadyRegistered.CreateFmt('Row Mapper already registered for type: %s', [entityClass.ClassName]);
  fRowMappers.Add(entityClass, rowMapper);
end;

function TAbstractSession.ResolveLazyClass(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; classType: TRttiType;
  const column: ColumnAttribute): TValue;
var
  capturedId: TValue;
  capturedSelf: Pointer;
  factory: TFunc<TObject>;
begin
  capturedId := id;
  capturedSelf := Self; // Capture as unsafe pointer to break cycle
  factory :=
    function: TObject
    begin
      Result := TAbstractSession(capturedSelf).GetLazyValueAsObject(
        capturedId, entity, column, classType.Handle);
    end;
  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<TObject>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<TObject>>(Spring.Lazy<TObject>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(factory));
  end;
end;

function TAbstractSession.ResolveLazyInterface(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; interfaceType: TRttiType;
  const column: ColumnAttribute): TValue;
var
  capturedId: TValue;
  capturedSelf: Pointer;
  factory: TFunc<IInterface>;
begin
  capturedId := id;
  capturedSelf := Self; // Capture as unsafe pointer to break cycle
  factory :=
    function: IInterface
    begin
      Result := TAbstractSession(capturedSelf).GetLazyValueAsInterface(
        capturedId, entity, column, interfaceType.Handle);
    end;
  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<IInterface>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<IInterface>>(Spring.Lazy<IInterface>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(factory));
  end;
end;

function TAbstractSession.ResolveLazyRecord(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; recordType: TRttiType;
  const column: ColumnAttribute): TValue;
var
  underlyingTypeInfo: PTypeInfo;
  capturedId: TValue;
  capturedSelf: Pointer;
  factory: TFunc<Nullable<TObject>>;
begin
  if not IsNullable(recordType.Handle) then
    raise EORMUnsupportedType.CreateFmt('Unsupported lazy type: %s. Expected Lazy<Nullable<T: TObject>>', [recordType.Name]);

  underlyingTypeInfo := GetUnderlyingType(recordType.Handle);
  capturedId := id;
  capturedSelf := Self; // Capture as unsafe pointer to break cycle
  case underlyingTypeInfo.Kind of
    tkClass: factory :=
      function: Nullable<TObject>
      begin
        Result.Create(TAbstractSession(capturedSelf).GetLazyValueAsObject(
          capturedId, entity, column, underlyingTypeInfo));
      end;
    else
      raise EORMUnsupportedType.CreateFmt('Unsupported target type: %s', [underlyingTypeInfo.TypeName]);
  end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<Nullable<TObject>>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<Nullable<TObject>>>(Spring.Lazy<Nullable<TObject>>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<Nullable<TObject>>>(TLazy<Nullable<TObject>>.Create(factory));
  end;
end;

function TAbstractSession.ResolveLazyValue(const entity: IEntityWrapper;
  const columnMember: TRttiMember; lazyTypeInfo: PTypeInfo): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
  column: ColumnAttribute;
  id: TValue;
begin
  lazyKind := GetLazyKind(lazyTypeInfo);
  targetType := TType.GetType(lazyTypeInfo).GetGenericArguments[0];
  if targetType = nil then
    raise EORMUnsupportedType.CreateFmt('Insufficient rtti information for lazy type: %s', [lazyTypeInfo.TypeName]);
  Result := TValue.Empty;

  column := columnMember.GetCustomAttribute<ColumnAttribute>;
  id := entity.PrimaryKeyValue;
  case targetType.TypeKind of
    tkClass: Result := ResolveLazyClass(id, entity.Entity, lazyKind, targetType, column);
    tkInterface: Result := ResolveLazyInterface(id, entity.Entity, lazyKind, targetType, column);
    tkRecord: Result := ResolveLazyRecord(id, entity.Entity, lazyKind, targetType, column);
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported target type: %s', [targetType.Name]);
  end;
  if not Result.IsEmpty then
    TValueData(Result).FTypeInfo := lazyTypeInfo;
end;

function TAbstractSession.MapEntityFromResultSetRow(const resultSet: IDBResultSet;
  entityClass: TClass): TObject;
var
  rowMapper: IRowMapper<TObject>;
begin
  if fRowMappers.TryGetValue(entityClass, rowMapper) then
    Result := rowMapper.MapRow(resultSet)
  else
    Result := DoMapEntity(resultSet, entityClass);
end;

function TAbstractSession.GetResultSet(const sqlStatement: string;
  const params: array of const): IDBResultSet;
begin
  Result := GetResultSet(sqlStatement, TDBParams.Create(params));
end;

function TAbstractSession.GetResultSet(const sqlStatement: string;
  const params: IEnumerable<TDBParam>): IDBResultSet;
var
  statement: IDBStatement;
begin
  Assert(Assigned(params), 'Parameters must be assigned');
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(sqlStatement);

  if params.Any then
    statement.SetParams(params);
  Result := statement.ExecuteQuery;
end;

function TAbstractSession.GetResultSetById(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute): IDBResultSet;
var
  selecter: ISelectCommand;
begin
  selecter := GetSelectByIdCommandExecutor(
    entityClass, id, foreignEntityClass, selectColumn);
  Result := selecter.Select;
end;

function TAbstractSession.GetSelectByIdCommandExecutor(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute): ISelectCommand;
begin
  Result := TSelectExecutor.Create(Connection, id, foreignEntityClass, selectColumn);
  Result.Build(entityClass);
end;

function TAbstractSession.GetSelectCommandExecutor(
  entityClass: TClass): ISelectCommand;
begin
  Result := TSelectExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetUpdateCommandExecutor(
  entityClass: TClass): IUpdateCommand;
begin
  Result := TUpdateExecutor.Create(Connection, fOldStateEntities);
  Result.Build(entityClass);
end;

procedure TAbstractSession.SetAssociations(const entity: IEntityWrapper;
  const resultSet: IDBResultSet);
var
  manyToOne: TManyToOneRelation;
  column: TORMAttribute;
  entityWrapper: IEntityWrapper;
begin
  if entity.HasManyToOneRelations then
  begin
    manyToOne := TManyToOneRelation.Create;
    try
      for column in entity.ManyToOneColumns do
      begin
        manyToOne.SetAssociation(column, entity.Entity, resultSet);
        entityWrapper := TEntityWrapper.Create(manyToOne.NewEntity, manyToOne.NewColumns);
        DoMapEntityFromColumns(entityWrapper, resultSet);
      end;
    finally
      manyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityFromColumns(const entity: IEntityWrapper;
  const resultSet: IDBResultSet);
var
  columnData: TColumnData;
  fieldValue: Variant;
  value: TValue;
  i: Integer;
begin
  entity.SetPrimaryKeyValue(entity.GetPrimaryKeyValue(resultSet));
  for i := 0 to entity.ColumnsData.Count - 1 do
  begin
    columnData := entity.ColumnsData[i];
    if columnData.IsPrimaryKey then
      Continue;

    if IsLazyType(columnData.TypeInfo) then
    begin
      value := ResolveLazyValue(entity, columnData.Member, columnData.TypeInfo);
      entity.SetValue(columnData.Member, value);
    end
    else
    begin
      try
        fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [columnData.ColumnName]);
      end;
      value := ColumnFromVariant(fieldValue, columnData, entity.Entity);
      entity.SetValue(columnData.Member, value);
    end;
  end;
end;

procedure TAbstractSession.SetInterfaceListOfObjects(const value: IObjectList;
  const resultSet: IDBResultSet; typeInfo: PTypeInfo);
var
  entityClass: TClass;
  entity: TObject;
begin
  entityClass := TRttiExplorer.GetEntityClass(typeInfo);
  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, entityClass);
    value.Add(entity);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetLazyColumns(const entity: IEntityWrapper);
var
  column: OneToManyAttribute;
  value: TValue;
begin
  if not entity.HasOneToManyRelations then
    Exit;
  for column in entity.OneToManyColumns do
  begin
    value := ResolveLazyValue(entity, column.Member, column.MemberType);
    if not value.IsEmpty then
      entity.SetValue(column.Member, value);
  end;
end;

procedure TAbstractSession.SetInterfaceListOfPrimitives(const value: IInterface;
  const resultSet: IDBResultSet; typeInfo: PTypeInfo);
var
  addMethod: TRttiMethod;
  list, item: TValue;
  fieldValue: Variant;
  index: Integer;
begin
  if not TType.GetType(typeInfo).Methods.TryGetFirst(addMethod,
    TMethodFilters.IsNamed(METHODNAME_CONTAINER_ADD)) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  list := TValue.From(value);
  index := 0;
  while not resultSet.IsEmpty do
  begin
    fieldValue := resultSet.GetFieldValue(index);
    item := TValue.FromVariant(fieldValue);
    addMethod.Invoke(list, [item]);
    resultSet.Next;
    Inc(index);
  end;
end;

procedure TAbstractSession.UnregisterNonGenericRowMapper(entityClass: TClass);
begin
  fRowMappers.Remove(entityClass);
end;

procedure TAbstractSession.UpdateForeignKeysFor(
  const foreignKeyEntity, primaryKeyEntity: IEntityWrapper);
var
  forColAttribute: ForeignJoinColumnAttribute;
  primaryKeyValue: TValue;
  primaryKeyTableName: string;
begin
  primaryKeyValue := primaryKeyEntity.PrimaryKeyValue;
  if primaryKeyValue.IsEmpty then
    Exit;
  primaryKeyTableName := primaryKeyEntity.TableName;
  for forColAttribute in foreignKeyEntity.ForeignKeyColumns do
  begin
    if SameText(forColAttribute.ReferencedTableName, primaryKeyTableName) then
      foreignKeyEntity.SetValue(forColAttribute.Member, primaryKeyValue);
  end;
end;

{$ENDREGION}


end.
