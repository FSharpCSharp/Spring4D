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
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params,
  Spring.Reflection;

type
  TAbstractSession = class(TAbstractManager)
  private
    fRowMappers: IDictionary<PTypeInfo,IRowMapper<TObject>>;
  protected
    function ColumnFromVariant(const value: Variant; const column: TColumnData; const entity: TObject): TValue;

    procedure SetEntityFromColumns(const entity: IEntityWrapper; const resultSet: IDBResultSet); virtual;
    procedure SetLazyColumns(const entity: IEntityWrapper);
    procedure SetAssociations(const entity: IEntityWrapper;
      const resultSet: IDBResultSet); virtual;

    function DoMapEntity(const resultSet: IDBResultSet; classInfo: PTypeInfo): TObject;
    procedure DoMapEntityFromColumns(const entityToMap: IEntityWrapper;
      const resultSet: IDBResultSet); virtual;
    function DoMapObjectInEntity(const resultSet: IDBResultSet; const baseEntity: TObject; objectClassInfo: PTypeInfo): TObject;
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
    function MapEntityFromResultSetRow(const resultSet: IDBResultSet; classInfo: PTypeInfo): TObject;

    procedure SetInterfaceListOfObjects(const value: IObjectList;
      const resultSet: IDBResultSet; classInfo: PTypeInfo);
    procedure SetInterfaceListOfPrimitives(const value: IInterface;
      const resultSet: IDBResultSet; classInfo: PTypeInfo);

    function DoGetLazy(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classInfo: Pointer): IDBResultSet;

    function GetLazyValueAsInterface(const id: TValue; const entity: TObject; const column: ColumnAttribute; interfaceType: PTypeInfo): IInterface;
    function GetLazyValueAsObject(const id: TValue; const entity: TObject; const column: ColumnAttribute; classInfo: PTypeInfo): TObject;

    function ResolveLazyValue(const entity: IEntityWrapper; columnMemberName: string; lazyTypeInfo: PTypeInfo): TValue;
    function ResolveLazyInterface(const id: TValue; const entity: TObject; lazyKind: TLazyKind; interfaceType: TRttiType; column: ColumnAttribute): TValue;
    function ResolveLazyClass(const id: TValue; const entity: TObject; lazyKind: TLazyKind; classType: TRttiType; column: ColumnAttribute): TValue;
    function ResolveLazyRecord(const id: TValue; const entity: TObject; lazyKind: TLazyKind; recordType: TRttiType; column: ColumnAttribute): TValue;

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

    procedure AttachEntity(const entity: IEntityWrapper); virtual; abstract;
    procedure DetachEntity(const entity: TObject); virtual; abstract;

    procedure DoInsert(const entity: TObject; const inserter: IInsertCommand); virtual;
    procedure DoUpdate(const entity: TObject; const updater: IUpdateCommand); virtual;
    procedure DoDelete(const entity: TObject; const deleter: IDeleteCommand); virtual;

    function GetInsertCommandExecutor(entityClass: TClass): IInsertCommand; virtual;
    function GetUpdateCommandExecutor(entityClass: TClass; const entityMap: IEntityMap): IUpdateCommand; virtual;
    function GetDeleteCommandExecutor(entityClass: TClass): IDeleteCommand; virtual;
    function GetSelectCommandExecutor(entityClass: TClass): ISelectCommand; virtual;
    function GetSelectByIdCommandExecutor(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil;
      const selectColumn: ColumnAttribute = nil): ISelectCommand; virtual;

    procedure RegisterNonGenericRowMapper(typeInfo: PTypeInfo; const rowMapper: IRowMapper<TObject>);
    procedure UnregisterNonGenericRowMapper(typeInfo: PTypeInfo);

    procedure UpdateForeignKeysFor(const foreignKeyEntity: IEntityWrapper; const primaryKeyEntity: IEntityWrapper);
  public
    constructor Create(const connection: IDBConnection); override;
  end;

implementation

uses
  SysUtils,
  Variants,
  Spring.SystemUtils,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update;


{$REGION 'TAbstractSession'}

function TAbstractSession.ColumnFromVariant(const value: Variant;
  const column: TColumnData; const entity: TObject): TValue;
var
  LEmbeddedEntityResultset: IDBResultset;
  LList, LConvertedValue: TValue;
begin
  case VarType(value) of
    varUnknown:
    begin
      LEmbeddedEntityResultset := TUtils.GetResultsetFromVariant(value);
      if TUtils.IsEnumerable(column.TypeInfo) then
      begin
        LList := TRttiExplorer.GetMemberValueDeep(entity, column.MemberName);
        if TRttiExplorer.GetLastGenericArgumentType(column.TypeInfo).IsInstance then
          SetInterfaceListOfObjects(LList.AsInterface as IObjectList, LEmbeddedEntityResultset, column.TypeInfo)
        else
          SetInterfaceListOfPrimitives(LList.AsInterface, LEmbeddedEntityResultset, column.TypeInfo);
        Result := LList;
      end
      else
      begin
        Result := DoMapEntity(LEmbeddedEntityResultset, column.TypeInfo);
      end;
    end
    else
    begin
      Result := TUtils.FromVariant(value);
      if not Result.IsEmpty then
        if TUtils.TryConvert(Result, column.TypeInfo, entity, LConvertedValue) then
          Result := LConvertedValue;
    end;
  end;
end;

constructor TAbstractSession.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fRowMappers := TCollections.CreateDictionary<PTypeInfo,IRowMapper<TObject>>;
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

procedure TAbstractSession.DoInsert(const entity: TObject;
  const inserter: IInsertCommand);
var
  entityWrapper: IEntityWrapper;
begin
  entityWrapper := TEntityWrapper.Create(entity);
  inserter.Execute(entity);
  AttachEntity(entityWrapper);
end;

function TAbstractSession.DoMapEntity(const resultSet: IDBResultSet; classInfo: PTypeInfo): TObject;
var
  entityWrapper: IEntityWrapper;
begin
  Result := TActivator.CreateInstance(classInfo);
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
  const baseEntity: TObject; objectClassInfo: PTypeInfo): TObject;
var
  fieldValue: Variant;
  value, convertedValue: TValue;
begin
  Result := nil;
  if not resultSet.IsEmpty then
  begin
    fieldValue := resultSet.GetFieldValue(0);
    value := TUtils.FromVariant(fieldValue);
    try
      if TUtils.TryConvert(value, objectClassInfo, baseEntity, convertedValue) then
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
  Result := nil;
  if not Assigned(entity) or id.IsEmpty then
    Exit;

  results := DoGetLazy(id, entity, column, interfaceType);
  if not TUtils.IsEnumerable(interfaceType) then
    raise EORMUnsupportedType.CreateFmt('Unsupported ORM lazy type: %s', [interfaceType.Name]);

  Result := TCollections.CreateObjectList<TObject>(True);
  SetInterfaceListOfObjects(Result as IObjectList, results, interfaceType);
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
  Result := DoMapObjectInEntity(results, entity, classInfo);
end;

procedure TAbstractSession.RegisterNonGenericRowMapper(typeInfo: PTypeInfo;
  const rowMapper: IRowMapper<TObject>);
begin
  if fRowMappers.ContainsKey(typeInfo) then
    raise EORMRowMapperAlreadyRegistered.CreateFmt('Row Mapper already registered for type: %s', [typeInfo.Name]);
  fRowMappers.Add(typeInfo, rowMapper);
end;

function TAbstractSession.ResolveLazyClass(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; classType: TRttiType;
  column: ColumnAttribute): TValue;
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
  LColumn := column;
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
end;

function TAbstractSession.ResolveLazyInterface(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; interfaceType: TRttiType; column: ColumnAttribute): TValue;
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
  LColumn := column;
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

function TAbstractSession.ResolveLazyRecord(const id: TValue;
  const entity: TObject; lazyKind: TLazyKind; recordType: TRttiType;
  column: ColumnAttribute): TValue;
var
  underlyingTypeInfo: PTypeInfo;
  LId: TValue;
  LEntity: TObject;
  factory: TFunc<Nullable<TObject>>;
  LColumn: ColumnAttribute;
begin
  if not IsNullable(recordType.Handle) then
    raise EORMUnsupportedType.CreateFmt('Unsupported lazy type: %s. Expected Lazy<Nullable<T: TObject>>', [recordType.ToString]);

  underlyingTypeInfo := GetUnderlyingType(recordType.Handle);
  LId := id;
  LEntity := entity;
  LColumn := column;
  case underlyingTypeInfo.Kind of
    tkClass: factory :=
      function: Nullable<TObject>
      begin
        Result.Create(GetLazyValueAsObject(LId, LEntity, LColumn, underlyingTypeInfo));
      end;
    else
      raise EORMUnsupportedType.CreateFmt('Unsupported target type: %s', [underlyingTypeInfo.Name]);
  end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<Nullable<TObject>>>(factory);
    lkRecord: Result := TValue.From<Spring.Lazy<Nullable<TObject>>>(Spring.Lazy<Nullable<TObject>>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<Nullable<TObject>>>(TLazy<Nullable<TObject>>.Create(factory));
  end;
end;

function TAbstractSession.ResolveLazyValue(const entity: IEntityWrapper; columnMemberName: string; lazyTypeInfo: PTypeInfo): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
  column: ColumnAttribute;
  id: TValue;
begin
  lazyKind := TType.GetLazyKind(lazyTypeInfo);
  targetType := TType.GetType(lazyTypeInfo).GetGenericArguments[0];
  if targetType = nil then
    raise EORMUnsupportedType.CreateFmt('Insufficient rtti information for lazy type: %s', [lazyTypeInfo.Name]);
  Result := TValue.Empty;

  column := entity.GetColumnAttribute(columnMemberName);
  id := entity.GetPrimaryKeyValue;
  case targetType.TypeKind of
    tkClass: Result := ResolveLazyClass(id, entity.GetEntity, lazyKind, targetType, column);
    tkInterface: Result := ResolveLazyInterface(id, entity.GetEntity, lazyKind, targetType, column);
    tkRecord: Result := ResolveLazyRecord(id, entity.GetEntity, lazyKind, targetType, column);
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported target type: %s', [targetType.Name]);
  end;
  if not Result.IsEmpty then
    TValueData(Result).FTypeInfo := lazyTypeInfo;
end;

function TAbstractSession.MapEntityFromResultSetRow(const resultSet: IDBResultSet;
  classInfo: PTypeInfo): TObject;
var
  rowMapper: IRowMapper<TObject>;
begin
  if fRowMappers.TryGetValue(classInfo, rowMapper) then
    Result := rowMapper.MapRow(resultSet)
  else
    Result := DoMapEntity(resultSet, classInfo);
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
  entityClass: TClass; const entityMap: IEntityMap): IUpdateCommand;
begin
  Result := TUpdateExecutor.Create(Connection, entityMap);
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
      for column in entity.GetManyToOneColumns do
      begin
        manyToOne.SetAssociation(column, entity.GetEntity, resultSet);
        entityWrapper := TEntityWrapper.Create(manyToOne.NewEntity, manyToOne.NewColumns);
        DoMapEntityFromColumns(entityWrapper, resultSet);
      end;
    finally
      manyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityFromColumns(const entity: IEntityWrapper; const resultSet: IDBResultSet);
var
  columnData: TColumnData;
  fieldValue: Variant;
  value: TValue;
  i: Integer;
begin
  entity.SetPrimaryKeyValue(entity.GetPrimaryKeyValueFrom(resultSet));
  for i:= 0 to entity.GetColumnsToMap.Count - 1 do
  begin
    columnData := entity.GetColumnsToMap[i];
    if columnData.IsPrimaryKey then
      Continue;

    if TType.IsLazyType(columnData.TypeInfo) then
    begin
      value := ResolveLazyValue(entity, columnData.MemberName, columnData.TypeInfo);
      entity.SetColumnValue(columnData.ColumnAttr, value);
    end
    else
    begin
      try
        fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [columnData.ColumnName]);
      end;
      value := ColumnFromVariant(fieldValue, columnData, entity.GetEntity);
      entity.SetColumnValue(columnData.ColumnAttr, value);
    end;
  end;
end;

procedure TAbstractSession.SetInterfaceListOfObjects(const value: IObjectList;
  const resultSet: IDBResultSet; classInfo: PTypeInfo);
var
  entityClass: TClass;
  entity: TObject;
begin
  entityClass := TRttiExplorer.GetEntityClass(classInfo);
  while not resultSet.IsEmpty do
  begin
    entity := MapEntityFromResultSetRow(resultSet, entityClass.ClassInfo);
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
  for column in entity.GetOneToManyColumns do
  begin
    value := ResolveLazyValue(entity, column.MemberName, column.MemberType);
    if not value.IsEmpty then
      entity.SetColumnValue(column, value);
  end;
end;

procedure TAbstractSession.SetInterfaceListOfPrimitives(const value: IInterface;
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

procedure TAbstractSession.UnregisterNonGenericRowMapper(typeInfo: PTypeInfo);
begin
  fRowMappers.Remove(typeInfo);
end;

procedure TAbstractSession.UpdateForeignKeysFor(const foreignKeyEntity: IEntityWrapper;
  const primaryKeyEntity: IEntityWrapper);
var
  forColAttribute: ForeignJoinColumnAttribute;
  primaryKeyValue: TValue;
  primaryKeyTableName: string;
begin
  primaryKeyValue := primaryKeyEntity.GetPrimaryKeyValue;
  if primaryKeyValue.IsEmpty then
    Exit;
  primaryKeyTableName := primaryKeyEntity.GetTableName;
  for forColAttribute in foreignKeyEntity.GetForeignKeyColumns do
  begin
    if SameText(forColAttribute.ReferencedTableName, primaryKeyTableName) then
      foreignKeyEntity.SetColumnValue(forColAttribute, primaryKeyValue);
  end;
end;

{$ENDREGION}


end.
