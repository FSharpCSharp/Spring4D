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

    {$REGION 'Lazy resolution mechanism - internal use only}
    function ColumnFromVariant(const value: Variant; const column: TColumnData;
      const entity: TObject): TValue;

    function MapAggregatedObject(const resultSet: IDBResultSet;
      const baseEntity: TObject; classType: TClass): TObject;

    function DoGetLazy(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classType: TClass): IDBResultSet;

    function GetLazyValueAsInterface(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; entityClass: TClass): IInterface;
    function GetLazyValueAsObject(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; classType: TClass): TObject;

    function ResolveLazyInterface(const id: TValue; const entity: TObject;
      interfaceType: TRttiType; const column: ColumnAttribute): TValue;
    function ResolveLazyObject(const id: TValue; const entity: TObject;
      entityClass: TClass; const column: ColumnAttribute): TValue;
    {$ENDREGION}
  protected
    /// <summary>
    ///   Retrieves multiple models from the resultset into Spring <c>
    ///   ICollection&lt;T&gt;).</c>
    /// </summary>
    procedure FetchFromQueryText(const sqlStatement: string;
      const params: array of const; const collection: IObjectList;
      classType: TClass); overload;
    procedure FetchFromQueryText(const sqlStatement: string;
      const params: IList<TDBParam>; const collection: IObjectList;
      classType: TClass); overload;

    /// <summary>
    ///   Maps resultset into a list of entities
    /// </summary>
    procedure MapEntitiesFromResultSet(const resultSet: IDBResultSet;
      const collection: IObjectList; classType: TClass);

    /// <summary>
    /// Maps resultset row into the entity
    /// </summary>
    function MapEntityFromResultSetRow(const resultSet: IDBResultSet;
      entityClass: TClass): TObject;

    /// <summary>
    ///   Get the resultset containing the item(s) matching the given id.
    /// </summary>
    function GetResultSetById(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil;
      const selectColumn: ColumnAttribute = nil): IDBResultSet;

    /// <summary>
    ///   Gets the resultset from SQL statement.
    /// </summary>
    function GetResultSet(const sqlStatement: string;
      const params: array of const): IDBResultSet; overload;

    /// <summary>
    ///   Gets the resultset from SQL statement.
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

  TRttiRowMapper = class(TInterfacedObject, IRowMapper<TObject>)
  private
    fEntityClass: TClass;
    fSession: TAbstractSession;
    fEntityWrapper: IEntityWrapper;

    function ResolveLazyValue(const entity: IEntityWrapper;
      const columnMember: TRttiMember): TValue;

    procedure SetAssociations(const entity: IEntityWrapper;
      const resultSet: IDBResultSet);
    procedure SetEntityFromColumns(const entity: IEntityWrapper;
      const resultSet: IDBResultSet);
    procedure SetLazyColumns(const entity: IEntityWrapper);

    procedure DoMapEntityFromColumns(const entity: IEntityWrapper;
      const resultSet: IDBResultSet);
  public
    constructor Create(entityClass: TClass; session: TAbstractSession);
    function MapRow(const resultSet: IDBResultSet): TObject;
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


{$REGION 'TRttiRowMapper'}

constructor TRttiRowMapper.Create(entityClass: TClass; session: TAbstractSession);
begin
  inherited Create;
  fEntityClass := entityClass;
  fSession := session;
  fEntityWrapper := TEntityWrapper.Create(entityClass);
end;

procedure TRttiRowMapper.DoMapEntityFromColumns(const entity: IEntityWrapper;
  const resultSet: IDBResultSet);
begin
  SetEntityFromColumns(entity, resultSet);
  SetLazyColumns(entity);
  SetAssociations(entity, resultSet);
  fSession.AttachEntity(entity);
end;

function TRttiRowMapper.MapRow(const resultSet: IDBResultSet): TObject;
begin
  Result := TActivator.CreateInstance(fEntityClass);
  fEntityWrapper.Entity := Result;
  DoMapEntityFromColumns(fEntityWrapper, resultSet);
end;

function TRttiRowMapper.ResolveLazyValue(const entity: IEntityWrapper;
  const columnMember: TRttiMember): TValue;
var
  memberType: PTypeInfo;
  targetType: TRttiType;
  column: ColumnAttribute;
  id: TValue;
begin
  memberType := columnMember.MemberType.Handle;
  if GetLazyKind(memberType) <> lkRecord then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s - expected Lazy<T>', [memberType.TypeName]);
  targetType := TType.GetType(memberType).GetGenericArguments[0];
  if targetType = nil then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s - insufficient rtti', [memberType.TypeName]);
  Result := TValue.Empty;

  column := columnMember.GetCustomAttribute<ColumnAttribute>;
  id := entity.PrimaryKeyValue;
  case targetType.TypeKind of
    tkClass: Result := fSession.ResolveLazyObject(id, entity.Entity, targetType.AsInstance.MetaclassType, column);
    tkInterface: Result := fSession.ResolveLazyInterface(id, entity.Entity, targetType, column);
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s', [targetType.Name]);
  end;
  if not Result.IsEmpty then
    TValueData(Result).FTypeInfo := memberType;
end;

procedure TRttiRowMapper.SetAssociations(const entity: IEntityWrapper;
  const resultSet: IDBResultSet);
var
  manyToOne: TManyToOneRelation;
  column: ManyToOneAttribute;
  entityWrapper: IEntityWrapper;
begin
  if entity.HasManyToOneRelations then
  begin
    manyToOne := TManyToOneRelation.Create;
    try
      for column in entity.ManyToOneColumns do
      begin
        manyToOne.SetAssociation(entity.Entity, column, resultSet);
        entityWrapper := TEntityWrapper.Create(manyToOne.NewEntity, manyToOne.NewColumns);
        // TODO use pre created rowmapper
        DoMapEntityFromColumns(entityWrapper, resultSet);
      end;
    finally
      manyToOne.Free;
    end;
  end;
end;

procedure TRttiRowMapper.SetEntityFromColumns(const entity: IEntityWrapper;
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

    if columnData.IsLazy then
    begin
      value := ResolveLazyValue(entity, columnData.Member);
      entity.SetValue(columnData.Member, value);
    end
    else
    begin
      try
        fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [columnData.ColumnName]);
      end;
      value := fSession.ColumnFromVariant(fieldValue, columnData, entity.Entity);
      entity.SetValue(columnData.Member, value);
    end;
  end;
end;

procedure TRttiRowMapper.SetLazyColumns(const entity: IEntityWrapper);
var
  column: OneToManyAttribute;
  value: TValue;
begin
  if not entity.HasOneToManyRelations then
    Exit;
  for column in entity.OneToManyColumns do
  begin
    value := ResolveLazyValue(entity, column.Member);
    if not value.IsEmpty then
      entity.SetValue(column.Member, value);
  end;
end;

{$ENDREGION}


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
  convertedValue: TValue;
begin
  Result := TValue.FromVariant(value);
  if not Result.IsEmpty then
    if Result.TryConvert(column.Member.MemberType.Handle, convertedValue) then
      Result := convertedValue;
end;

procedure TAbstractSession.DoDelete(const entity: TObject;
  const deleter: IDeleteCommand);
begin
  deleter.Execute(entity);
  DetachEntity(entity);
end;

function TAbstractSession.DoGetLazy(const id: TValue; const entity: TObject;
  const column: ColumnAttribute; classType: TClass): IDBResultSet;
var
  baseEntityClass,
  entityToLoadClass: TClass;
begin
  baseEntityClass := entity.ClassType;
  entityToLoadClass := classType;

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

function TAbstractSession.MapAggregatedObject(const resultSet: IDBResultSet;
  const baseEntity: TObject; classType: TClass): TObject;

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
      if value.TryConvert(classType.ClassInfo, convertedValue) then
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
  MapEntitiesFromResultSet(results, collection, classType);
end;

procedure TAbstractSession.FetchFromQueryText(const sqlStatement: string;
  const params: array of const; const collection: IObjectList; classType: TClass);
var
  results: IDBResultSet;
begin
  results := GetResultSet(sqlStatement, params);
  MapEntitiesFromResultSet(results, collection, classType);
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
  entityClass: TClass): IInterface;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := DoGetLazy(id, entity, column, entityClass);
  Result := TCollections.CreateObjectList<TObject>(True);
  MapEntitiesFromResultSet(results, Result as IObjectList, entityClass);
end;

function TAbstractSession.GetLazyValueAsObject(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  classType: TClass): TObject;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := DoGetLazy(id, entity, column, classType);
  Result := MapAggregatedObject(results, entity, classType);
end;

procedure TAbstractSession.MapEntitiesFromResultSet(
  const resultSet: IDBResultSet; const collection: IObjectList;
  classType: TClass);
var
  rowMapper: IRowMapper<TObject>;
  entity: TObject;
begin
  if not fRowMappers.TryGetValue(classType, rowMapper) then
    rowMapper := TRttiRowMapper.Create(classType, Self);

  while not resultSet.IsEmpty do
  begin
    entity := rowMapper.MapRow(resultSet);
    collection.Add(entity);
    resultSet.Next;
  end;
end;

function TAbstractSession.MapEntityFromResultSetRow(
  const resultSet: IDBResultSet; entityClass: TClass): TObject;
var
  rowMapper: IRowMapper<TObject>;
begin
  if not fRowMappers.TryGetValue(entityClass, rowMapper) then
    rowMapper := TRttiRowMapper.Create(entityClass, Self);

  Result := rowMapper.MapRow(resultSet)
end;

procedure TAbstractSession.RegisterNonGenericRowMapper(entityClass: TClass;
  const rowMapper: IRowMapper<TObject>);
begin
  if fRowMappers.ContainsKey(entityClass) then
    raise EORMRowMapperAlreadyRegistered.CreateFmt('Row Mapper already registered for type: %s', [entityClass.ClassName]);
  fRowMappers.Add(entityClass, rowMapper);
end;

function TAbstractSession.ResolveLazyInterface(const id: TValue;
  const entity: TObject; interfaceType: TRttiType;
  const column: ColumnAttribute): TValue;
var
  entityClass: TClass;
  capturedId: TValue;
  capturedSelf: Pointer;
  factory: TFunc<IInterface>;
begin
  if not interfaceType.IsGenericTypeOf('IEnumerable<>') then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s', [interfaceType.Name]);
  entityClass := interfaceType.GetGenericArguments[0].AsInstance.MetaclassType;

  capturedId := id;
  capturedSelf := Self; // Capture as unsafe pointer to break cycle
  factory :=
    function: IInterface
    begin
      Result := TAbstractSession(capturedSelf).GetLazyValueAsInterface(
        capturedId, entity, column, entityClass);
    end;
  Result := TValue.From<Lazy<IInterface>>(TLazy<IInterface>.Create(factory));
end;

function TAbstractSession.ResolveLazyObject(const id: TValue;
  const entity: TObject; entityClass: TClass;
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
        capturedId, entity, column, entityClass);
    end;
  Result := TValue.From<Lazy<TObject>>(TLazy<TObject>.Create(factory, True));
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
    if SameText(forColAttribute.ReferencedTableName, primaryKeyTableName) then
      foreignKeyEntity.SetValue(forColAttribute.Member, primaryKeyValue);
end;

{$ENDREGION}


end.
