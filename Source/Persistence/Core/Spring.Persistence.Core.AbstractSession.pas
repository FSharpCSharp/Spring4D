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

    procedure MapFromResultsetToCollection(const resultSet: IDBResultSet; const collection: IObjectList; classType: TClass);

    function MapEntityFromResultsetRow(const resultSet: IDBResultSet; classType: TClass): TObject; overload;
    function MapEntityFromResultsetRow(const resultSet: IDBResultSet; classType: TClass; realEntity: TObject): TObject; overload;

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

    function GetResultsetById(entityClass: TClass; const id: TValue; foreignEntityClass: TClass = nil; const selectColumn: ColumnAttribute = nil): IDBResultSet;
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
  Spring,
  Spring.Helpers,
  Spring.Persistence.Core.CollectionAdapterResolver,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection,
  Spring.Reflection.Activator;


{$REGION 'TAbstractSession'}

procedure TAbstractSession.DoDelete(const entity, executor: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := executor as TDeleteExecutor;
  LDeleter.Execute(entity);
  DetachEntity(entity);
end;

procedure TAbstractSession.MapFromResultsetToCollection(const resultSet: IDBResultSet;
  const collection: IObjectList; classType: TClass);
var
  LCurrent: TObject;
begin
  while not resultSet.IsEmpty do
  begin
    LCurrent := MapEntityFromResultsetRow(resultSet, classType);
    collection.Add(LCurrent);
    resultSet.Next;
  end;
end;

function TAbstractSession.DoGetLazy(const id: TValue; const entity: TObject;
  const column: ColumnAttribute; classInfo: Pointer): IDBResultSet;
var
  LType: TRttiType;
  LTypes: TArray<TRttiType>;
  LBaseEntityClass, LEntityToLoadClass: TClass;
begin
  LBaseEntityClass := entity.ClassType;
  LType := TType.GetType(classInfo);
  LTypes := LType.GetGenericArguments;
  if Length(LTypes) > 0 then
    LType := LTypes[High(LTypes)];
  LEntityToLoadClass := LType.AsInstance.MetaclassType;

  if not TEntityCache.IsValidEntity(LEntityToLoadClass) then
    LEntityToLoadClass := LBaseEntityClass;

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

procedure TAbstractSession.DoMapEntity(var entityToMap: TObject;
  const resultSet: IDBResultSet; const realEntity: TObject);
var
  LEntityData: TEntityData;
  LResult, LValue: TValue;
  LVal: Variant;
begin
  LEntityData := TEntityCache.Get(entityToMap.ClassType);
  {TODO -oLinas -cGeneral : if entityToCreate class type is not our annotated ORM Entity type (it can be e.g. TPicture, TStream, etc.), simply just set value}
  if not LEntityData.IsTableEntity and Assigned(realEntity) then
  begin
    if not resultSet.IsEmpty then
    begin
      LVal := resultSet.GetFieldValue(0);
      LValue := TUtils.FromVariant(LVal);

      if TUtils.TryConvert(LValue, Self,
        TType.GetType(entityToMap.ClassType), realEntity, LResult) then
      begin
        if entityToMap <> nil then
          entityToMap.Free;
        entityToMap := LResult.AsObject;
        TFinalizer.FinalizeInstance(LValue);
      end;
    end;
  end
  else
  begin
    DoMapEntityFromColumns(entityToMap, resultSet, LEntityData.ColumnsData, LEntityData);
  end;
end;

procedure TAbstractSession.DoMapEntityFromColumns(const entityToMap: TObject;
  const resultSet: IDBResultSet; const columns: TColumnDataList;
  const entityData: TEntityData);
var
  LEntityData: TEntityData;
begin
  SetEntityFromColumns(entityToMap, columns, resultSet);
  //we need to set internal values for the lazy type field
  LEntityData := entityData;
  if entityToMap.ClassType <> entityData.EntityClass then
    LEntityData := TEntityCache.Get(entityToMap.ClassType);

  SetLazyColumns(entityToMap, LEntityData);

  SetAssociations(entityToMap, resultSet, LEntityData);

  AttachEntity(entityToMap);
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
procedure TAbstractSession.FetchFromQueryText(const sqlStatement: string;
  const params: IList<TDBParam>; const collection: IObjectList;
  classType: TClass);
var
  LResults: IDBResultSet;
begin
  LResults := GetResultset(sqlStatement, params);
  MapFromResultsetToCollection(LResults, collection, classType);
end;

procedure TAbstractSession.FetchFromQueryText(const sqlStatement: string;
  const params: array of const; const collection: IObjectList;
  classType: TClass);
var
  LResults: IDBResultSet;
begin
  LResults := GetResultset(sqlStatement, params);
  MapFromResultsetToCollection(LResults, collection, classType);
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

function TAbstractSession.GetObjectList<T>(const resultSet: IDBResultSet): T;
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LProp: TRttiProperty;
  LAddParameters: TArray<TRttiParameter>;
begin
  Result := T.Create;

  LEntityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));

  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, TypeInfo(T), LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LAddParameters := LAddMethod.GetParameters;
  if Length(LAddParameters) <> 1 then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  TType.SetPropertyValue(Result, METHODNAME_CONTAINER_OWNSOBJECTS, True);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
  else
    raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  while not resultSet.IsEmpty do
  begin
    LCurrent := MapEntityFromResultsetRow(resultSet, LEntityClass);
    LAddMethod.Invoke(Result, [LCurrent]);
    resultSet.Next;
  end;
end;

function TAbstractSession.MapEntityFromResultsetRow(const resultSet: IDBResultSet;
  classType: TClass; realEntity: TObject): TObject;
begin
  Result := TActivator.CreateInstance(classType);
  DoMapEntity(Result, resultSet, realEntity);
end;

function TAbstractSession.MapEntityFromResultsetRow(const resultSet: IDBResultSet;
  classType: TClass): TObject;
begin
  Result := TActivator.CreateInstance(classType);
  DoMapEntity(Result, resultSet, nil);
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
        DoMapEntityFromColumns(LManyToOne.NewEntity, resultSet, LManyToOne.NewColumns, entityData);
      end;
    finally
      LManyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityFromColumns(const entity: TObject;
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

procedure TAbstractSession.SetEntityFromColumns(const entity: TObject;
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

  LEntityClass := TRttiExplorer.GetEntityClass(classInfo);

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
    LCurrent := MapEntityFromResultsetRow(resultSet, LEntityClass);
    LAddMethod.Invoke(LValue, [LCurrent]);
    resultSet.Next;
  end;
end;

procedure TAbstractSession.SetInterfaceList<T>(var value: T;
  const resultSet: IDBResultSet);
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LCollection: IObjectList;
begin
  if not (PTypeInfo(TypeInfo(T)).Kind = tkInterface) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  LEntityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));
  LCollection := TValue.From<T>(value).AsInterface as IObjectList;
  while not resultSet.IsEmpty do
  begin
    LCurrent := MapEntityFromResultsetRow(resultSet, LEntityClass);
    LCollection.Add(LCurrent);
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

procedure TAbstractSession.SetOne<T>(var value: T;
  const resultSet: IDBResultSet; const entity: TObject);
var
  LValue, LConverted: TValue;
  LType: TRttiType;
  LColumn: ColumnAttribute;
  LVal: Variant;
begin
  LType := TRttiExplorer.GetEntityRttiType(TypeInfo(T));

  if TEntityCache.TryGetColumnByMemberName(entity.ClassType, LType.Name, LColumn)
    and not resultSet.IsEmpty then
  begin
    LVal := resultSet.GetFieldValue(LColumn.Name);
    LValue := TUtils.FromVariant(LVal);
    value := LValue.AsType<T>;
   // TRttiExplorer.SetMemberValue(Self, entity, LColumn, LValue);
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
