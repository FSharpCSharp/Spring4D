unit Spring.Persistence.Core.AbstractSession;

{$I sv.inc}
interface

uses
  Spring.Persistence.Core.AbstractManager, Spring.Persistence.Core.Interfaces, Rtti, TypInfo
  ,Spring.Persistence.Core.EntityCache
  ,Spring.Collections
  ,Spring.Persistence.SQL.Params
  ,Spring.Persistence.Mapping.Attributes;

type
  TAbstractSession = class(TAbstractManager)
  protected
    procedure SetEntityColumns(AEntity: TObject; AColumns: TColumnDataList; AResultset: IDBResultset); overload; virtual;
    procedure SetEntityColumns(AEntity: TObject; AColumns: IList<ManyValuedAssociation>; AResultset: IDBResultset); overload; virtual;
    procedure SetLazyColumns(AEntity: TObject; AEntityData: TEntityData);
    procedure SetAssociations(AEntity: TObject; AResultset: IDBResultset; AEntityData: TEntityData); virtual;

    procedure DoSetEntity(var AEntityToCreate: TObject; AResultset: IDBResultset; ARealEntity: TObject); virtual;
    procedure DoSetEntityValues(var AEntityToCreate: TObject; AResultset: IDBResultset; AColumns: TColumnDataList; AEntityData: TEntityData); virtual;
    procedure DoFetch<T: class, constructor>(AResultset: IDBResultset; const ACollection: TValue);

    function GetOne<T: class, constructor>(AResultset: IDBResultset; AEntity: TObject): T; overload;
    function GetOne(AResultset: IDBResultset; AClass: TClass): TObject; overload;
    function GetObjectList<T: class, constructor>(AResultset: IDBResultset): T;

    procedure SetInterfaceList<T>(var AValue: T; AResultset: IDBResultset); overload;
    procedure SetInterfaceList(var AValue: IInterface; AResultset: IDBResultset; AClassInfo: PTypeInfo); overload;
    procedure SetSimpleInterfaceList(var AValue: IInterface; AResultset: IDBResultset; AClassInfo: PTypeInfo);
    procedure SetOne<T>(var AValue: T; AResultset: IDBResultset; AEntity: TObject);

    function GetSelector(AClass: TClass): TObject;
    function DoGetLazy<T>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute; out AIsEnumerable: Boolean): IDBResultset;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the <c>resultset</c>.
    ///	</summary>
    {$ENDREGION}
    function GetListFromResultset<T: class, constructor>(AResultset: IDBResultset): IList<T>;
    function GetList<T: class, constructor>(const AQuery: string; const AParams: IList<TDBParam>): IList<T>; overload;

    procedure AttachEntity(AEntity: TObject); virtual; abstract;
    procedure DetachEntity(AEntity: TObject); virtual; abstract;
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Gets the <c>Resultset</c> from SQL statement.
    ///	</summary>
    {$ENDREGION}
    function GetResultset(const ASql: string; const AParams: array of const): IDBResultset; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Gets the <c>Resultset</c> from SQL statement.
    ///	</summary>
    {$ENDREGION}
    function GetResultset(const ASql: string; AParams: IList<TDBParam>): IDBResultset; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the sql statement into the Collection (
    ///	  <c>TObjectList&lt;T&gt;</c> or Spring <c>ICollection&lt;T&gt;</c>).
    ///	</summary>
    {$ENDREGION}
    procedure Fetch<T: class, constructor>(const ASql: string;
      const AParams: array of const; ACollection: ICollection<T>); overload;
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the <c>Resultset</c> into the
    ///	  Collection (<c>TObjectList&lt;T&gt;</c> or Spring
    ///	  <c>ICollection&lt;T&gt;).</c>
    ///	</summary>
    {$ENDREGION}
    procedure Fetch<T: class, constructor>(AResultset: IDBResultset; ACollection: ICollection<T>); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the <c>Resultset</c> into the any
    ///	  Collection. Collection must contain <c>Add</c> method with single
    ///	  parameter.
    ///	</summary>
    {$ENDREGION}
    procedure Fetch<T: class, constructor>(AResultset: IDBResultset; const ACollection: TValue); overload;

    function GetLazyValueClass<T: class, constructor>(const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute): T;
    procedure SetLazyValue<T>(var AValue: T; const AID: TValue; AEntity: TObject; AColumn: ColumnAttribute);
  end;

implementation

uses
  Spring.Persistence.Core.Relation.ManyToOne
  ,Spring.Persistence.Core.Utils
  ,Spring.Persistence.Mapping.RttiExplorer
  ,Spring.Persistence.Core.CollectionAdapterResolver
  ,Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.Reflection
  ,Spring.Persistence.Core.Consts
  ,Spring.Persistence.SQL.Commands.Select
  ,Spring.Persistence.SQL.Commands.Factory
  ;

{ TAbstractSession }

procedure TAbstractSession.DoFetch<T>(AResultset: IDBResultset;
  const ACollection: TValue);
var
  LCurrent: T;
  LCollectionAdapter: ICollectionAdapter<T>;
begin
  LCollectionAdapter := TCollectionAdapterResolver.Resolve<T>(ACollection);
  if not LCollectionAdapter.IsAddSupported then
    raise EORMContainerDoesNotHaveAddMethod.Create('Container does not have "Add" method.');

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne<T>(AResultset, nil);
    LCollectionAdapter.Add(LCurrent);
    AResultset.Next;
  end;
end;

function TAbstractSession.DoGetLazy<T>(const AID: TValue; AEntity: TObject;
  AColumn: ColumnAttribute; out AIsEnumerable: Boolean): IDBResultset;
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
    Result := LSelecter.Select(AEntity, LBaseEntityClass);
  finally
    LSelecter.Free;
  end;
end;

procedure TAbstractSession.DoSetEntity(var AEntityToCreate: TObject;
  AResultset: IDBResultset; ARealEntity: TObject);
var
  LEntityData: TEntityData;
  LResult, LValue: TValue;
  LVal: Variant;
begin
  LEntityData := TEntityCache.Get(AEntityToCreate.ClassType);
  {TODO -oLinas -cGeneral : if AEntity class type is not our real Entity type, simply just set value}
  if not LEntityData.IsTableEntity and Assigned(ARealEntity) then
  begin
    if not AResultset.IsEmpty then
    begin
      LVal := AResultset.GetFieldValue(0);
      LValue := TUtils.FromVariant(LVal);

      if TUtils.TryConvert(LValue, Self,
        TRttiExplorer.GetRttiType(AEntityToCreate.ClassType), ARealEntity, LResult) then
      begin
        if AEntityToCreate <> nil then
          AEntityToCreate.Free;
        AEntityToCreate := LResult.AsObject;
        FreeValueObject(LValue);
      end;
    end;
  end
  else
  begin
    DoSetEntityValues(AEntityToCreate, AResultset, LEntityData.ColumnsData, LEntityData);
  end;
end;

procedure TAbstractSession.DoSetEntityValues(var AEntityToCreate: TObject;
  AResultset: IDBResultset; AColumns: TColumnDataList;
  AEntityData: TEntityData);
var
  LEntityData: TEntityData;
begin
  SetEntityColumns(AEntityToCreate, AColumns, AResultset);
  //we need to set internal values for the lazy type field
  LEntityData := AEntityData;
  if (AEntityToCreate.ClassType <> AEntityData.EntityClass) then
    LEntityData := TEntityCache.Get(AEntityToCreate.ClassType);

  SetLazyColumns(AEntityToCreate, LEntityData);

  SetAssociations(AEntityToCreate, AResultset, LEntityData);

  AttachEntity(AEntityToCreate);
end;

procedure TAbstractSession.Fetch<T>(const ASql: string;
  const AParams: array of const; ACollection: ICollection<T>);
var
  LResults: IDBResultset;
begin
  LResults := GetResultset(ASql, AParams);
  Fetch<T>(LResults, ACollection);
end;

procedure TAbstractSession.Fetch<T>(AResultset: IDBResultset;
  ACollection: ICollection<T>);
var
  LCollection: TValue;
begin
  LCollection := TValue.From(ACollection);
  DoFetch<T>(AResultset, LCollection);
end;

procedure TAbstractSession.Fetch<T>(AResultset: IDBResultset;
  const ACollection: TValue);
begin
  DoFetch<T>(AResultset, ACollection);
end;

function TAbstractSession.GetLazyValueClass<T>(const AID: TValue;
  AEntity: TObject; AColumn: ColumnAttribute): T;
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

function TAbstractSession.GetList<T>(const AQuery: string;
  const AParams: IList<TDBParam>): IList<T>;
var
  LResults: IDBResultset;
begin
  Result := TCollections.CreateObjectList<T>(True);
  LResults := GetResultset(AQuery, AParams);
  Fetch<T>(LResults, Result);
end;

function TAbstractSession.GetListFromResultset<T>(AResultset: IDBResultset): IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(True);
  Fetch<T>(AResultset, Result);
end;

function TAbstractSession.GetObjectList<T>(AResultset: IDBResultset): T;
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
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  if Result.TryGetProperty(METHODNAME_CONTAINER_OWNSOBJECTS, LProp) then
    LProp.SetValue(TObject(Result), True);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
    else
      raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne(AResultset, LEntityClass);

    LAddMethod.Invoke(Result, [LCurrent]);

    AResultset.Next;
  end;
end;

function TAbstractSession.GetOne(AResultset: IDBResultset;
  AClass: TClass): TObject;
begin
  Result := TRttiExplorer.CreateType(AClass);
  DoSetEntity(Result, AResultset, nil);
end;

function TAbstractSession.GetOne<T>(AResultset: IDBResultset;
  AEntity: TObject): T;
begin
  Result := T.Create;
  DoSetEntity(TObject(Result), AResultset, AEntity);
end;

function TAbstractSession.GetResultset(const ASql: string;
  const AParams: array of const): IDBResultset;
var
  LParams: IList<TDBParam>;
begin
  LParams := TCollections.CreateObjectList<TDBParam>();
  if (Length(AParams) > 0) then
  begin
    ConvertParams(AParams, LParams);
  end;
  Result := GetResultset(ASql, LParams);
end;

function TAbstractSession.GetResultset(const ASql: string;
  AParams: IList<TDBParam>): IDBResultset;
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(AParams), 'Parameters must be assigned');
  LStmt := Connection.CreateStatement();
  LStmt.SetSQLCommand(ASql);

  if (AParams.Count > 0) then
  begin
    LStmt.SetParams(AParams);
  end;
  Result := LStmt.ExecuteQuery();
end;

function TAbstractSession.GetSelector(AClass: TClass): TObject;
var
  LSelectExecutor: TSelectExecutor;
begin
  LSelectExecutor := CommandFactory.GetCommand<TSelectExecutor>(AClass, Connection);
  LSelectExecutor.LazyColumn := nil;
  Result := LSelectExecutor;
end;

procedure TAbstractSession.SetAssociations(AEntity: TObject;
  AResultset: IDBResultset; AEntityData: TEntityData);
var
  LCol: TORMAttribute;
  LManyToOne: TManyToOneRelation;
begin
  if AEntityData.HasManyToOneRelations then
  begin
    LManyToOne := TManyToOneRelation.Create;
    try
      for LCol in AEntityData.ManyToOneColumns do
      begin
        LManyToOne.SetAssociation(LCol, AEntity, AResultset);
        DoSetEntityValues(LManyToOne.NewEntity, AResultset, LManyToOne.NewColumns, AEntityData);
      end;
    finally
      LManyToOne.Free;
    end;
  end;
end;

procedure TAbstractSession.SetEntityColumns(AEntity: TObject;
  AColumns: TColumnDataList; AResultset: IDBResultset);
var
  LCol: TColumnData;
  LVal: Variant;
  LValue, LPrimaryKey: TValue;
  LTypeInfo: PTypeInfo;
  i: Integer;
begin
  if AColumns.TryGetPrimaryKeyColumn(LCol) then
  begin
    try
      LVal := AResultset.GetFieldValue(LCol.Name);
    except
      raise EORMColumnNotFound.CreateFmt(EXCEPTION_PRIMARYKEY_NOTFOUND, [LCol.Name]);
    end;
    LPrimaryKey := TUtils.FromVariant(LVal);
    TRttiExplorer.SetMemberValue(Self, AEntity, LCol.ClassMemberName, LPrimaryKey);
  end;

  for i := 0 to AColumns.Count - 1 do
  begin
    LCol := AColumns[i];
    if LCol.IsPrimaryKey then
    begin
      Continue;
    end;

    LTypeInfo := LCol.ColTypeInfo; //  GetTypeInfo(AEntity.ClassInfo);
    if (LTypeInfo <> nil) and (TUtils.IsLazyType(LTypeInfo)) then
    begin
      LValue := LPrimaryKey; //assign primary key value to lazy type, later convert procedure will assign it to lazy type's private field
    end
    else
    begin
      try
        LVal := AResultset.GetFieldValue(LCol.Name);
      except
        raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [LCol.Name]);
      end;
      LValue := TUtils.ColumnFromVariant(LVal, LCol, Self, AEntity);
    end;

    TRttiExplorer.SetMemberValue(Self, AEntity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TAbstractSession.SetEntityColumns(AEntity: TObject;
  AColumns: IList<ManyValuedAssociation>; AResultset: IDBResultset);
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

procedure TAbstractSession.SetInterfaceList(var AValue: IInterface;
  AResultset: IDBResultset; AClassInfo: PTypeInfo);
var
  LCurrent: TObject;
  LEntityClass: TClass;
  LAddMethod: TRttiMethod;
  LAddParameters: TArray<TRttiParameter>;
  LValue: TValue;
begin
  if not (AClassInfo.Kind = tkInterface) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetEntityClass(AClassInfo, LEntityClass) then
    raise EORMUnsupportedType.Create(EXCEPTION_UNSUPPORTED_CONTAINER_TYPE);

  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, AClassInfo, LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);


  LAddParameters := LAddMethod.GetParameters;
  if (Length(LAddParameters) <> 1) then
    raise EORMContainerAddMustHaveOneParameter.Create(EXCEPTION_CONTAINER_ADD_ONE_PARAM);

  case LAddParameters[0].ParamType.TypeKind of
    tkClass, tkClassRef, tkInterface, tkPointer, tkRecord:
    else
      raise EORMContainerItemTypeNotSupported.Create(EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED);
  end;

  LValue := TValue.From(AValue);

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne(AResultset, LEntityClass);

    LAddMethod.Invoke(LValue, [LCurrent]);

    AResultset.Next;
  end;
end;

procedure TAbstractSession.SetInterfaceList<T>(var AValue: T;
  AResultset: IDBResultset);
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

  LValue := TValue.From<T>(AValue);

  while not AResultset.IsEmpty do
  begin
    LCurrent := GetOne(AResultset, LEntityClass);

    LAddMethod.Invoke(LValue, [LCurrent]);

    AResultset.Next;
  end;
end;

procedure TAbstractSession.SetLazyColumns(AEntity: TObject;
  AEntityData: TEntityData);
var
  LCol: ManyValuedAssociation;
  LValue: TValue;
  LColumns: IList<OneToManyAttribute>;
begin
  if not AEntityData.HasOneToManyRelations then
    Exit;
  LColumns := AEntityData.OneToManyColumns;
  for LCol in LColumns do
  begin
    LValue := TRttiExplorer.GetMemberValue(AEntity, LCol.MappedBy); //get foreign key value
    TRttiExplorer.SetMemberValue(Self, AEntity, LCol.ClassMemberName, LValue);
  end;
end;

procedure TAbstractSession.SetLazyValue<T>(var AValue: T; const AID: TValue;
  AEntity: TObject; AColumn: ColumnAttribute);
var
  IsEnumerable: Boolean;
  LResults: IDBResultset;
begin
  if not Assigned(AEntity) or AID.IsEmpty then
  begin
    Exit();
  end;

  case PTypeInfo(TypeInfo(T)).Kind of
    tkClass, tkClassRef, tkPointer, tkRecord, tkUnknown:
    begin
      raise EORMUnsupportedType.CreateFmt(EXCEPTION_UNSUPPORTED_LAZY_TYPE, [string(PTypeInfo(TypeInfo(T)).Name)]);
    end;
  end;

  LResults := DoGetLazy<T>(AID, AEntity, AColumn, IsEnumerable);

  if IsEnumerable then
    SetInterfaceList<T>(AValue, LResults)
  else
    SetOne<T>(AValue, LResults, AEntity);
end;

procedure TAbstractSession.SetOne<T>(var AValue: T; AResultset: IDBResultset;
  AEntity: TObject);
var
  LValue, LConverted: TValue;
  LType: TRttiType;
  LColumn: ColumnAttribute;
  LVal: Variant;
begin
  LType := TRttiExplorer.GetEntityRttiType(TypeInfo(T));

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

procedure TAbstractSession.SetSimpleInterfaceList(var AValue: IInterface;
  AResultset: IDBResultset; AClassInfo: PTypeInfo);
var
  LAddMethod: TRttiMethod;
  LValue, LCurrent: TValue;
  LIndex: Integer;
begin
  if not TRttiExplorer.TryGetBasicMethod(METHODNAME_CONTAINER_ADD, AClassInfo, LAddMethod) then
    raise EORMContainerDoesNotHaveAddMethod.Create(EXCEPTION_CONTAINER_DOESNOTHAVE_ADD);

  LValue := TValue.From(AValue);
  LIndex := 0;
  while not AResultset.IsEmpty do
  begin
    LCurrent := TUtils.FromVariant( AResultset.GetFieldValue(LIndex) );
    LAddMethod.Invoke(LValue, [LCurrent]);
    AResultset.Next;
    Inc(LIndex);
  end;
end;

end.

