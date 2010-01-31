{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.EntityFramework experimental;  // Spring.Data.Entity

{$I Spring.inc}

interface

uses
  Classes,
  Contnrs,
  Windows,
  SysUtils,
  DateUtils,
  DB,
  Variants,
  Generics.Defaults,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Data,
  Spring.DesignPatterns,
  Spring.Helpers;

type
  TEntity = class;

  TEntityState = (
    esDetached,
    esUnchanged,
    esNew,
    esModified,
    esDeleted
  );

  TEntityStates = set of TEntityState;

  TLoadStatus = (lsGhost, lsLoading, lsLoaded);


  {$REGION 'TEntityID'}

  /// <summary>
  /// Represents an identifier of an entity
  /// </summary>
  TEntityID = record
  private
    fValue: string;
    function GetAsInteger: Integer;
    function GetAsInt64: Int64;
    function GetAsString: string;
    function GetAsGuid: TGuid;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
  public
    constructor Create(const value: Integer); overload;
    constructor Create(const value: Int64); overload;
    constructor Create(const value: string); overload;
    constructor Create(const value: TGuid); overload;
    constructor Create(const value: Variant); overload;
    function Equals(const entityID: TEntityID): Boolean;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsString: string read GetAsString;
    property AsGuid: TGuid read GetAsGuid;
    property AsVariant: Variant read GetAsVariant;
    property IsNull: Boolean read GetIsNull;
    { Comparison Operators }
    class operator NotEqual(const left, right: TEntityID): Boolean;
    class operator Equal(const left, right: TEntityID): Boolean;
    { Conversion Operators }
    class operator Implicit(const value: Integer): TEntityID;
    class operator Implicit(const value: Int64): TEntityID;
    class operator Implicit(const value: string): TEntityID;
    class operator Implicit(const value: TGuid): TEntityID;
    class operator Implicit(const value: Variant): TEntityID;
    class operator Implicit(const value: TEntityID): Integer;
    class operator Implicit(const value: TEntityID): Int64;
    class operator Implicit(const value: TEntityID): string;
    class operator Implicit(const value: TEntityID): TGuid;
    class operator Implicit(const value: TEntityID): Variant;
  end;

  {$ENDREGION}


  IEntityCollection = ICollection<TEntity>;
  IEntityList       = IList<TEntity>;

  /// <summary>
  /// IIdentifiable
  /// </summary>
  IIdentifiable = interface
    function  GetID: TEntityID;
    procedure SetID(const value: TEntityID);
    property ID: TEntityID read GetID write SetID;
  end;

  IEntityChangeTracker = interface
    procedure OnPropertyChanging(const name: string);
    procedure OnPropertyChanged(const name: string);
    function GetEntityState: TEntityState;
    property EntityState: TEntityState read GetEntityState;
  end;

  IEntityIDGenerator = interface
    function NextID: TEntityID;
  end;

  IRepository<T> = interface(IEntityIDGenerator)
//    function GetEntitySetName: string;
    function FindOne(const entityID: TEntityID): T;
//    function FindAll(const specification: ISpecification<T>): IList<T>;
  	procedure Save(entity: T);
    procedure Delete(entity: T);
  end;

  {$M+}

  /// <summary>
  /// Represents an abstract domain object.
  /// </summary>
  TDomainObject = class abstract(TInterfaceBase)
//    property OnPropertyChanging: TNotifyEvent read fOnPropertyChanging write fOnPropertyChanging;
//    property OnPropertyChanged: TNotifyEvent read fOnPropertyChanged write fOnPropertyChanged;
  end;

  {$M-}

  TEntityObjectClass = class of TEntity;

  /// <summary>
  /// Represents an abstract domain object with an identity.
  /// </summary>
  TEntity = class abstract(TDomainObject, IIdentifiable, IInterface)
  private
    fID: TEntityID;
    fIsLoaded: Boolean;
    function GetID: TEntityID;
    function GetEntityState: TEntityState;
    function GetIsUnchanged: Boolean;
    function GetIsNew: Boolean;
    function GetIsModified: Boolean;
    function GetIsDeleted: Boolean;
    function GetChangeTracker: IEntityChangeTracker;
  protected
    { TODO: Pull up to TDomainObject }
    procedure SetPropertyValue<T>(const propertyName: string; var currentValue: T; const newValue: T); inline;
    procedure PropertyChanging(const name: string); virtual;
    procedure PropertyChanged(const name: string); virtual;
  protected
    fChangeTracker: IEntityChangeTracker;
    property ChangeTracker: IEntityChangeTracker read GetChangeTracker;
    function GetEntitySetName: string; virtual;
    procedure SetID(const value: TEntityID); virtual;
    property IsLoaded: Boolean read fIsLoaded;
  public
    constructor Create;
    procedure Loaded; deprecated;
    property EntityState: TEntityState read GetEntityState;
    property EntitySetName: string read GetEntitySetName;
    property IsUnchanged: Boolean read GetIsUnchanged;
    property IsNew: Boolean read GetIsNew;
    property IsModified: Boolean read GetIsModified;
    property IsDeleted: Boolean read GetIsDeleted;
  published
    property ID: TEntityID read GetID write SetID;
  end;

  TValueObject = class abstract(TDomainObject)

  end;

  { TNamedEntity }
  TNamedEntity = class(TEntity)
  private
    fName: string;
    fCreatedBy: string;
    fCreatedTime: TDateTime;
    fUpdatedBy: string;
    fUpdatedTime: TDateTime;
    procedure SetName(const value: string);
    procedure SetCreatedBy(const Value: string);
    procedure SetCreatedTime(const value: TDateTime);
    procedure SetUpdatedBy(const Value: string);
    procedure SetUpdatedTime(const value: TDateTime);
  protected
    property CreatedBy: string read fCreatedBy write SetCreatedBy;
    property CreatedTime: TDateTime read fCreatedTime write SetCreatedTime;
    property UpdatedBy: string read fUpdatedBy write SetUpdatedBy;
    property UpdatedTime: TDateTime read fUpdatedTime write SetUpdatedTime;
  published
    property Name: string read fName write SetName;
  end;

  TObjectStateEntry = class(TInterfaceBase, IEntityChangeTracker, IInterface)
  private
    fEntity: TObject;
    fEntityState: TEntityState;
//    fModifiedProperties: IList<string>;
    function GetEntityState: TEntityState;
    procedure OnPropertyChanging(const name: string);
    procedure OnPropertyChanged(const name: string);
  public
    constructor Create(entity: TObject; entityState: TEntityState);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  //  function GetModifiedProperties: IEnumerable<string>;
    property EntityState: TEntityState read GetEntityState;
  end;

  TEntityKeyPart = record
    EntitySetName: string;
    EntityID: TEntityID;
  end;

  /// <summary>
  /// Maintains object state and identity management for entity type instances
  /// and relationship instances.
  /// </summary>
  TObjectStateManager = class
  strict private
    fList: TDictionary<TEntityKeyPart, TObjectStateEntry>;
  protected
    function GetEntityKey(entity: TObject): TEntityKeyPart;
    procedure AddEntry(entity: TObject; state: TEntityState);
    procedure AddOrUpdateEntry(entity: TObject; state: TEntityState);
    procedure RemoveEntry(entity: TObject);
    procedure ChangeState(entity: TObject; newState: TEntityState);
  public
    constructor Create;
    destructor Destroy; override;
    function GetStateEntries(state: TEntityState): IEnumerable<TObjectStateEntry>;
    function GetStateEntry(entity: TObject): TObjectStateEntry;
    function TryGetStateEntry(entity: TObject; out entry: TObjectStateEntry): Boolean; overload;
    function TryGetStateEntry(const entitySetName: string; const entityID: TEntityID; out entry: TObjectStateEntry): Boolean; overload;
  end;

  /// <summary>
  /// Identity Map
  /// </summary>
  IEntityMap = interface
    function TryFindOne(const entitySetName: string; const entityID: TEntityID; out entity: TObject): Boolean;
  end;

  TCollectionNotification = Generics.Collections.TCollectionNotification;

  /// <summary>
  /// IObjectNotification<T>
  /// </summary>
  IObjectNotification = interface
    procedure OnAdded(sender: TObject; obj: TObject);
    procedure OnChanged(sender: TObject; obj: TObject);
    procedure OnDeleted(sender: TObject; obj: TObject);
  end;

  /// <summary>
  /// Entity Context
  /// </summary>
  IEntityContext = interface // (IObservable<IObjectNotification>)
    procedure Attach(entity: TObject);
    procedure Detach(entity: TObject);
    procedure AddObject(entity: TObject);
    procedure DeleteObject(entity: TObject);
    procedure SaveChanges;
  end;

  /// <summary>
  /// Entity Repository Registry
  /// </summary>
  TRepositoryRegistry = class(TClassRegistry<TClass, IInterface>)
  public
    class function GetRepository(classType: TClass): IRepository<TObject>; overload;
    class function GetRepository<T: class>: IRepository<T>; overload;
    class procedure RegisterRepository<T: class>(const repository: IRepository<T>);
    class procedure UnregisterRepository<T: class>;
  end;

  TObjectNotification = (onAdded, onChanged, onDeleted);

  TObjectContext = class abstract(TSingletonBase<TObjectContext>, IEntityContext, IEntityMap, IInterface)
  private
    fObjectStateManager: TObjectStateManager;
    fListeners: TDictionary<TClass, TList<IObjectNotification>>;
  protected
    fConnection: IDBConnection;
    procedure Notify(item: TObject; notification: TObjectNotification);
    function GetListeners(const classType: TClass): TList<IObjectNotification>;
    { IEntityMap }
    function TryFindOne(const entitySetName: string; const entityID: TEntityID; out entity: TObject): Boolean;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  public
    procedure AddListener(const listener: IObjectNotification);
    procedure RemoveListener(const listener: IObjectNotification);
    procedure Attach(entity: TObject);
    procedure Detach(entity: TObject);
    procedure AddObject(entity: TObject);
    procedure DeleteObject(entity: TObject);
    procedure SaveChanges; virtual;
    function CreateEntityID<T: class>: TEntityID;
    function FindOne<T: class>(const entityID: TEntityID): T;
//    function FindAll<T>: IList<T>; overload;
//    function FindAll<T: class>(const specification: ISpecification<T>): IList<T>; overload;
    property Connection: IDBConnection read fConnection;
    property ObjectStateManager: TObjectStateManager read fObjectStateManager;
  end;

  EEntityException = class(Exception);

  TObjectStateEntryList = TListAdapter<TObjectStateEntry>;

implementation

uses
  Spring.ResourceStrings;


{$REGION 'TEntityID'}

constructor TEntityID.Create(const value: Int64);
begin
  fValue := IntToStr(value);
end;

constructor TEntityID.Create(const value: Integer);
begin
  fValue := IntToStr(value);
end;

constructor TEntityID.Create(const value: string);
begin
  fValue := value;
end;

constructor TEntityID.Create(const value: TGuid);
begin
  fValue := value.ToString;
end;

constructor TEntityID.Create(const value: Variant);
begin
  fValue := VarToStrDef(value, '');
end;

function TEntityID.Equals(const entityID: TEntityID): Boolean;
begin
  Result := fValue = entityID.fValue;
end;

function TEntityID.GetAsInteger: Integer;
begin
  Result := StrToInt(fValue);
end;

function TEntityID.GetAsInt64: Int64;
begin
  Result := StrToInt64(fValue);
end;

function TEntityID.GetAsString: string;
begin
  Result := fValue;
end;

function TEntityID.GetAsGuid: TGuid;
begin
  Result := TGuid.Create(AsString);
end;

function TEntityID.GetAsVariant: Variant;
begin
  Result := fValue;
end;

function TEntityID.GetIsNull: Boolean;
begin
  Result := fValue = '';
end;

class operator TEntityID.Equal(const left, right: TEntityID): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TEntityID.NotEqual(const left, right: TEntityID): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator TEntityID.Implicit(const value: Integer): TEntityID;
begin
  Result := TEntityID.Create(value);
end;

class operator TEntityID.Implicit(const value: Int64): TEntityID;
begin
  Result := TEntityID.Create(value);
end;

class operator TEntityID.Implicit(const value: string): TEntityID;
begin
  Result := TEntityID.Create(value);
end;

class operator TEntityID.Implicit(const value: TGuid): TEntityID;
begin
  Result := TEntityID.Create(value);
end;

class operator TEntityID.Implicit(const value: Variant): TEntityID;
begin
  Result := TEntityID.Create(value);
end;

class operator TEntityID.Implicit(const value: TEntityID): Int64;
begin
  Result := value.AsInt64;
end;

class operator TEntityID.Implicit(const value: TEntityID): Integer;
begin
  Result := value.AsInteger;
end;

class operator TEntityID.Implicit(const value: TEntityID): string;
begin
  Result := value.AsString;
end;

class operator TEntityID.Implicit(const value: TEntityID): Variant;
begin
  Result := value.AsVariant;
end;

class operator TEntityID.Implicit(const value: TEntityID): TGuid;
begin
  Result := value.AsGuid;
end;

{$ENDREGION}


{$REGION 'TEntity'}

constructor TEntity.Create;
begin
  inherited Create;
end;

procedure TEntity.Loaded;
begin
  if not fIsLoaded then
  begin
    fIsLoaded := True;
  end;
end;

procedure TEntity.SetPropertyValue<T>(const propertyName: string;
  var currentValue: T; const newValue: T);
var
  comparer: IEqualityComparer<T>;
begin
  comparer := TEqualityComparer<T>.Default;
  if not comparer.Equals(currentValue, newValue) then
  begin
    if IsUnchanged or IsModified then
    begin
      PropertyChanging(propertyName);
    end;
    currentValue := newValue;
    if IsUnchanged or IsModified then
    begin
      PropertyChanged(propertyName);
    end;
  end;
end;

procedure TEntity.PropertyChanging(const name: string);
begin
  if ChangeTracker <> nil then
  begin
    ChangeTracker.OnPropertyChanging(name);
  end;
end;

procedure TEntity.PropertyChanged(const name: string);
begin
  if (ChangeTracker <> nil) then
  begin
    ChangeTracker.OnPropertyChanged(name);
  end;
end;

function TEntity.GetID: TEntityID;
begin
  Result := fID;
end;

function TEntity.GetChangeTracker: IEntityChangeTracker;
begin
  Result := fChangeTracker;
end;

function TEntity.GetEntitySetName: string;
begin
  Result := ClassName;
end;

function TEntity.GetEntityState: TEntityState;
begin
  if ChangeTracker <> nil then
    Result := ChangeTracker.EntityState
  else
    Result := esDetached;
end;

function TEntity.GetIsUnchanged: Boolean;
begin
  Result := EntityState = esUnchanged;
end;

function TEntity.GetIsNew: Boolean;
begin
  Result := EntityState = esNew;
end;

function TEntity.GetIsModified: Boolean;
begin
  Result := EntityState = esModified;
end;

function TEntity.GetIsDeleted: Boolean;
begin
  Result := EntityState = esDeleted;
end;

procedure TEntity.SetID(const value: TEntityID);
begin
  if IsNew or not IsLoaded then
  begin
    fID := value;
  end;
end;

{$ENDREGION}


{$REGION 'TNamedEntity'}

procedure TNamedEntity.SetName(const value: string);
begin
  SetPropertyValue<string>('Name', fName, value);
end;

procedure TNamedEntity.SetCreatedBy(const Value: string);
begin
  SetPropertyValue<string>('CreatedBy', fCreatedBy, value);
end;

procedure TNamedEntity.SetCreatedTime(const value: TDateTime);
begin
  SetPropertyValue<TDateTime>('CreatedTime', fCreatedTime, value);
end;

procedure TNamedEntity.SetUpdatedBy(const Value: string);
begin
  SetPropertyValue<string>('UpdatedBy', fUpdatedBy, value);
end;

procedure TNamedEntity.SetUpdatedTime(const value: TDateTime);
begin
  SetPropertyValue<TDateTime>('UpdatedTime', fUpdatedTime, value);
end;

{$ENDREGION}


{$REGION 'TObjectStateEntry'}

constructor TObjectStateEntry.Create(entity: TObject; entityState: TEntityState);
begin
  inherited Create;
  fEntity := entity;
  fEntityState := entityState;
end;

destructor TObjectStateEntry.Destroy;
begin
  fEntity.Free;
  inherited Destroy;
end;

procedure TObjectStateEntry.BeforeDestruction;
begin
  if fEntity is TEntity then
  begin
    TEntity(fEntity).fChangeTracker := nil;
  end;
  inherited BeforeDestruction;
end;

function TObjectStateEntry.GetEntityState: TEntityState;
begin
  Result := fEntityState;
end;

procedure TObjectStateEntry.OnPropertyChanging(const name: string);
begin

end;

procedure TObjectStateEntry.OnPropertyChanged(const name: string);
begin
  if fEntityState = esUnchanged then
  begin
    fEntityState := esModified;
  end;
end;

{$ENDREGION}


{$REGION 'TObjectStateManager'}

//(*
constructor TObjectStateManager.Create;
begin
  inherited Create;
  fList := TObjectDictionary<TEntityKeyPart, TObjectStateEntry>.Create(
    [doOwnsValues],
    TEqualityComparer<TEntityKeyPart>.Construct(
      function(const Left, Right: TEntityKeyPart): Boolean
      begin
        Result := (left.EntitySetName = right.EntitySetName) and
          (left.EntityID.Equals(right.EntityID));
      end,
      function(const Value: TEntityKeyPart): Integer
      begin
        Result := TEqualityComparer<string>.Default.GetHashCode(value.EntitySetName);
        Result := Result xor TEqualityComparer<string>.Default.GetHashCode(value.EntityID.AsString);
      end
    )
  );
end;

destructor TObjectStateManager.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TObjectStateManager.GetEntityKey(entity: TObject): TEntityKeyPart;
begin
  if entity is TEntity then
    Result.EntityID := TEntity(entity).ID;
  if entity is TEntity then
    Result.EntitySetName := TEntity(entity).GetEntitySetName
  else
    Result.EntitySetName := entity.ClassName;
end;

procedure TObjectStateManager.AddEntry(entity: TObject; state: TEntityState);
var
  entityKey: TEntityKeyPart;
  entry: TObjectStateEntry;
begin
  entityKey := GetEntityKey(entity);
  if not fList.ContainsKey(entityKey) then
  begin
    entry := TObjectStateEntry.Create(entity, state);
    fList.Add(entityKey, entry);
  end;
end;

procedure TObjectStateManager.AddOrUpdateEntry(entity: TObject;
  state: TEntityState);
var
  entityKey: TEntityKeyPart;
  entry: TObjectStateEntry;
begin
  entityKey := GetEntityKey(entity);
  if fList.TryGetValue(entityKey, entry) then
  begin
    entry.fEntityState := state;
  end
  else
  begin
    entry := TObjectStateEntry.Create(entity, state);
    (entity as TEntity).fChangeTracker := entry;
    fList.Add(entityKey, entry);
  end;
end;

procedure TObjectStateManager.RemoveEntry(entity: TObject);
var
  entityKey: TEntityKeyPart;
begin
  entityKey := GetEntityKey(entity);
  fList.Remove(entityKey);
end;

procedure TObjectStateManager.ChangeState(entity: TObject;
  newState: TEntityState);
var
  entityKey: TEntityKeyPart;
  entry: TObjectStateEntry;
begin
  entityKey := GetEntityKey(entity);
  entry := fList[entityKey];
  entry.fEntityState := newState;
end;

function TObjectStateManager.TryGetStateEntry(entity: TObject;
  out entry: TObjectStateEntry): Boolean;
var
  entityKey: TEntityKeyPart;
begin
  entityKey := GetEntityKey(entity);
  Result := fList.TryGetValue(entityKey, entry);
end;

function TObjectStateManager.TryGetStateEntry(const entitySetName: string;
  const entityID: TEntityID; out entry: TObjectStateEntry): Boolean;
var
  entityKey: TEntityKeyPart;
begin
  entityKey.EntitySetName := entitySetName;
  entityKey.EntityID := entityID;
  Result := fList.TryGetValue(entityKey, entry);
end;

function TObjectStateManager.GetStateEntries(
  state: TEntityState): IEnumerable<TObjectStateEntry>;
var
  entry: TObjectStateEntry;
  collection: IList<TObjectStateEntry>;
begin
  collection := TContainers.CreateList<TObjectStateEntry>;
  for entry in fList.Values do
  begin
    if entry.EntityState = state then
      collection.Add(entry);
  end;
  Result := collection;
end;

function TObjectStateManager.GetStateEntry(entity: TObject): TObjectStateEntry;
var
  entityKey: TEntityKeyPart;
begin
  entityKey := GetEntityKey(entity);
  Result := fList[entityKey];
end;

//*)

{$ENDREGION}


{$REGION 'TRepositoryRegistry'}

class function TRepositoryRegistry.GetRepository(classType: TClass): IRepository<TObject>;
var
  repository: IInterface;
begin
  if Instance.TryGetHandler(classType, repository) then
  begin
    IInterface(Result) := repository;
  end
  else
  begin
    raise EEntityException.CreateFmt(SRepositoryNotFound, [classType.ClassName]);
  end;
end;

class function TRepositoryRegistry.GetRepository<T>: IRepository<T>;
var
  classType: TClass;
begin
  classType := TRtti.GetTypeData<T>.ClassType;
  IInterface(Result) := TRepositoryRegistry.GetRepository(classType);
end;

class procedure TRepositoryRegistry.RegisterRepository<T>(
  const repository: IRepository<T>);
var
  classType: TClass;
begin
  classType := TRtti.GetTypeData<T>.ClassType;
  Instance.&Register(classType, repository);
end;

class procedure TRepositoryRegistry.UnregisterRepository<T>;
var
  classType: TClass;
begin
  classType := TRtti.GetTypeData<T>.ClassType;
  Instance.Unregister(classType);
end;

{$ENDREGION}


{$REGION 'TObjectContext'}

procedure TObjectContext.DoCreate;
begin
  inherited DoCreate;
  fObjectStateManager := TObjectStateManager.Create;
  fListeners := TObjectDictionary<TClass, TList<IObjectNotification>>.Create([doOwnsValues]);
end;

procedure TObjectContext.DoDestroy;
begin
  fObjectStateManager.Free;
  fListeners.Free;
  inherited DoDestroy;
end;

function TObjectContext.TryFindOne(const entitySetName: string;
  const entityID: TEntityID; out entity: TObject): Boolean;
var
//  entityKey: TEntityKeyPart;
  entry: TObjectStateEntry;
begin
//  entityKey.EntitySetName := entitySetName;
//  entityKey.EntityID := entityID;
  Result := ObjectStateManager.TryGetStateEntry(entitySetName, entityID, entry);
  if Result and (entry <> nil) then
    entity := entry.fEntity
  else
    entity := nil;
end;

procedure TObjectContext.Attach(entity: TObject);
begin
  fObjectStateManager.AddOrUpdateEntry(entity, esUnchanged);
end;

function TObjectContext.GetListeners(const classType: TClass): TList<IObjectNotification>;
var
  keyClass: TClass;
begin
  keyClass := classType;
  Result := nil;
  while keyClass <> nil do
  begin
    if fListeners.TryGetValue(keyClass, Result) then
      Exit;
    keyClass := keyClass.ClassParent;
  end;
  if Result = nil then
  begin
    Result := TList<IObjectNotification>.Create;
    fListeners.Add(classType, Result);
  end;
end;

procedure TObjectContext.Notify(item: TObject; notification: TObjectNotification);
var
  list: TList<IObjectNotification>;
  listener: IObjectNotification;
begin
  list := GetListeners(item.ClassType);
  case notification of
    onAdded:
    begin
      for listener in list do
      begin
        listener.OnAdded(Self, item);
      end;
    end;
    onChanged:
    begin
      for listener in list do
      begin
        listener.OnChanged(Self, item);
      end;
    end;
    onDeleted:
    begin
      for listener in list do
      begin
        listener.OnDeleted(Self, item);
      end;
    end;
  end;
end;

procedure TObjectContext.AddListener(const listener: IObjectNotification);
var
  keyClass: TClass;
  list: TList<IObjectNotification>;
begin
  keyClass := TEntity;  // TEMP
  list := GetListeners(keyClass);
  if not list.Contains(listener) then
  begin
    list.Add(listener);
  end;
end;

procedure TObjectContext.RemoveListener(
  const listener: IObjectNotification);
var
  list: TList<IObjectNotification>;
begin
  for list in fListeners.Values do
  begin
    if list.Contains(listener) then
      list.Remove(listener)
  end;
end;

procedure TObjectContext.AddObject(entity: TObject);
begin
  fObjectStateManager.AddOrUpdateEntry(entity, esNew);
end;

procedure TObjectContext.DeleteObject(entity: TObject);
begin
  fObjectStateManager.AddOrUpdateEntry(entity, esDeleted);
end;

procedure TObjectContext.Detach(entity: TObject);
begin
  raise ENotImplementedException.Create('Detach');
end;

procedure TObjectContext.SaveChanges;
var
  entry: TObjectStateEntry;
  entity: TObject;
  repository: IRepository<TObject>;
  newCollection: IEnumerable<TObjectStateEntry>;
  modifiedCollection: IEnumerable<TObjectStateEntry>;
  deletedCollection: IEnumerable<TObjectStateEntry>;
begin
  Assert(Connection <> nil, 'Connection should not be nil.');
  with Connection.BeginTransaction do
  try
    newCollection := ObjectStateManager.GetStateEntries(esNew);
    for entry in newCollection do
    begin
      entity := entry.fEntity;
      repository := TRepositoryRegistry.GetRepository(entity.ClassType);
      repository.Save(entity);
    end;
    modifiedCollection := ObjectStateManager.GetStateEntries(esModified);
    for entry in modifiedCollection do
    begin
      entity := entry.fEntity;
      repository := TRepositoryRegistry.GetRepository(entity.ClassType);
      repository.Save(entity);
    end;
    deletedCollection := ObjectStateManager.GetStateEntries(esDeleted);
    for entry in deletedCollection do
    begin
      entity := entry.fEntity;
      repository := TRepositoryRegistry.GetRepository(entity.ClassType);
      repository.Delete(entity);
    end;
    Commit;
  except
    Rollback;
    raise;
  end;
  for entry in newCollection do
  begin
    ObjectStateManager.AddOrUpdateEntry(entry.fEntity, esUnchanged);
  end;
  for entry in modifiedCollection do
  begin
    ObjectStateManager.AddOrUpdateEntry(entry.fEntity, esUnchanged);
  end;
  for entry in deletedCollection do
  begin
    ObjectStateManager.RemoveEntry(entry.fEntity);
  end;
  for entry in newCollection do
  begin
    Notify(entry.fEntity, onAdded);
  end;
  for entry in modifiedCollection do
  begin
    Notify(entry.fEntity, onChanged);
  end;
  for entry in deletedCollection do
  begin
    Notify(entry.fEntity, onDeleted);
  end;
end;

function TObjectContext.CreateEntityID<T>: TEntityID;
begin
  Result := TGuid.NewGuid;  { TODO: CreateEntityID }
end;

function TObjectContext.FindOne<T>(const entityID: TEntityID): T;
var
  repository: IRepository<T>;
begin
  repository := TRepositoryRegistry.GetRepository<T>;
  Result := repository.FindOne(entityID);
end;

{$ENDREGION}

initialization

finalization

end.
