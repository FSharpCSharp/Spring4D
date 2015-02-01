unit Spring.Persistence.Core.EntityWrapper;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes
  ;

type
  TEntityWrapper = class(TInterfacedObject, IEntityWrapper)
  private
    fEntity: TObject;
    fEntityClassData: TEntityData;
    fColumnsToMap: TColumnDataList;
  protected
    function GetEntity: TObject;
  public
    constructor Create(const entity: TObject; const columnsToMap: TColumnDataList = nil); virtual;
    destructor Destroy; override;

    function GetPrimaryKeyValue: TValue;
    function GetPrimaryKeyValueFrom(const resultSet: IDBResultSet): TValue;

    function GetColumnAttribute(const memberName: String): ColumnAttribute;
    function GetColumnsToMap: TColumnDataList;
    function GetColumnValue(const memberName: string): TValue; overload;
    function GetColumnValue(const column: TORMAttribute): TValue; overload;
    function GetColumnValueFrom(const resultSet: IDBResultSet; const columnName: string): TValue;

    procedure SetColumnValue(const columnMemberName: string; const value: TValue); overload;
    procedure SetColumnValue(const column: TORMAttribute; const value: TValue); overload;
    procedure SetMemberValue(const memberName: string; const value: TValue);
    procedure SetPrimaryKeyValue(const value: TValue);

    function HasOneToManyRelations: Boolean;
    function HasManyToOneRelations: Boolean;
    function GetOneToManyColumns: IList<OneToManyAttribute>;
    function GetManyToOneColumns: IList<ManyToOneAttribute>;
    function GetForeignKeyColumns: IList<ForeignJoinColumnAttribute>;

    function GetTableName: string;
  end;

implementation

uses
  SysUtils,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.EmbeddedEntity,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Core.Consts,
  Spring.Reflection
  ;

{ TEntityWrapper }

constructor TEntityWrapper.Create(const entity: TObject; const columnsToMap: TColumnDataList);
begin
  inherited Create;
  fEntity := entity;
  fEntityClassData := TEntityCache.Get(entity.ClassType);
  fColumnsToMap := columnsToMap;
end;

destructor TEntityWrapper.Destroy;
begin
  inherited Destroy;
end;

function TEntityWrapper.GetColumnAttribute(
  const memberName: String): ColumnAttribute;
begin
  Result := fEntityClassData.ColumnByMemberName(memberName);
end;

function TEntityWrapper.GetColumnsToMap: TColumnDataList;
begin
  if Assigned(fColumnsToMap) then
    Result := fColumnsToMap
  else
    Result := fEntityClassData.ColumnsData;
end;

function TEntityWrapper.GetColumnValue(const column: TORMAttribute): TValue;
begin
  Result := column.RttiMember.GetValue(fEntity);
end;

function TEntityWrapper.GetColumnValue(const memberName: string): TValue;
begin
  Result := fEntityClassData.ColumnByMemberName(memberName).RttiMember.GetValue(fEntity);
end;

function TEntityWrapper.GetColumnValueFrom(const resultSet: IDBResultSet; const columnName: string): TValue;
var
  fieldValue: Variant;
begin
  try
    fieldValue := resultSet.GetFieldValue(columnName);
  except
    raise EORMColumnNotFound.CreateFmt(EXCEPTION_COLUMN_NOTFOUND, [columnName]);
  end;
  Result := TUtils.FromVariant(fieldValue);
end;

function TEntityWrapper.GetEntity: TObject;
begin
  Result := fEntity;
end;

function TEntityWrapper.GetForeignKeyColumns: IList<ForeignJoinColumnAttribute>;
begin
  Result := fEntityClassData.ForeignColumns;
end;

function TEntityWrapper.GetManyToOneColumns: IList<ManyToOneAttribute>;
begin
  Result := fEntityClassData.ManyToOneColumns;
end;

function TEntityWrapper.GetOneToManyColumns: IList<OneToManyAttribute>;
begin
  Result := fEntityClassData.OneToManyColumns;
end;

function TEntityWrapper.GetPrimaryKeyValue: TValue;
begin
  if not fEntityClassData.HasPrimaryKey then
    Exit(TValue.Empty);
  Result := fEntityClassData.PrimaryKeyColumn.RttiMember.GetValue(fEntity);
end;

function TEntityWrapper.GetPrimaryKeyValueFrom(const resultSet: IDBResultSet): TValue;
var
  value: Variant;
  columnData: TColumnData;
begin
  if resultSet is TEmbeddedEntity then
    Exit(TValue.Empty);
  //TODO: need to use this method because it correctly maps column name from joined tables
  if not GetColumnsToMap.TryGetPrimaryKeyColumn(columnData) then
    raise EORMPrimaryKeyColumnNotFound.CreateFmt(
      'Primary key column cannot be found for entity: %s', [fEntity.ClassName]);

  //if not fEntityClassData.HasPrimaryKey then
  //  raise EORMPrimaryKeyColumnNotFound.CreateFmt(
  //    'Primary key column cannot be found for entity: %s', [fEntity.ClassName]);
  try
    value := resultSet.GetFieldValue(columnData.ColumnName);
  except
    raise EORMPrimaryKeyColumnNotFound.CreateFmt(EXCEPTION_PRIMARYKEY_NOTFOUND,
      [columnData.ColumnName]);
  end;
  Result := TUtils.FromVariant(value);
end;

function TEntityWrapper.GetTableName: string;
begin
  Result := fEntityClassData.EntityTable.TableName;
end;

function TEntityWrapper.HasManyToOneRelations: Boolean;
begin
  Result := fEntityClassData.HasManyToOneRelations;
end;

function TEntityWrapper.HasOneToManyRelations: Boolean;
begin
  Result := fEntityClassData.HasOneToManyRelations;
end;

procedure TEntityWrapper.SetColumnValue(const column: TORMAttribute; const value: TValue);
begin
  column.RttiMember.SetValue(fEntity, value);
end;

procedure TEntityWrapper.SetColumnValue(const columnMemberName: string;
  const value: TValue);
begin
  SetColumnValue(fEntityClassData.ColumnByMemberName(columnMemberName), value);
end;

procedure TEntityWrapper.SetMemberValue(const memberName: string;
  const value: TValue);
begin
  TType.SetMemberValue(fEntity, memberName, value);
end;

procedure TEntityWrapper.SetPrimaryKeyValue(const value: TValue);
begin
  if not value.IsEmpty then
    TType.SetMemberValue(fEntity, fEntityClassData.PrimaryKeyColumn.MemberName, value);
end;

end.
