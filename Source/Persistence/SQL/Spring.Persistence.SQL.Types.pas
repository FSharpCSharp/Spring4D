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

unit Spring.Persistence.SQL.Types;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  TypInfo;

type
  /// <summary>
  ///   Represents Database table.
  /// </summary>
  TSQLTable = class
  private
    fName: string;
    fSchema: string;
    fDescription: string;
    fAlias: string;
    function GetAlias: string;
    function GetName: string;
    function GetNameWithoutSchema: string;
    function GetFullTableName: string;
  public
    class function CreateFromClass(entityClass: TClass): TSQLTable;

    function SchemaExists: Boolean;

    procedure SetFromAttribute(const attribute: TableAttribute);

    property Alias: string read GetAlias write fAlias;
    property Description: string read fDescription write fDescription;
    property Name: string read GetName write fName;
    property Schema: string read fSchema write fSchema;
    property NameWithoutSchema: string read GetNameWithoutSchema;
    property FullTableName: string read GetFullTableName;
  end;

  /// <summary>
  ///   Represents field of the database table.
  /// </summary>
  ISQLField = interface
    ['{2316102E-61A3-4454-A7B2-18090C384882}']
    function GetFieldname: string;
    function GetFullFieldName(const escapeChar: Char): string;
    function GetTable: TSQLTable;
    function GetAlias: string;
    procedure SetAlias(const value: string);
    property Alias: string read GetAlias write SetAlias;
    property Fieldname: string read GetFieldname;
    property Table: TSQLTable read GetTable;
  end;

  /// <summary>
  ///   Represents field of the database table.
  /// </summary>
  TSQLField = class(TInterfacedObject, ISQLField)
  private
    fTable: TSQLTable;
    fFieldname: string;
    fAlias: string;
    fColumn: ColumnAttribute;
    function GetFieldname: string;
    function GetTable: TSQLTable;
    function GetAlias: string;
    procedure SetAlias(const value: string);
  public
    constructor Create(const fieldName: string; const table: TSQLTable); virtual;

    function GetFullFieldName(const escapeChar: Char): string; virtual;
    function GetEscapedName(const name: string; const escapeChar: Char): string; virtual;
    function GetEscapedFieldname(const escapeChar: Char): string; virtual;

    property Alias: string read GetAlias write SetAlias;
    property Column: ColumnAttribute read fColumn write fColumn;
    property Fieldname: string read GetFieldname write fFieldname;
    property Table: TSQLTable read GetTable write fTable;
  end;

  TSQLParamField = class(TSQLField)
  private
    fParamName: string;
  public
    constructor Create(const fieldName: string; const table: TSQLTable;
      const column: ColumnAttribute; const paramName: string); reintroduce; virtual;

    property ParamName: string read fParamName write fParamName;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>select</c>
  ///   statements.
  /// </summary>
  TSQLSelectField = class(TSQLField);

  /// <summary>
  ///   Represents field of the database table which is used in <c>insert</c>
  ///   statements.
  /// </summary>
  TSQLInsertField = class(TSQLParamField);

  /// <summary>
  ///   Represents field of the database table which is used in <c>update</c>
  ///   statements.
  /// </summary>
  TSQLUpdateField = class(TSQLParamField);

  /// <summary>
  ///   Represents field of the database table which is used in <c>create table</c>
  ///    statements.
  /// </summary>
  TSQLCreateField = class(TSQLField)
  private
    fIsPrimaryKey: Boolean;
    fIsIdentity: Boolean;
    fTypeInfo: PTypeInfo;
    fLength: Integer;
    fScale: Integer;
    fProperties: TColumnProperties;
    fDescription: string;
    fPrecision: Integer;
    fColumnAttribute: ColumnAttribute;
  public
    procedure SetFromAttribute(const attribute: ColumnAttribute); virtual;
    function Clone: TSQLCreateField;

    property Description: string read fDescription;
    property IsPrimaryKey: Boolean read fIsPrimaryKey;
    property IsIdentity: Boolean read fIsIdentity;
    property TypeInfo: PTypeInfo read fTypeInfo write fTypeInfo;
    property Length: Integer read fLength;
    property Precision: Integer read fPrecision;
    property Scale: Integer read fScale;
    property Properties: TColumnProperties read fProperties;
  end;

  /// <summary>
  ///   Represents foreign key field of the database table.
  /// </summary>
  TSQLForeignKeyField = class(TSQLField)
  private
    fReferencedColumnName: string;
    fConstraints: TForeignStrategies;
    fReferencedTableName: string;
    function GetForeignKeyName: string;
    function GetConstraintsAsString: string;
  public
    constructor Create(const fieldName: string; const table: TSQLTable;
      const referencedColumnName, referencedTableName: string;
      constraints: TForeignStrategies); reintroduce; overload;

    property ForeignKeyName: string read GetForeignKeyName;
    property ReferencedColumnName: string read fReferencedColumnName write fReferencedColumnName;
    property ReferencedTableName: string read fReferencedTableName write fReferencedTableName;
    property Constraints: TForeignStrategies read fConstraints write fConstraints;
    property ConstraintsAsString: string read GetConstraintsAsString;
  end;

  TMatchMode = (mmExact, mmStart, mmEnd, mmAnywhere);

  TWhereOperator = (woEqual, woNotEqual, woMore, woLess, woLike, woNotLike,
    woMoreOrEqual, woLessOrEqual, woIn, woNotIn, woIsNull, woIsNotNull, woOr,
    woOrEnd, woAnd, woAndEnd, woNot, woNotEnd, woBetween, woJunction);

  TStartOperators = set of TWhereOperator;

  TEndOperators = set of TWhereOperator;

  /// <summary>
  ///   Represents field of the database table which is used in <c>where</c>
  ///   clause.
  /// </summary>
  TSQLWhereField = class(TSQLParamField)
  private
    fWhereOperator: TWhereOperator;
    fMatchMode: TMatchMode;
    fLeftSQL: string;
    fRightSQL: string;
    fParamName2: string;
  public
    constructor Create(const fieldName: string; const table: TSQLTable); reintroduce; overload;
    constructor Create(const leftSQL, rightSQL: string); reintroduce; overload;

    function ToSQLString(const escapeChar: Char): string; virtual;

    property MatchMode: TMatchMode read fMatchMode write fMatchMode;
    property WhereOperator: TWhereOperator read fWhereOperator write fWhereOperator;
    property LeftSQL: string read fLeftSQL write fLeftSQL;
    property RightSQL: string read fRightSQL write fRightSQL;
    property ParamName2: string read fParamName2 write fParamName2;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>where</c>
  ///   clause.
  /// </summary>
  TSQLWherePropertyField = class(TSQLWhereField)
  private
    fOtherTable: TSQLTable;
    function GetFullLeftFieldName: string;
    function GetFullRightFieldName: string;
  public
    constructor Create(const leftPropertyName, rightPropertyName: string;
      const leftTable, rightTable: TSQLTable); overload;

    function ToSQLString(const escapeChar: Char): string; override;

    property OtherTable: TSQLTable read fOtherTable write fOtherTable;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>group by</c>
  ///   clause.
  /// </summary>
  TSQLGroupByField = class(TSQLField)
  end;

  TSortingDirection = (stAscending, stDescending);

  /// <summary>
  ///   Represents field of the database table which is used in <c>order by</c>
  ///   clause.
  /// </summary>
  TSQLOrderByField = class(TSQLField)
  private
    fSortingDirection: TSortingDirection;
  public
    constructor Create(const fieldName: string; const table: TSQLTable); override;

    function GetFullOrderByFieldname(const escapeChar: Char): string;

    property SortingDirection: TSortingDirection read fSortingDirection write fSortingDirection;
  end;

  TSQLJoinType = (jtInner, jtLeft);

  /// <summary>
  ///   Represents <c>join</c> segment.
  /// </summary>
  TSQLJoinSegment = class
  private
    fPrimaryKeyField: ISQLField;
    fForeignKeyField: ISQLField;
  public
    constructor Create(const primaryKeyField, foreignKeyField: ISQLField);

    property PrimaryKeyField: ISQLField read fPrimaryKeyField write fPrimaryKeyField;
    property ForeignKeyField: ISQLField read fForeignKeyField write fForeignKeyField;
  end;

  /// <summary>
  ///   Represents <c>join</c> of database tables.
  /// </summary>
  TSQLJoin = class
  private
    fJoinType: TSQLJoinType;
    fSegments: IList<TSQLJoinSegment>;
  public
    constructor Create(const joinType: TSQLJoinType);

    property JoinType: TSQLJoinType read fJoinType write fJoinType;
    property Segments: IList<TSQLJoinSegment> read fSegments write fSegments;
  end;

  /// <summary>
  ///   Static class which is used to generate table aliases.
  /// </summary>
  TSQLAliasGenerator = class
  private
    class var fAliases: IDictionary<string,string>;
    class var fCharIndex: Byte;

    class function AliasExists(const tableName: string): Boolean;
  public
    class constructor Create;
    class destructor Destroy;

    class function GetAlias(const tableName: string): string;
  end;

const
  WhereOperatorNames: array[TWhereOperator] of string = (
    {woEqual =} '=', {woNotEqual =} '<>', {woMore = }'>', {woLess = }'<', {woLike = }'LIKE', {woNotLike = }'NOT LIKE',
    {woMoreOrEqual = }'>=', {woLessOrEqual = }'<=', {woIn = }'IN', {woNotIn = }'NOT IN', {woIsNull} 'IS NULL', {woIsNotNull} 'IS NOT NULL'
    ,{woOr}'OR', {woOrEnd}'', {woAnd} 'AND', {woAndEnd}'', {woNot}'NOT', {woNotEnd}'',{woBetween}'BETWEEN', {woJunction} ''
    );

  StartOperators: TStartOperators = [woOr, woAnd, woNot];

  EndOperators: TEndOperators = [woOrEnd, woAndEnd, woNotEnd];

  StartEndOperators = [woOr, woOrEnd, woAnd, woAndEnd, woNot, woNotEnd];

  SortingDirectionNames: array[TSortingDirection] of string = (
    ' ASC', // stAscending
    ' DESC' // stDescending
  );

  JoinTypeNames: array[TSQLJoinType] of string = (
    ' INNER JOIN ',     // jtInner
    ' LEFT OUTER JOIN ' // jtOuter
  );

  ForeignStrategyNames: array[TForeignStrategy] of string = (
    ' ON DELETE SET NULL',    // fsOnDeleteSetNull
    ' ON DELETE SET DEFAULT', // fsOnDeleteSetDefault
    ' ON DELETE CASCADE',     // fsOnDeleteCascade
    ' ON DELETE NO ACTION',   // fsOnDeleteNoAction
    ' ON UPDATE SET NULL',    // fsOnUpdateSetNull
    ' ON UPDATE SET DEFAULT', // fsOnUpdateSetDefault
    ' ON UPDATE CASCADE',     // fsOnUpdateCascade
    ' ON UPDATE NO ACTION'    // fsOnUpdateNoAction
    );

  function GetMatchModeString(matchMode: TMatchMode; const pattern: string): string;
  function GetEndOperator(startOperator: TWhereOperator): TWhereOperator;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions;

function GetMatchModeString(matchMode: TMatchMode; const pattern: string): string;
const
  MATCH_CHAR = '%';
begin
  case matchMode of
    mmExact: Result := pattern;
    mmStart: Result := pattern + MATCH_CHAR;
    mmEnd: Result := MATCH_CHAR + pattern;
    mmAnywhere: Result := MATCH_CHAR + pattern + MATCH_CHAR;
  end;
  Result := QuotedStr(Result);
end;

function GetEndOperator(startOperator: TWhereOperator): TWhereOperator;
begin
  Result := startOperator;
  case startOperator of
    woOr: Result := woOrEnd;
    woAnd: Result := woAndEnd;
    woNot: Result := woNotEnd;
  end;
end;


{$REGION 'TSQLTable'}

class function TSQLTable.CreateFromClass(entityClass: TClass): TSQLTable;
var
  entityData: TEntityData;
begin
  if not Assigned(entityClass) then
    Exit(nil);

  entityData := TEntityCache.Get(entityClass);
  if not entityData.IsTableEntity then
    raise ETableNotSpecified.CreateFmt('Entity ("%s") is not a table', [entityClass.ClassName]);

  Result := TSQLTable.Create;
  Result.SetFromAttribute(entityData.EntityTable);
end;

function TSQLTable.GetAlias: string;
begin
  if fAlias = '' then
    fAlias := TSQLAliasGenerator.GetAlias(Name);
  Result := fAlias;
end;

function TSQLTable.GetFullTableName: string;
begin
  Result := Name + ' ' + Alias;
end;

function TSQLTable.GetName: string;
begin
  Result := '';
  if SchemaExists then
    Result := fSchema + '.';

  Result := Result + fName;
end;

function TSQLTable.GetNameWithoutSchema: string;
begin
  Result := fName;
end;

function TSQLTable.SchemaExists: Boolean;
begin
  Result := fSchema <> '';
end;

procedure TSQLTable.SetFromAttribute(const attribute: TableAttribute);
begin
  fName := attribute.TableName;
  fSchema := attribute.Schema;
end;

{$ENDREGION}


{$REGION 'TSQLField'}

constructor TSQLField.Create(const fieldName: string; const table: TSQLTable);
begin
  inherited Create;
  fFieldname := fieldName;
  fTable := table;
  fAlias := '';
end;

function TSQLField.GetAlias: string;
begin
  Result := fAlias;
end;

function TSQLField.GetEscapedFieldname(const escapeChar: Char): string;
var
  pos: Integer;
  fieldName, aliasName: string;
begin
  pos := PosEx(' ', fFieldname);
  if pos > 1 then
  begin
    //escape all words including alias name
    fieldName := GetEscapedName(Copy(fFieldname, 1, pos - 1), escapeChar);
    aliasName := Copy(fFieldname, pos, Length(fFieldname));
    Result := fieldName + aliasName;
  end
  else
    Result := GetEscapedName(fFieldname, escapeChar);
end;

function TSQLField.GetEscapedName(const name: string; const escapeChar: Char): string;
begin
  Result := AnsiQuotedStr(name, escapeChar);
end;

function TSQLField.GetFieldname: string;
begin
  Result := fFieldname;
end;

function TSQLField.GetFullFieldName(const escapeChar: Char): string;
begin
  if fAlias <> '' then
    Result := fAlias
  else
    Result := fTable.Alias + '.' + GetEscapedFieldname(escapeChar);
end;

function TSQLField.GetTable: TSQLTable;
begin
  Result := fTable;
end;

procedure TSQLField.SetAlias(const value: string);
begin
  fAlias := value;
end;

{$ENDREGION}


{$REGION 'TSQLJoin'}

constructor TSQLJoin.Create(const joinType: TSQLJoinType);
begin
  inherited Create;
  fJoinType := joinType;
  fSegments := TCollections.CreateObjectList<TSQLJoinSegment>;
end;

{$ENDREGION}


{$REGION 'TSQLJoinSegment'}

constructor TSQLJoinSegment.Create(const primaryKeyField, foreignKeyField: ISQLField);
begin
  inherited Create;
  fPrimaryKeyField := primaryKeyField;
  fForeignKeyField := foreignKeyField;
end;

{$ENDREGION}


{$REGION 'TSQLAliasGenerator'}

class constructor TSQLAliasGenerator.Create;
begin
  fAliases := TCollections.CreateDictionary<string,string>(100);
  fCharIndex := 65;
end;

class destructor TSQLAliasGenerator.Destroy;
begin
  fAliases := nil;
  inherited;
end;

class function TSQLAliasGenerator.AliasExists(const tableName: string): Boolean;
begin
  Result := fAliases.ContainsKey(tableName);
end;

class function TSQLAliasGenerator.GetAlias(const tableName: string): string;
var
  tblNameUpcase: string;
begin
  tblNameUpcase := UpperCase(tableName);
  if not AliasExists(tblNameUpcase) then
  begin
    Result := Chr(fCharIndex);
    fAliases.Add(tblNameUpcase, Result);
    Inc(fCharIndex);
  end
  else
    Result := fAliases[tblNameUpcase];
end;

{$ENDREGION}


{$REGION 'TSQLOrderField'}

constructor TSQLOrderByField.Create(const fieldName: string; const table: TSQLTable);
begin
  inherited Create(fieldName, table);
  fSortingDirection := stAscending;
end;

function TSQLOrderByField.GetFullOrderByFieldname(const escapeChar: Char): string;
begin
  Result := GetFullFieldName(escapeChar) + SortingDirectionNames[fSortingDirection];
end;

{$ENDREGION}


{$REGION 'TSQLWhereField'}

constructor TSQLWhereField.Create(const fieldName: string; const table: TSQLTable);
begin
  inherited Create(fieldName, table, nil, ':' + fieldName);
  fWhereOperator := woEqual;
  fMatchMode := mmExact;
end;

constructor TSQLWhereField.Create(const leftSQL, rightSQL: string);
begin
  Create('', nil);
  fWhereOperator := woOr;
  fMatchMode := mmExact;
  fLeftSQL := leftSQL;
  fRightSQL := rightSQL;
end;

function TSQLWhereField.ToSQLString(const escapeChar: Char): string;
begin
  case WhereOperator of
    woIsNull, woIsNotNull: Result := GetFullFieldName(escapeChar) + ' ' + WhereOperatorNames[WhereOperator];
    woLike, woNotLike, woIn, woNotIn: Result := GetFullFieldName(escapeChar);
    woOr, woAnd: Result := Format('(%s %s %s)', [fLeftSQL, WhereOperatorNames[WhereOperator], fRightSQL]);
    woNot: Result := Format('%s (%s)', [WhereOperatorNames[WhereOperator], fLeftSQL]);
    woOrEnd, woAndEnd, woNotEnd: Result := '';
    woJunction: Result := Format('(%s)', [fLeftSQL]);
    woBetween: Result := Format('(%s %s %s AND %s)', [GetFullFieldName(escapeChar), WhereOperatorNames[WhereOperator], ParamName, ParamName2]);
  else
    Result := GetFullFieldName(escapeChar) + ' ' + WhereOperatorNames[WhereOperator] + ' ' + ParamName + ' ';
  end;
end;

{$ENDREGION}


{$REGION 'TSQLCreateField'}

function TSQLCreateField.Clone: TSQLCreateField;
begin
  Result := TSQLCreateField.Create(fFieldname, fTable);
  Result.SetFromAttribute(fColumnAttribute);
end;

procedure TSQLCreateField.SetFromAttribute(const attribute: ColumnAttribute);
begin
  Assert(Assigned(attribute));
  fColumnAttribute := attribute;
  fProperties := attribute.Properties;
  fLength := attribute.Length;
  fScale := attribute.Scale;
  fDescription := attribute.Description;
  fPrecision := attribute.Precision;
  fIsIdentity := attribute.IsIdentity;
  if fIsIdentity then
    fIsPrimaryKey := fIsIdentity
  else
    fIsPrimaryKey := cpPrimaryKey in attribute.Properties;
  fTypeInfo := attribute.MemberType;
end;

{$ENDREGION}


{$REGION 'TSQLForeignKeyField'}

constructor TSQLForeignKeyField.Create(const fieldName: string;
  const table: TSQLTable; const referencedColumnName,
  referencedTableName: string; constraints: TForeignStrategies);
begin
  inherited Create(fieldname, table);
  fReferencedColumnName := referencedColumnName;
  fReferencedTableName := referencedTableName;
  fConstraints := constraints;
end;

function TSQLForeignKeyField.GetConstraintsAsString: string;
var
  constraint: TForeignStrategy;
begin
  Result := '';

  for constraint in fConstraints do
    Result := Result + ForeignStrategyNames[constraint];
end;

function TSQLForeignKeyField.GetForeignKeyName: string;
begin
  Result := Format('FK_%0:s_%1:s', [fTable.GetNameWithoutSchema, Fieldname]);
end;

{$ENDREGION}


{$REGION 'TSQLWherePropertyField'}

constructor TSQLWherePropertyField.Create(const leftPropertyName,
  rightPropertyName: string; const leftTable, rightTable: TSQLTable);
begin
  inherited Create(leftPropertyName, rightPropertyName);
  fTable := leftTable;
  fOtherTable := rightTable;
end;

function TSQLWherePropertyField.GetFullLeftFieldName: string;
begin
  Result := fTable.Alias + '.' + fLeftSQL;
end;

function TSQLWherePropertyField.GetFullRightFieldName: string;
begin
  Result := fOtherTable.Alias + '.' + fRightSQL;
end;

function TSQLWherePropertyField.ToSQLString(const escapeChar: Char): string;
begin
  Result := Format('%s %s %s', [GetFullLeftFieldname, WhereOperatorNames[fWhereOperator], GetFullRightFieldname]);
end;

{$ENDREGION}


{$REGION 'TSQLParamField'}

constructor TSQLParamField.Create(const fieldName: string;
  const table: TSQLTable; const column: ColumnAttribute; const paramName: string);
begin
  inherited Create(fieldName, table);
  fColumn := column;
  fParamName := paramName;
end;

{$ENDREGION}


end.
