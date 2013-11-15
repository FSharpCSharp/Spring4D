(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.Types;

interface

uses
  Generics.Collections, Mapping.Attributes, TypInfo;

const
  CRLF = #13#10;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Database table.
  ///	</summary>
  {$ENDREGION}
  TSQLTable = class
  private
    FName: string;
    FSchema: string;
    FDescription: string;
    FAlias: string;
    function GetAlias: string;
    procedure SetName(const Value: string);
    function GetName: string;
  public
    class function CreateFromClass(AEntityClass: TClass): TSQLTable;

    function SchemaExists(): Boolean;

    function GetNameWithoutSchema(): string;
    function GetFullTableName(): string;

    procedure SetFromAttribute(AAttribute: TableAttribute);

    property Alias: string read GetAlias write FAlias;
    property Description: string read FDescription write FDescription;
    property Name: string read GetName write SetName;
    property Schema: string read FSchema write FSchema;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table.
  ///	</summary>
  {$ENDREGION}
  ISQLField = interface
    ['{2316102E-61A3-4454-A7B2-18090C384882}']
    function GetFieldname: string;
    function GetFullFieldname(const AEscapeChar: Char): string;
    function GetTable: TSQLTable;
    function GetAlias: string;
    procedure SetAlias(const Value: string);
    property Alias: string read GetAlias write SetAlias;
    property Fieldname: string read GetFieldname;
    property Table: TSQLTable read GetTable;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table.
  ///	</summary>
  {$ENDREGION}
  TSQLField = class(TInterfacedObject, ISQLField)
  private
    FTable: TSQLTable;
    FFieldname: string;
    FAlias: string;
    function GetFieldname: string;
    function GetTable: TSQLTable;
    function GetAlias: string;
    procedure SetAlias(const Value: string);
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable); virtual;
    destructor Destroy; override;

    function GetFullFieldname(const AEscapeChar: Char): string; virtual;
    function GetEscapedName(const AName: string; const AEscapeChar: Char): string; virtual;
    function GetEscapedFieldname(const AEscapeChar: Char): string; virtual;

    property Alias: string read GetAlias write SetAlias;
    property Fieldname: string read GetFieldname write FFieldname;
    property Table: TSQLTable read GetTable write FTable;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in <c>Select</c>
  ///	  statements.
  ///	</summary>
  {$ENDREGION}
  TSQLSelectField = class(TSQLField)

  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in
  ///	  <c>Create Table</c> statements.
  ///	</summary>
  {$ENDREGION}
  TSQLCreateField = class(TSQLField)
  private
    FIsPrimaryKey: Boolean;
    FIsIdentity: Boolean;
    FTypeKindInfo: PTypeInfo;
    FLength: Integer;
    FScale: Integer;
    FProperties: TColumnProperties;
    FDescription: string;
    FPrecision: Integer;
    FColumnAttribute: ColumnAttribute;
  public
    procedure SetFromAttribute(AColumnAttr: ColumnAttribute); virtual;
    function Clone(): TSQLCreateField;

    property Description: string read FDescription;
    property IsPrimaryKey: Boolean read FIsPrimaryKey;
    property IsIdentity: Boolean read FIsIdentity;
    property TypeKindInfo: PTypeInfo read FTypeKindInfo write FTypeKindInfo;
    property Length: Integer read FLength;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
    property Properties: TColumnProperties read FProperties;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents foreign key field of the database table.
  ///	</summary>
  {$ENDREGION}
  TSQLForeignKeyField = class(TSQLField)
  private
    FReferencedColumnName: string;
    FConstraints: TForeignStrategies;
    FReferencedTableName: string;
    function GetForeignKeyName: string;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable
      ; const AReferencedColumnName, AReferencedTableName: string; AConstraints: TForeignStrategies); reintroduce; overload;

    function GetConstraintsAsString(): string;

    property ForeignKeyName: string read GetForeignKeyName;
    property ReferencedColumnName: string read FReferencedColumnName write FReferencedColumnName;
    property ReferencedTableName: string read FReferencedTableName write FReferencedTableName;
    property Constraints: TForeignStrategies read FConstraints write FConstraints;
  end;

  TMatchMode = (mmExact, mmStart, mmEnd, mmAnywhere);

  TWhereOperator = (woEqual = 0, woNotEqual, woMore, woLess, woLike, woNotLike,
    woMoreOrEqual, woLessOrEqual, woIn, woNotIn, woIsNull, woIsNotNull, woOr,
    woOrEnd, woAnd, woAndEnd, woNot, woNotEnd, woBetween, woJunction);

  TStartOperators = set of TWhereOperator;

const
  WhereOpNames: array[TWhereOperator] of string = (
    {woEqual =} '=', {woNotEqual =} '<>', {woMore = }'>', {woLess = }'<', {woLike = }'LIKE', {woNotLike = }'NOT LIKE',
    {woMoreOrEqual = }'>=', {woLessOrEqual = }'<=', {woIn = }'IN', {woNotIn = }'NOT IN', {woIsNull} 'IS NULL', {woIsNotNull} 'IS NOT NULL'
    ,{woOr}'OR', {woOrEnd}'', {woAnd} 'AND', {woAndEnd}'', {woNot}'NOT', {woNotEnd}'',{woBetween}'BETWEEN', {woJunction} ''
    );

  StartOperators: TStartOperators = [woOr, woAnd, woNot];

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in <c>where</c>
  ///	  clause.
  ///	</summary>
  {$ENDREGION}
  TSQLWhereField = class(TSQLField)
  private
    FWhereOperator: TWhereOperator;
    FMatchMode: TMatchMode;
    FLeftSQL: string;
    FRightSQL: string;
    FParamName: string;
    FParamName2: string;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable); overload; override;
    constructor Create(const ALeftSQL, ARightSQL: string); reintroduce; overload;

    function ToSQLString(const AEscapeChar: Char): string; virtual;

    property MatchMode: TMatchMode read FMatchMode write FMatchMode;
    property WhereOperator: TWhereOperator read FWhereOperator write FWhereOperator;
    property LeftSQL: string read FLeftSQL write FLeftSQL;
    property RightSQL: string read FRightSQL write FRightSQL;
    property ParamName: string read FParamName write FParamName;
    property ParamName2: string read FParamName2 write FParamName2;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in <c>where</c>
  ///	  clause.
  ///	</summary>
  {$ENDREGION}
  TSQLWherePropertyField = class(TSQLWhereField)
  private
    FOtherTable: TSQLTable;
  public
    constructor Create(const ALeftPropertyName, ARightPropertyName: string; ALeftTable, ARightTable: TSQLTable); overload;

    function GetFullLeftFieldname(): string; virtual;
    function GetFullRightFieldname(): string; virtual;

    function ToSQLString(const AEscapeChar: Char): string; override;

    property OtherTable: TSQLTable read FOtherTable write FOtherTable;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in <c>Group By</c>
  ///	  clause.
  ///	</summary>
  {$ENDREGION}
  TSQLGroupByField = class(TSQLField)

  end;

  TOrderType = (otAscending, otDescending);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents field of the database table which is used in <c>Order By</c>
  ///	  clause.
  ///	</summary>
  {$ENDREGION}
  TSQLOrderField = class(TSQLField)
  private
    FOrderType: TOrderType;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable); override;

    function GetFullOrderByFieldname(const AEscapeChar: Char): string;

    property OrderType: TOrderType read FOrderType write FOrderType;
  end;

  TSQLJoinType = (jtInner, jtLeft);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents join segment.
  ///	</summary>
  {$ENDREGION}
  TSQLJoinSegment = class
  private
    FPKField: ISQLField;
    FFKField: ISQLField;
  public
    constructor Create(const APKField: ISQLField; const AFKField: ISQLField); virtual;

    property PKField: ISQLField read FPKField write FPKField;
    property FKField: ISQLField read FFKField write FFKField;
  end;


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents join of database tables.
  ///	</summary>
  {$ENDREGION}
  TSQLJoin = class
  private
    FJoinType: TSQLJoinType;
    FSegments: TObjectList<TSQLJoinSegment>;
  public
    constructor Create(const AJoinType: TSQLJoinType); virtual;
    destructor Destroy; override;

    class function GetJoinTypeAsString(const AJoinType: TSQLJoinType): string;

    property JoinType: TSQLJoinType read FJoinType write FJoinType;
    property Segments: TObjectList<TSQLJoinSegment> read FSegments write FSegments;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Static class which is used to generate table aliases.
  ///	</summary>
  {$ENDREGION}
  TSQLAliasGenerator = class
  private
    class var FAliases: TDictionary<string,string>;
    class var FCharIndex: Byte;
  public
    class constructor Create();
    class destructor Destroy;

    class function AliasExists(const ATable: TSQLTable): Boolean;
    class function GetAlias(const ATable: TSQLTable): string;
  end;

  function GetMatchModeString(AMatchMode: TMatchMode; const APattern: string): string;
  function GetEndOperator(AStartOperator: TWhereOperator): TWhereOperator;

implementation

uses
  Core.Exceptions
  ,Core.EntityCache
  ,SysUtils
  ,StrUtils
  ;

function GetMatchModeString(AMatchMode: TMatchMode; const APattern: string): string;
const
  MATCH_CHAR = '%';
begin
  case AMatchMode of
    mmExact: Result := APattern;
    mmStart: Result := APattern + MATCH_CHAR;
    mmEnd: Result := MATCH_CHAR + APattern;
    mmAnywhere: Result := MATCH_CHAR + APattern + MATCH_CHAR;
  end;
  Result := QuotedStr(Result);
end;

function GetEndOperator(AStartOperator: TWhereOperator): TWhereOperator;
begin
  Result := AStartOperator;
  case AStartOperator of
    woOr: Result := woOrEnd;
    woAnd: Result := woAndEnd;
    woNot: Result := woNotEnd;
  end;
end;



{ TSQLTable }

class function TSQLTable.CreateFromClass(AEntityClass: TClass): TSQLTable;
var
  LEntityData: TEntityData;
begin
  if AEntityClass = nil then
    Exit(nil);

  LEntityData := TEntityCache.Get(AEntityClass);
  if not LEntityData.IsTableEntity then
    raise ETableNotSpecified.CreateFmt('Entity ("%S") is not a table', [AEntityClass.ClassName]);

  Result := TSQLTable.Create;
  Result.SetFromAttribute(LEntityData.EntityTable);
end;

function TSQLTable.GetAlias: string;
begin
  if (FAlias = '') then
  begin
    FAlias := TSQLAliasGenerator.GetAlias(Self);
  end;

  Result := FAlias;
end;

function TSQLTable.GetFullTableName: string;
begin
  Result := Name + ' ' + Alias;
end;

function TSQLTable.GetName: string;
begin
  Result := '';
  if SchemaExists then
    Result := Schema + '.';

  Result := Result + FName;
end;

function TSQLTable.GetNameWithoutSchema: string;
begin
  Result := FName;
end;

function TSQLTable.SchemaExists: Boolean;
begin
  Result := (FSchema <> '');
end;

procedure TSQLTable.SetFromAttribute(AAttribute: TableAttribute);
begin
  Name := AAttribute.TableName;
  Schema := AAttribute.Schema;
end;

procedure TSQLTable.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
  end;
end;

{ TSQLField }

constructor TSQLField.Create(const AFieldname: string; ATable: TSQLTable);
begin
  inherited Create;
  FFieldname := AFieldname;
  FTable := ATable;
  FAlias := '';
end;

destructor TSQLField.Destroy;
begin
  inherited Destroy;
end;

function TSQLField.GetAlias: string;
begin
  Result := FAlias;  
end;

function TSQLField.GetEscapedFieldname(const AEscapeChar: Char): string;
var
  LPos: Integer;
  LFieldname, LAliasName: string;
begin
  LPos := PosEx(' ', FFieldname);
  if LPos > 1 then
  begin
    //escape all words including alias name
    LFieldname := GetEscapedName(Copy(FFieldname, 1, LPos - 1), AEscapeChar);
    LAliasName := Copy(FFieldname, LPos, Length(FFieldname));
    Result := LFieldname + LAliasName;
  end
  else
    Result := GetEscapedName(FFieldname, AEscapeChar);
end;

function TSQLField.GetEscapedName(const AName: string; const AEscapeChar: Char): string;
begin
  Result := AnsiQuotedStr(AName, AEscapeChar);
end;

function TSQLField.GetFieldname: string;
begin
  Result := FFieldname;
end;

function TSQLField.GetFullFieldname(const AEscapeChar: Char): string;
begin
  if (FAlias <> '') then
    Result := FAlias
  else
    Result := Table.Alias + '.' + GetEscapedFieldname(AEscapeChar);
end;

function TSQLField.GetTable: TSQLTable;
begin
  Result := FTable;
end;

procedure TSQLField.SetAlias(const Value: string);
begin
  FAlias := Value;
end;

{ TSQLJoin }

constructor TSQLJoin.Create(const AJoinType: TSQLJoinType);
begin
  inherited Create;
  FJoinType := AJoinType;
  FSegments := TObjectList<TSQLJoinSegment>.Create;
end;

destructor TSQLJoin.Destroy;
begin
  FSegments.Free;
  inherited Destroy;
end;

class function TSQLJoin.GetJoinTypeAsString(const AJoinType: TSQLJoinType): string;
begin
  Result := '';
  case AJoinType of
    jtInner: Result := 'INNER JOIN';
    jtLeft: Result := 'LEFT OUTER JOIN'
    else
      raise EUnknownJoinType.Create('Unknown join type: ' + GetEnumName(TypeInfo(TSQLJoinType), Ord(AJoinType)));
  end;
end;

{ TSQLJoinSegment }

constructor TSQLJoinSegment.Create(const APKField, AFKField: ISQLField);
begin
  inherited Create;
  FPKField := APKField;
  FFKField := AFKField;
end;

{ TSQLAliasGenerator }

class function TSQLAliasGenerator.AliasExists(const ATable: TSQLTable): Boolean;
begin
  Result := FAliases.ContainsKey(ATable.Name);
end;

class constructor TSQLAliasGenerator.Create;
begin
  FAliases := TDictionary<string,string>.Create(100);
  FCharIndex := 65;
end;

class destructor TSQLAliasGenerator.Destroy;
begin
  FAliases.Free;
  inherited;
end;

class function TSQLAliasGenerator.GetAlias(const ATable: TSQLTable): string;
begin
  if not AliasExists(ATable) then
  begin
    Result := Chr(FCharIndex);
    FAliases.Add(ATable.Name, Result);
    Inc(FCharIndex);
  end
  else
  begin
    Result := FAliases[ATable.Name];
  end;
end;

{ TSQLOrderField }

constructor TSQLOrderField.Create(const AFieldname: string; ATable: TSQLTable);
begin
  inherited Create(AFieldname, ATable);
  FOrderType := otAscending;
end;

function TSQLOrderField.GetFullOrderByFieldname(const AEscapeChar: Char): string;
begin
  Result := GetFullFieldname(AEscapeChar);

  case FOrderType of
    otAscending:  Result := Result + ' ASC' ;
    otDescending: Result := Result + ' DESC';
  end;
end;

{ TSQLWhereField }

constructor TSQLWhereField.Create(const AFieldname: string; ATable: TSQLTable);
begin
  inherited Create(AFieldname, ATable);
  FWhereOperator := woEqual;
  FMatchMode := mmExact;
  FParamName := ':' + AFieldname;
end;

constructor TSQLWhereField.Create(const ALeftSQL, ARightSQL: string);
begin
  inherited Create('', nil);
  FWhereOperator := woOr;
  FMatchMode := mmExact;
  FLeftSQL := ALeftSQL;
  FRightSQL := ARightSQL;
end;

function TSQLWhereField.ToSQLString(const AEscapeChar: Char): string;
begin
  case WhereOperator of
    woIsNull, woIsNotNull: Result := GetFullFieldname(AEscapeChar) + ' ' + WhereOpNames[WhereOperator];
    woLike, woNotLike, woIn, woNotIn: Result := GetFullFieldname(AEscapeChar);
    woOr, woAnd: Result := Format('(%S %S %S)', [FLeftSQL, WhereOpNames[WhereOperator], FRightSQL]);
    woNot: Result := Format('%S (%S)', [WhereOpNames[WhereOperator], FLeftSQL]);
    woOrEnd, woAndEnd, woNotEnd: Result := '';
    woJunction: Result := Format('(%S)', [FLeftSQL]);
    woBetween: Result := Format('(%S %S %S AND %S)', [GetFullFieldname(AEscapeChar), WhereOpNames[WhereOperator], FParamName, FParamName2]);
    else
      Result := GetFullFieldname(AEscapeChar) + ' ' + WhereOpNames[WhereOperator] + ' ' + FParamName + ' ';
  end;
end;

{ TSQLCreateField }

function TSQLCreateField.Clone: TSQLCreateField;
begin
  Result := TSQLCreateField.Create(FFieldname, FTable);
  Result.SetFromAttribute(FColumnAttribute);
end;

procedure TSQLCreateField.SetFromAttribute(AColumnAttr: ColumnAttribute);
begin
  Assert(Assigned(AColumnAttr));
  FColumnAttribute := AColumnAttr;
  FProperties := AColumnAttr.Properties;
  FLength := AColumnAttr.Length;
  FScale := AColumnAttr.Scale;
  FDescription := AColumnAttr.Description;
  FPrecision := AColumnAttr.Precision;
  FIsIdentity := AColumnAttr.IsIdentity;
  if FIsIdentity then
    FIsPrimaryKey := FIsIdentity
  else
  begin
    FIsPrimaryKey := (cpPrimaryKey in AColumnAttr.Properties);
  end;
  FTypeKindInfo := AColumnAttr.GetColumnTypeInfo;
end;

{ TSQLForeignKeyField }

constructor TSQLForeignKeyField.Create(const AFieldname: string; ATable: TSQLTable; const AReferencedColumnName,
  AReferencedTableName: string; AConstraints: TForeignStrategies);
begin
  inherited Create(AFieldname, ATable);
  FReferencedColumnName := AReferencedColumnName;
  FReferencedTableName := AReferencedTableName;
  FConstraints := AConstraints;
end;

function TSQLForeignKeyField.GetConstraintsAsString: string;
var
  LConstraint: TForeignStrategy;
begin
  Result := '';

  for LConstraint in FConstraints do
  begin
    if LConstraint in FConstraints then
    begin
      case LConstraint of
        fsOnDeleteSetNull: Result := Result + ' ON DELETE SET NULL';
        fsOnDeleteSetDefault: Result := Result + ' ON DELETE SET DEFAULT';
        fsOnDeleteCascade: Result := Result + ' ON DELETE CASCADE';
        fsOnDeleteNoAction: Result := Result + ' ON DELETE NO ACTION';
        fsOnUpdateSetNull: Result := Result + ' ON UPDATE SET NULL';
        fsOnUpdateSetDefault: Result := Result + ' ON UPDATE SET DEFAULT';
        fsOnUpdateCascade: Result := Result + ' ON UPDATE CASCADE';
        fsOnUpdateNoAction: Result := Result + ' ON UPDATE NO ACTION';
      end;
    end;
  end;
end;

function TSQLForeignKeyField.GetForeignKeyName: string;
begin
  Result := Format('FK_%0:S_%1:S', [Table.GetNameWithoutSchema, Fieldname]);
end;

{ TSQLWherePropertyField }

constructor TSQLWherePropertyField.Create(const ALeftPropertyName, ARightPropertyName: string; ALeftTable,
  ARightTable: TSQLTable);
begin
  inherited Create(ALeftPropertyName, ARightPropertyName);
  Table := ALeftTable;
  FOtherTable := ARightTable;
end;

function TSQLWherePropertyField.GetFullLeftFieldname: string;
begin
  Result := Table.Alias + '.' + LeftSQL;
end;

function TSQLWherePropertyField.GetFullRightFieldname: string;
begin
  Result := FOtherTable.Alias + '.' + RightSQL;
end;

function TSQLWherePropertyField.ToSQLString(const AEscapeChar: Char): string;
begin
  Result := Format('%S %S %S', [GetFullLeftFieldname, WhereOpNames[WhereOperator], GetFullRightFieldname]);
end;

end.
