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
    function SchemaExists(): Boolean;

    function GetNameWithoutSchema(): string;
    function GetFullTableName(): string;

    procedure SetFromAttribute(AAttribute: TableAttribute);

    property Alias: string read GetAlias write FAlias;
    property Description: string read FDescription write FDescription;
    property Name: string read GetName write SetName;
    property Schema: string read FSchema write FSchema;
  end;

  ISQLField = interface
    ['{2316102E-61A3-4454-A7B2-18090C384882}']
    function GetFieldname: string;
    function GetFullFieldname(): string;
    function GetTable: TSQLTable;
    property Fieldname: string read GetFieldname;
    property Table: TSQLTable read GetTable;
  end;

  TSQLField = class(TInterfacedObject, ISQLField)
  private
    FTable: TSQLTable;
    FFieldname: string;
    FFreeTable: Boolean;
    function GetFieldname: string;
    function GetTable: TSQLTable;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable; const AFreeTable: Boolean = False); virtual;
    destructor Destroy; override;

    function GetFullFieldname(): string;


    property AutoFreeTable: Boolean read FFreeTable write FFreeTable;
    property Fieldname: string read GetFieldname write FFieldname;
    property Table: TSQLTable read GetTable write FTable;
  end;

  TSQLSelectField = class(TSQLField)

  end;

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

  TSQLForeignKeyField = class(TSQLField)
  private
    FReferencedColumnName: string;
    FConstraints: TForeignStrategies;
    FReferencedTableName: string;
    function GetForeignKeyName: string;
  public
    function GetConstraintsAsString(): string;

    property ForeignKeyName: string read GetForeignKeyName;
    property ReferencedColumnName: string read FReferencedColumnName write FReferencedColumnName;
    property ReferencedTableName: string read FReferencedTableName write FReferencedTableName;
    property Constraints: TForeignStrategies read FConstraints write FConstraints;
  end;

  TWhereOperator = (woEqual = 0, woNotEqual, woMore, woLess, woLike, woNotLike,
    woMoreOrEqual, woLessOrEqual, woIn, woNotIn);

const
  WhereOpNames: array[TWhereOperator] of string = (
    {woEqual =} '=', {woNotEqual =} '<>', {woMore = }'>', {woLess = }'<', {woLike = }'LIKE', {woNotLike = }'NOT LIKE',
    {woMoreOrEqual = }'>=', {woLessOrEqual = }'<=', {woIn = }'IN', {woNotIn = }'NOT IN');

type
  TSQLWhereField = class(TSQLField)
  private
    FWhereOperator: TWhereOperator;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable); overload;

    function ToSQLString(): string;

    property WhereOperator: TWhereOperator read FWhereOperator write FWhereOperator;
  end;

  TSQLGroupByField = class(TSQLField)

  end;

  TOrderType = (otAscending, otDescending);

  TSQLOrderField = class(TSQLField)
  private
    FOrderType: TOrderType;
  public
    constructor Create(const AFieldname: string; ATable: TSQLTable); overload;

    function GetFullOrderByFieldname(): string;
  
    property OrderType: TOrderType read FOrderType write FOrderType;
  end;

  TSQLJoinType = (jtInner, jtLeft);

  TSQLJoinSegment = class
  private
    FPKField: ISQLField;
    FFKField: ISQLField;
  public
    constructor Create(const APKField: ISQLField; const AFKField: ISQLField); virtual;

    property PKField: ISQLField read FPKField write FPKField;
    property FKField: ISQLField read FFKField write FFKField;
  end;


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

implementation

uses
  Core.Exceptions
  ,SysUtils
  ;



{ TSQLTable }

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

constructor TSQLField.Create(const AFieldname: string; ATable: TSQLTable; const AFreeTable: Boolean);
begin
  inherited Create;
  FFieldname := AFieldname;
  FTable := ATable;
  FFreeTable := AFreeTable;
end;

destructor TSQLField.Destroy;
begin
  if FFreeTable then
    FTable.Free;
  inherited Destroy;
end;

function TSQLField.GetFieldname: string;
begin
  Result := FFieldname;
end;

function TSQLField.GetFullFieldname: string;
begin
  Result := Table.Alias + '.' + Fieldname;
end;

function TSQLField.GetTable: TSQLTable;
begin
  Result := FTable;
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

function TSQLOrderField.GetFullOrderByFieldname: string;
begin
  Result := GetFullFieldname;

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
end;

function TSQLWhereField.ToSQLString: string;
begin
  Result := GetFullFieldname + ' ' + WhereOpNames[WhereOperator] + ' :' + Fieldname + ' ';
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
  Result := Format('FK_%0:S_%1:S', [Table.Name, Fieldname]);
end;

end.
