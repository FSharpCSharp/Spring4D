unit Spring.Persistence.SQL.Generators.MongoDB;

interface

uses
  SvSerializer,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Abstract,
  Spring.Persistence.SQL.Generators.NoSQL,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>MongoDB</b> query generator.
  /// </summary>
  TMongoDBGenerator = class(TNoSQLGenerator)
  private
    class var fSerializerFormat: TSvSerializeFormat;

    class constructor Create;
  protected
    function GetExpressionFromWhereField(AField: TSQLWhereField; AFieldIndex: Integer): string; virtual;
    function ResolveFieldAndExpression(const AFieldname: string; out AField: string; out AExpression: string; const ADelta: Integer = 1): Boolean;
    function GetPrefix(ATable: TSQLTable): string; virtual;
    function GetSortingDirection(sortingDirection: TSortingDirection): string; virtual;
    function WrapResult(const AResult: string): string; virtual;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateUniqueId: Variant; override;
    function GetUpdateVersionFieldQuery(const command: TUpdateCommand;
      const versionColumn: VersionAttribute; const version, primaryKey: Variant): Variant; override;

    function GenerateSelect(const command: TSelectCommand): string; override;
    function GenerateInsert(const command: TInsertCommand): string; override;
    function GenerateUpdate(const commmand: TUpdateCommand): string; override;
    function GenerateDelete(const command: TDeleteCommand): string; override;

    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GenerateGetQueryCount(const sql: string): string; override;
    function GetSQLTableCount(const tableName: string): string; override;

    class function GetParamName(index: Integer): string;

    class property SerializerFormat: TSvSerializeFormat read fSerializerFormat write fSerializerFormat;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.Utils
  ,SvSerializerSuperJson
  ,SysUtils
  ,StrUtils
  ,Math
  ,Spring.Persistence.SQL.Register
  ,mongoID
  ,MongoBson
  ,Variants
  ;

{ TMongoDBGenerator }

class constructor TMongoDBGenerator.Create;
begin
  fSerializerFormat := sstSuperJson;
end;

function TMongoDBGenerator.GenerateDelete(
  const command: TDeleteCommand): string;
begin
  Result := 'D' + GetPrefix(command.Table) +'{"_id": '+ GetParamName(0) + '}';
end;

function TMongoDBGenerator.GenerateGetQueryCount(const sql: string): string;
begin
  Result := 'count' + Copy(sql, 2, Length(sql));
end;

function TMongoDBGenerator.GenerateInsert(
  const command: TInsertCommand): string;
begin
  if (command.Entity = nil) then
    Exit('');
  TSvSerializer.SerializeObject(command.Entity, Result, fSerializerFormat);
  Result := 'I' + GetPrefix(command.Table) + Result;
end;

function TMongoDBGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
begin
  Result := Format('page%d_%d_%s', [limit, offset, Copy(sql, 2, Length(sql))]);
end;

function TMongoDBGenerator.GenerateSelect(
  const command: TSelectCommand): string;
var
  LField, LPrevField: TSQLWhereField;
  i, LFieldIndex: Integer;
  LStmtType: string;
begin
  Result := '';
  LStmtType := 'S';
  LFieldIndex := 0;
  for i := 0 to command.WhereFields.Count - 1 do
  begin
    LField := command.WhereFields[i];
    LPrevField := command.WhereFields[Max(0, i - 1)];

    if not (LPrevField.WhereOperator in StartOperators) and not (LField.WhereOperator in EndOperators)  then
    begin
      if i <> 0 then
        Result := Result + ',';
    end;

    if (LField.WhereOperator in StartEndOperators) then
    begin
      Dec(LFieldIndex);
    end;

    Result := Result + GetExpressionFromWhereField(LField, LFieldIndex);
    Inc(LFieldIndex);
  end;

  for i := 0 to command.OrderByFields.Count - 1 do
  begin
    if i<>0 then
      LStmtType := LStmtType + ','
    else
    begin
      LStmtType := 'SO';
    end;

    LStmtType := LStmtType + '{' + AnsiQuotedStr(command.OrderByFields[i].Fieldname, '"') + ': ' +
      GetSortingDirection(command.OrderByFields[i].SortingDirection) + '}';
  end;
  if Length(LStmtType) > 1 then
  begin
    Insert(IntToStr(Length(LStmtType)-2) + '_', LStmtType, 3); //insert length
  end;

  Result := WrapResult(Result);
  Result := LStmtType + GetPrefix(command.Table) + Result;
end;

function TMongoDBGenerator.GenerateUniqueId: Variant;
begin
  Result := mongoObjectId;
end;

function TMongoDBGenerator.GenerateUpdate(
  const commmand: TUpdateCommand): string;
begin
  if (commmand.Entity = nil) then
    Exit('');
  TSvSerializer.SerializeObject(commmand.Entity, Result, fSerializerFormat);
  Result := 'U' + GetPrefix(commmand.Table) + Result;
end;

const
  WhereOpNames: array[TWhereOperator] of string = (
    {woEqual =} '=', {woNotEqual =} '$ne', {woMore = }'$gt', {woLess = }'$lt', {woLike = }'$regex', {woNotLike = }'',
    {woMoreOrEqual = }'$gte', {woLessOrEqual = }'$lte', {woIn = }'$in', {woNotIn = }'$nin', {woIsNull} '', {woIsNotNull} ''
    ,{woOr}'$or', {woOrEnd}'', {woAnd} '$and', {woAndEnd}'', {woNot}'$not', {woNotEnd}'',{woBetween}'', {woJunction} ''
    );

function TMongoDBGenerator.GetExpressionFromWhereField(
  AField: TSQLWhereField; AFieldIndex: Integer): string;
var
  LField, LExpression: string;
begin
  case AField.WhereOperator of
    woEqual: Result := '{' + AnsiQuotedStr(AField.Fieldname, '"') + ' : ' + GetParamName(AFieldIndex) + '}';
    woNotEqual, woMoreOrEqual, woMore, woLess, woLessOrEqual :
      Result := Format('{%S: { %S: %S}}', [AnsiQuotedStr(AField.Fieldname, '"'), WhereOpNames[AField.WhereOperator], GetParamName(AFieldIndex)]);
    woIsNotNull: Result := Format('{%S: { $ne: null }}', [AnsiQuotedStr(AField.Fieldname, '"')]);
    woIsNull: Result := Format('{%S: null}', [AnsiQuotedStr(AField.Fieldname, '"')]);
    woBetween: Result := Format('{$and: [ { %0:S: { $gte: %1:S} }, { %0:S: { $lte: %2:S} } ] }'
      , [AnsiQuotedStr(AField.Fieldname, '"'), GetParamName(AFieldIndex), GetParamName(AFieldIndex + 1)]);
    woOr, woAnd:
    begin
        Result := Format('{%S: [', [WhereOpNames[AField.WhereOperator]]);
    end;
    woNot: Result := Format('%S: ', [WhereOpNames[AField.WhereOperator]]);
    woNotEnd: Result := '';
    woOrEnd, woAndEnd: Result := ']}';
    woLike:
    begin
      Result := AField.Fieldname;
      if ResolveFieldAndExpression(AField.Fieldname, LField, LExpression) then
        Result := Format('{ %S: { $regex: ''.*%S.*'', $options: ''i''}}', [AnsiQuotedStr(LField, '"'), LExpression]);
    end;
    woNotLike:
    begin
      Result := AField.Fieldname;
      if ResolveFieldAndExpression(AField.Fieldname, LField, LExpression) then
        Result := Format('{ %S: { $not: "/.*%S.*/i"}}', [AnsiQuotedStr(LField, '"'), LExpression]);
    end;
    woIn, woNotIn:
    begin
      Result := AField.Fieldname;
      if ResolveFieldAndExpression(AField.Fieldname, LField, LExpression) then
        Result := Format('{%S: { %S: [%S] } }', [AnsiQuotedStr(LField, '"'), WhereOpNames[AField.WhereOperator], LExpression]);
    end;
  end;
end;

function TMongoDBGenerator.GetSortingDirection(sortingDirection: TSortingDirection): string;
begin
  Result := '1';
  case sortingDirection of
    stAscending: Result := '1';
    stDescending: Result := '-1';
  end;
end;

class function TMongoDBGenerator.GetParamName(index: Integer): string;
begin
  Result := '?' + IntToStr(index);
end;

function TMongoDBGenerator.GetPrefix(ATable: TSQLTable): string;
begin
  Result := '[' + ATable.Name + ']';
end;

function TMongoDBGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMongoDB;
end;

function TMongoDBGenerator.GetSQLTableCount(const tableName: string): string;
begin
  Result := 'count' + '[' + tableName + ']';
end;

function TMongoDBGenerator.GetUpdateVersionFieldQuery(
  const command: TUpdateCommand; const versionColumn: VersionAttribute;
  const version, primaryKey: Variant): Variant;
begin
  Result := BSON([command.PrimaryKeyColumn.Name, primaryKey, versionColumn.Name, version]);
end;

function TMongoDBGenerator.ResolveFieldAndExpression(const AFieldname: string;
  out AField, AExpression: string; const ADelta: Integer): Boolean;
var
  LPos, LPos2: Integer;
begin
  //Field NOT IN (1,2,3)
  LPos := PosEx(' ', AFieldname);
  AField := Copy(AFieldname, 1, LPos - 1);
  LPos := PosEx(' ', AFieldname, LPos + 1);
  LPos2 := PosEx(' ', AFieldname, LPos + 1);
  if LPos2 > 0 then
    LPos := LPos2;

  AExpression := Copy(AFieldname, LPos + 1 + ADelta, Length(AFieldname) - LPos - 1 - ADelta);
  Result := True;
end;

function TMongoDBGenerator.WrapResult(const AResult: string): string;
begin
  Result := AResult;
  if Length(Result) = 0 then
    Result := '{}'
  else
  begin
    if not StartsStr('{', Result) then
    begin
      Result := '{' + Result + '}';
    end;
  end;
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TMongoDBGenerator.Create);

end.
