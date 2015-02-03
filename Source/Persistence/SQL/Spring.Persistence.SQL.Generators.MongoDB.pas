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

unit Spring.Persistence.SQL.Generators.MongoDB;

interface

uses
  Rtti,
  SysUtils,
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
    class var fsJson: TFormatSettings;
    class constructor Create;
  protected
    function GetExpressionFromWhereField(AField: TSQLWhereField; AFieldIndex: Integer): string; virtual;
    function ResolveFieldAndExpression(const AFieldname: string; out AField: string; out AExpression: string; const ADelta: Integer = 1): Boolean;
    function GetPrefix(ATable: TSQLTable): string; virtual;
    function GetSortingDirection(sortingDirection: TSortingDirection): string; virtual;
    function WrapResult(const AResult: string): string; virtual;

    function DoGetInsertJson(const command: TInsertCommand): string;
    function DoGetUpdateJson(const command: TUpdateCommand): string;
    function CreateClassInsertCommandAndTable(const fromValue: TValue): TInsertCommand;
    function CreateClassUpdateCommandAndTable(const fromValue: TValue): TUpdateCommand;    
    function ToJsonValue(const value: TValue): string;
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
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.Utils
  ,Spring.Persistence.Core.EntityCache
  ,Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.Core.EntityWrapper
  ,Spring.Collections
  ,Spring.Reflection
  ,StrUtils
  ,Math
  ,Spring.Persistence.SQL.Register
  ,mongoID
  ,MongoBson
  ,Variants
  ,TypInfo
  ;

{ TMongoDBGenerator }

class constructor TMongoDBGenerator.Create;
begin
  fsJson := TFormatSettings.Create;
  fsJson.DecimalSeparator := '.';
  fsJson.ShortDateFormat := 'yyyy-mm-dd';
  fsJson.DateSeparator := '-';
  fsJson.TimeSeparator := ':';
  fsJson.LongDateFormat := 'yyyy-mm-dd hh:mm:ss';
end;



function TMongoDBGenerator.CreateClassInsertCommandAndTable(
  const fromValue: TValue): TInsertCommand;
var
  fields: IList<ColumnAttribute>;
  entity: IEntityWrapper;
  entityObject: TObject;
  table: TSQLTable;
begin
  entityObject := fromValue.AsObject;
  entity := TEntityWrapper.Create(entityObject);
  
  table := TSQLTable.Create;
  table.Name := entityObject.ClassName;   
   
  fields := TCollections.CreateList<ColumnAttribute>;
  fields.AddRange(entity.GetColumns);
  
  Result := TInsertCommand.Create(table);
  Result.Entity := entityObject;
  Result.SetCommandFieldsFromColumns(fields);
end;

function TMongoDBGenerator.CreateClassUpdateCommandAndTable(
  const fromValue: TValue): TUpdateCommand;
var
  fields: IList<ColumnAttribute>;
  entity: IEntityWrapper;
  entityObject: TObject;
  table: TSQLTable;
begin
  entityObject := fromValue.AsObject;
  entity := TEntityWrapper.Create(entityObject);
  
  table := TSQLTable.Create;
  table.Name := entityObject.ClassName;   
   
  fields := TCollections.CreateList<ColumnAttribute>;
  fields.AddRange(entity.GetColumns);
  
  Result := TUpdateCommand.Create(table);
  Result.Entity := entityObject;
  Result.SetCommandFieldsFromColumns(fields);  
end;

function TMongoDBGenerator.DoGetInsertJson(
  const command: TInsertCommand): string;
var
  i, j: Integer;
  insertField: TSQLInsertField;
  classCommand: TInsertCommand;
  list: IList;
  current: TValue;

  function GetJsonValueFromClass(const value: TValue): string;
  begin
    Result := 'null';
    if value.AsObject = nil then
      Exit;  

    classCommand := CreateClassInsertCommandAndTable(value);
    try
      Result := DoGetInsertJson(classCommand);
    finally
      classCommand.Table.Free;
      classCommand.Free;
    end;  
  end;

begin
  Result := '{';
  for i := 0 to command.InsertFields.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ',';

    insertField := command.InsertFields[i];
    case insertField.Column.MemberType.Kind of
      tkClass:
      begin
        Result := Result + QuotedStr(insertField.Fieldname) + ': ';
        current := insertField.Column.GetValue(command.Entity);
        Result := Result + GetJsonValueFromClass(current);         
      end;
      tkInterface:
      begin
        Result := Result + QuotedStr(insertField.Fieldname) + ': [';
        list := insertField.Column.GetValue(command.Entity).AsInterface as IList;
        for j := 0 to list.Count - 1 do
        begin
          if j <> 0 then
            Result := Result + ',';

          current := list[j];  
          if list.ElementType.Kind = tkClass then
            Result := Result + GetJsonValueFromClass(current)                              
          else
            Result := Result + ToJsonValue(current);
        end;
        Result := Result + ']';
      end
      else
      begin
        current := TValue.Empty;
        if command.Entity <> nil then
          current := insertField.Column.GetValue(command.Entity);
          
        Result := Result + QuotedStr(insertField.Fieldname) + ': ' 
          + ToJsonValue(current);
      end;          
    end;
  end;
  Result := Result + '}';
end;

function TMongoDBGenerator.DoGetUpdateJson(
  const command: TUpdateCommand): string;
var
  i, j: Integer;
  updateField: TSQLUpdateField;
  classCommand: TUpdateCommand;
  list: IList;
  current: TValue;

  function GetJsonValueFromClass(const value: TValue): string;
  begin
    Result := 'null';
    if value.AsObject = nil then
      Exit;  

    classCommand := CreateClassUpdateCommandAndTable(value);
    try
      Result := DoGetUpdateJson(classCommand);
    finally
      classCommand.Table.Free;
      classCommand.Free;
    end;  
  end;

begin
  Result := '{';
  for i := 0 to command.UpdateFields.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ',';

    updateField := command.UpdateFields[i];
    case updateField.Column.MemberType.Kind of
      tkClass:
      begin
        Result := Result + QuotedStr(updateField.Fieldname) + ': ';
        current := updateField.Column.GetValue(command.Entity);
        Result := Result + GetJsonValueFromClass(current);         
      end;
      tkInterface:
      begin
        Result := Result + QuotedStr(updateField.Fieldname) + ': [';
        list := updateField.Column.GetValue(command.Entity).AsInterface as IList;
        for j := 0 to list.Count - 1 do
        begin
          if j <> 0 then
            Result := Result + ',';

          current := list[j];  
          if list.ElementType.Kind = tkClass then
            Result := Result + GetJsonValueFromClass(current)                              
          else
            Result := Result + ToJsonValue(current);
        end;
        Result := Result + ']';
      end
      else
      begin
        current := TValue.Empty;
        if command.Entity <> nil then
          current := updateField.Column.GetValue(command.Entity);
          
        Result := Result + QuotedStr(updateField.Fieldname) + ': ' 
          + ToJsonValue(current);
      end;          
    end;
  end;
  Result := Result + '}';
  
end;

function TMongoDBGenerator.GenerateDelete(
  const command: TDeleteCommand): string;
begin
  Result := 'D' + GetPrefix(command.Table) +'{"_id": '+ command.WhereFields.First.ParamName + '}';
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
  Result := DoGetInsertJson(command);
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
  Result := DoGetUpdateJson(commmand);  
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
    woEqual: Result := '{' + AnsiQuotedStr(AField.Fieldname, '"') + ' : ' + AField.ParamName + '}';
    woNotEqual, woMoreOrEqual, woMore, woLess, woLessOrEqual :
      Result := Format('{%S: { %S: %S}}', [AnsiQuotedStr(AField.Fieldname, '"'), WhereOpNames[AField.WhereOperator], AField.ParamName]);
    woIsNotNull: Result := Format('{%S: { $ne: null }}', [AnsiQuotedStr(AField.Fieldname, '"')]);
    woIsNull: Result := Format('{%S: null}', [AnsiQuotedStr(AField.Fieldname, '"')]);
    woBetween: Result := Format('{$and: [ { %0:S: { $gte: %1:S} }, { %0:S: { $lte: %2:S} } ] }'
      , [AnsiQuotedStr(AField.Fieldname, '"'), AField.ParamName, AField.ParamName2]);
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
  Result := BSON([command.PrimaryKeyColumn.ColumnName, primaryKey, versionColumn.ColumnName, version]);
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

function IsObjectId(const value: string): Boolean;
begin
  Result := StartsText('ObjectID("', value);
end;

function TMongoDBGenerator.ToJsonValue(const value: TValue): string;
var
  variantValue: Variant;
begin
  Result := 'null';
  if value.IsEmpty then
    Exit;

  variantValue := TUtils.AsVariant(value);    
  case VarType(variantValue) of
    varString, varUString, varStrArg, varOleStr:
    begin
      Result := VarToStrDef(variantValue, 'null');
      if IsObjectId(Result) then   //ObjectID("sdsd457845")
        Result := '"' + ReplaceStr(Result, '"', '\"') + '"'
      else
        Result := AnsiQuotedStr(Result, '"');
    end;
    varBoolean:
    begin
      if Boolean(variantValue) then
        Result := 'true'
      else
        Result := 'false';
    end;
    varDouble:
      Result := FloatToStr(variantValue, fsJson);
    varDate:
      Result := DateTimeToStr(variantValue, fsJson);
    else
      Result := VarToStrDef(variantValue, 'null');
  end;    
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
