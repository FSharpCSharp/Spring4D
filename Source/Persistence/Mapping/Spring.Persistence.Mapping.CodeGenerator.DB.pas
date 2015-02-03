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

unit Spring.Persistence.Mapping.CodeGenerator.DB;

interface

uses
  Spring.Persistence.Mapping.CodeGenerator.Abstract
  ,Generics.Collections
  ,ADODB
  ,DB
  ,Classes
  ;

type
  TEntityModelDataLoader = class
  private
    FEntities: TObjectList<TEntityModelData>;
    FConnected: Boolean;
    FDefaultSchemaName: string;
    FDBConnection: TADOConnection;
    FConnectionString: string;
    FPrimaryKeys: TDictionary<string, Boolean>;
    FDatabaseName: string;
    FOutputDir: string;
    FUnitPrefix: string;
    FUseNullableTypes: Boolean;
  protected
    function CreateEntityDataFromFields(AFields: TFields; const ATableName: string): TEntityModelData;
    function GetFieldTypeName(AField: TField): string;
    procedure GetTables(var AList: TStrings; ASystemTables: Boolean = False);
    procedure ParseTableName(const ATableName: string; out ATable, ASchema: string);

    function GetIsAutoIncField(ADataset: TADODataSet; const AColumnName: string): Boolean;
    function GetIsPrimaryKeyField(ADataset: TADODataSet; const AColumnName: string): Boolean;
    function GetPrimKeyFindKey(const ATable, ASchema, AColumn: string): string;
    procedure LoadPrimaryKeys;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Connect: Boolean;
    function GetUnitName(const ATableName: string): string;

    function Execute: Boolean;

    procedure LoadTables;
    //serializable properties
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property DefaultSchemaName: string read FDefaultSchemaName write FDefaultSchemaName;
    property Connected: Boolean read FConnected;
    property ConnectionString: string read FConnectionString write FConnectionString;
    property OutputDir: string read FOutputDir write FOutputDir;
    property UnitPrefix: string read FUnitPrefix write FUnitPrefix;
    property UseNullableTypes: Boolean read FUseNullableTypes write FUseNullableTypes;
    [SvTransient]
    property Entities: TObjectList<TEntityModelData> read FEntities;
  end;

implementation

uses
  SysUtils
  ,Variants
  ,StrUtils
  ;

{ TEntityModelDataLoader }

function TEntityModelDataLoader.Connect: Boolean;
begin
  FConnected := False;

  if FDBConnection.Connected then
    FDBConnection.Close;

  FDBConnection.ConnectionString := FConnectionString;
  FDBConnection.Open;

  FConnected := FDBConnection.Connected;
  Result := FConnected;
end;

constructor TEntityModelDataLoader.Create;
begin
  inherited Create;
  FEntities := TObjectList<TEntityModelData>.Create(True);

  FUnitPrefix := 'ORM.Model.';

  FDBConnection := TADOConnection.Create(nil);
  FDBConnection.LoginPrompt := False;
  FPrimaryKeys := TDictionary<string, Boolean>.Create;
end;

function TEntityModelDataLoader.CreateEntityDataFromFields(AFields: TFields; const ATableName: string): TEntityModelData;
var
  LField, LNameField, LIsNullableField, LPrecisionField, LScaleField, LCharLength, LDescrField: TField;
  LColumnData: TColumnData;
  LTable, LSchema: string;
 // LOut: Boolean;
  LDataset: TADODataSet;
  VDBName, VSchemaName, VTableName: Variant;
begin
  Result := TEntityModelData.Create;
  ParseTableName(ATableName, LTable, LSchema);
  Result.TableName := LTable;
  Result.SchemaName := LSchema;

  LDataset := TADODataSet.Create(nil);
  try
    if (LSchema <> '') then
      VSchemaName := LSchema;

    if (DatabaseName <> '') then
      VDBName := DatabaseName;

    VTableName := LTable;

    FDBConnection.OpenSchema(siColumns, VarArrayOf([VDBName, VSchemaName, VTableName]), EmptyParam, LDataset);

    LNameField := LDataset.FieldByName('COLUMN_NAME');
    LIsNullableField := LDataset.FieldByName('IS_NULLABLE');
    LPrecisionField := LDataset.FieldByName('NUMERIC_PRECISION');
    LScaleField := LDataset.FieldByName('NUMERIC_SCALE');
    LCharLength := LDataset.FieldByName('CHARACTER_MAXIMUM_LENGTH');
    LDescrField := LDataset.FieldByName('DESCRIPTION');

    while not LDataset.Eof do
    begin
      LColumnData := TColumnData.Create;
      LColumnData.ColumnName := LNameField.AsString;
      LColumnData.IsRequired := not LIsNullableField.AsBoolean;
      LColumnData.NotNull := not LIsNullableField.AsBoolean;

      if not LPrecisionField.IsNull then
        LColumnData.ColumnPrecision := LPrecisionField.AsInteger;

      if not LScaleField.IsNull then
        LColumnData.ColumnScale := LScaleField.AsInteger;

      if not LCharLength.IsNull then
        LColumnData.ColumnLength := LCharLength.AsInteger;

      if not LDescrField.IsNull then
        LColumnData.ColumnDescription := LDescrField.AsString;

      LField := AFields.FindField(LColumnData.ColumnName);
      if Assigned(LField) then
      begin
        LColumnData.IsPrimaryKey := GetIsPrimaryKeyField(LField.DataSet as TADODataSet, LColumnData.ColumnName);
        LField.Required := LColumnData.IsRequired;
        LColumnData.ColumnTypeName := GetFieldTypeName(LField);
        LColumnData.DontUpdate := LField.ReadOnly;
        LColumnData.IsAutogenerated := GetIsAutoIncField(LField.DataSet as TADODataSet, LColumnData.ColumnName);
      end;

      Result.Columns.Add(LColumnData);

      LDataset.Next;
    end;
  finally
    LDataset.Free;
  end;
end;

destructor TEntityModelDataLoader.Destroy;
begin
  FEntities.Free;
  FDBConnection.Free;
  FPrimaryKeys.Free;
  inherited Destroy;
end;

function TEntityModelDataLoader.Execute: Boolean;
begin
  Result := False;
end;

function TEntityModelDataLoader.GetFieldTypeName(AField: TField): string;
begin
  Result := FieldTypeNames[AField.DataType];

  case AField.DataType of
    ftBytes, ftVarBytes, ftBlob, ftMemo, ftFmtMemo, ftOraBlob, ftOraClob, ftStream, ftObject:
      Result := 'TMemoryStream';
    ftGraphic: Result := 'TPicture';
    ftFMTBcd:
    begin
      if TFMTBCDField(AField).Size = 0 then
        Result := 'Int64'
      else
        Result := 'Double';
    end;
    ftBCD:
    begin
      if TBCDField(AField).Size = 0 then
        Result := 'Int64'
      else
        Result := 'Double';
    end;
    ftBoolean: Result := 'Boolean';
    ftDate: Result := 'TDate';
    ftDateTime: Result := 'TDateTime';
    ftTime: Result := 'TTime';
    ftSmallint: Result := 'SmallInt';
    ftInteger: Result := 'Integer';
    ftWord: Result := 'Word';
    ftFloat: Result := 'Double';
    ftCurrency: Result := 'Currency';
    ftAutoInc: Result := 'Int64';
    ftLongWord: Result := 'LongWord';
    ftGuid: Result := 'TGuid';
    ftShortint: Result := 'ShortInt';
    ftByte: Result := 'Byte';
    ftExtended: Result := 'Extended';
    ftLargeint: Result := 'Int64';
    ftWideMemo: Result := 'string';
    ftString, ftWideString, ftFixedWideChar, ftFixedChar: Result := 'string';
  end;
end;

function TEntityModelDataLoader.GetIsAutoIncField(ADataset: TADODataSet;
  const AColumnName: string): Boolean;
var
  i: Integer;
  LProps: Properties;
  LPropName: string;
begin
  LProps := ADataset.Recordset.Fields.Item[AColumnName].Properties;

  for i := 0 to LProps.Count - 1 do
  begin
    LPropName := LowerCase( string(LProps.Item[i].Name));
    if PosEx('autoincrement', LPropName) > 0 then
    begin
      Exit( (LProps.Item[i].Value));
    end;
  end;
  Result := False;
end;

function TEntityModelDataLoader.GetIsPrimaryKeyField(ADataset: TADODataSet;
  const AColumnName: string): Boolean;
var
  i: Integer;
  LProps: Properties;
  LPropName: string;
begin
  LProps := ADataset.Recordset.Fields.Item[AColumnName].Properties;

  for i := 0 to LProps.Count - 1 do
  begin
    LPropName := LowerCase( string(LProps.Item[i].Name));
    if PosEx('keycolumn', LPropName) > 0 then
    begin
      Exit(Boolean(LProps.Item[i].Value));
    end;
  end;
  Result := False;
end;

function TEntityModelDataLoader.GetPrimKeyFindKey(const ATable, ASchema, AColumn: string): string;
begin
  Result := UpperCase(ASchema + '.' + ATable + '_' + AColumn);
end;

procedure TEntityModelDataLoader.GetTables(var AList: TStrings; ASystemTables: Boolean);
var
  LTypeField,
  LNameField
  ,LSchemaField: TField;
  LTableType: WideString;
  LTablename: string;
  LDataSet: TADODataSet;
begin
  if not FConnected then
    Exit;

  LDataSet := TADODataSet.Create(nil);
  try
    FDBConnection.OpenSchema(siTables, EmptyParam, EmptyParam, LDataSet);
    LTypeField := LDataSet.FieldByName('TABLE_TYPE'); { do not localize }
    LNameField := LDataSet.FieldByName('TABLE_NAME'); { do not localize }
    LSchemaField := LDataSet.FieldByName('TABLE_SCHEMA'); { do not localize }
    AList.BeginUpdate;
    try
      AList.Clear;
      while not LDataSet.EOF do
      begin
        LTableType := LTypeField.AsWideString;
        if (LTableType = 'TABLE') or
           (ASystemTables and (LTableType = 'SYSTEM TABLE')) then
        begin
          LTablename := LNameField.AsString;
          if (LSchemaField.AsString <> '') then
            LTablename := LSchemaField.AsString + '.' + LTablename;
          AList.Add(LTablename);
        end;
        LDataSet.Next;
      end;
    finally
      AList.EndUpdate;
    end;
  finally
    LDataSet.Free;
  end;
end;

function TEntityModelDataLoader.GetUnitName(const ATableName: string): string;
begin
  Result := FUnitPrefix + ATableName;
end;

procedure TEntityModelDataLoader.LoadPrimaryKeys;
var
  LDataset: TADODataSet;
  LTablenameField, LColumnNameField, LSchemaField: TField;
  VDBName, VSchemaName, VTableName: Variant;
  LTables: TStrings;
  LFullTable, LTable, LSchema: string;
begin
  FPrimaryKeys.Clear;

  LDataset := TADODataSet.Create(nil);
  LTables := TStringList.Create;
  try
    GetTables(LTables);

    if (DatabaseName <> '') then
      VDBName := DatabaseName;

    for LFullTable in LTables do
    begin
      ParseTableName(LFullTable, LTable, LSchema);

      if (LSchema <> '') then
        VSchemaName := LSchema;

      VTableName := LTable;

      FDBConnection.OpenSchema(siPrimaryKeys, VarArrayOf([ VDBName , VSchemaName , VTableName ])
        , EmptyParam, LDataset);

      LTablenameField := LDataset.FieldByName('TABLE_NAME');
      LColumnNameField := LDataset.FieldByName('COLUMN_NAME');
      LSchemaField := LDataset.FieldByName('TABLE_SCHEMA');

      while not LDataset.Eof do
      begin
        FPrimaryKeys.Add(
          GetPrimKeyFindKey(LTablenameField.AsString, LSchemaField.AsString,
            LColumnNameField.AsString), True);
        LDataset.Next;
      end;
    end;
  finally
    LDataset.Free;
    LTables.Free;
  end;
end;

procedure TEntityModelDataLoader.LoadTables;
var
  LTables: TStrings;
  LTableName: string;
  LDataset: TADODataSet;
  LEntityModel: TEntityModelData;
begin
  FEntities.Clear;

  if not FConnected then
    if not Connect then
      Exit;

  LTables := TStringList.Create;
  LDataset := TADODataSet.Create(nil);
  try
    LDataset.Connection := FDBConnection;
    LDataset.DisableControls;
    GetTables(LTables);

    for LTableName in LTables do
    begin
      if LDataset.Active then
        LDataset.Close;

      LDataset.CommandText := Format('SELECT * FROM %0:S WHERE 1=2', [LTableName]);
      try
        LDataset.Open;

        if LDataset.Fields.Count > 0 then
        begin
          LEntityModel := CreateEntityDataFromFields(LDataset.Fields, LTableName);
          FEntities.Add(LEntityModel);
        end;
      except
        on E:Exception do
        begin
          //spawn
        end;
      end;
    end;
  finally
    LTables.Free;
    LDataset.Free;
  end;
end;

procedure TEntityModelDataLoader.ParseTableName(const ATableName: string; out ATable,
  ASchema: string);
var
  LIndex: Integer;
begin
  ATable := ATableName;
  ASchema := '';
  LIndex := PosEx('.', ATableName);
  if (LIndex > 1) then
  begin
    ATable := Copy(ATableName, LIndex + 1, Length(ATableName)-1);
    ASchema := Copy(ATableName, 1, LIndex - 1);
  end;
end;

end.
