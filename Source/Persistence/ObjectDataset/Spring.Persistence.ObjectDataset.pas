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

unit Spring.Persistence.ObjectDataset;

interface

uses
  Classes,
  DB,
  TypInfo,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Persistence.ObjectDataset.Abstract,
  Spring.Persistence.ObjectDataset.Algorithms.Sort,
  Spring.Persistence.ObjectDataset.ExprParser;

type
  TObjectDataset = class(TAbstractObjectDataset)
  private
    FDataList: IObjectList;
    FDefaultStringFieldLength: Integer;
    FFilterIndex: Integer;
    FFilterParser: TExprParser;
    FItemTypeInfo: PTypeInfo;
    FIndexFieldList: IList<TIndexFieldInfo>;
    FProperties: IList<TRttiProperty>;
    FSort: string;
    FSorted: Boolean;
    FCtx: TRttiContext;

    FOnAfterFilter: TNotifyEvent;
    FOnAfterSort: TNotifyEvent;
    FOnBeforeFilter: TNotifyEvent;
    FOnBeforeSort: TNotifyEvent;
    FColumnAttributeTypeInfo: PTypeInfo;

    function GetSort: string;
    procedure SetSort(const Value: string);
    function GetFilterCount: Integer;
    // we cannot use IDictionary, because it changes order of items.
    // we use list with predicate searching - slower, but working
    function PropertyFinder(const s: string): Spring.TPredicate<TRttiProperty>;
  protected
    procedure DoAfterOpen; override;
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
    procedure DoPostRecord(Index: Integer; Append: Boolean); override;
    procedure RebuildPropertiesCache; override;

    function DataListCount: Integer; override;
    function GetCurrentDataList: IObjectList; override;
    function GetRecordCount: Integer; override;
    function RecordConformsFilter: Boolean; override;
    procedure UpdateFilter; override;

    procedure DoOnAfterFilter; virtual;
    procedure DoOnBeforeFilter; virtual;
    procedure DoOnBeforeSort; virtual;
    procedure DoOnAfterSort; virtual;

    function CompareRecords(const Item1, Item2: TValue; AIndexFieldList: IList<TIndexFieldInfo>): Integer; virtual;
    function ConvertPropertyValueToVariant(const AValue: TValue): Variant; virtual;
    function InternalGetFieldValue(AField: TField; const AItem: TValue): Variant; virtual;
    function ParserGetVariableValue(Sender: TObject; const VarName: string; var Value: Variant): Boolean; virtual;
    function ParserGetFunctionValue(Sender: TObject; const FuncName: string;
      const Args: Variant; var ResVal: Variant): Boolean; virtual;
    procedure DoFilterRecord(AIndex: Integer); virtual;
    procedure InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo); virtual;
    procedure InternalSetSort(const AValue: string; AIndex: Integer = 0); virtual;
    procedure LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs); virtual;
    procedure LoadFieldDefsFromItemType; virtual;
    procedure RefreshFilter; virtual;

    function  IsCursorOpen: Boolean; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure SetFilterText(const Value: string); override;

    function GetChangedSortText(const ASortText: string): string;
    function CreateIndexList(const ASortText: string): IList<TIndexFieldInfo>;
    function FieldInSortIndex(AField: TField): Boolean;
    function ValueToVariant(const value: TValue): Variant;
    function StreamToVariant(AStream: TStream): OleVariant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Makes the current dataset clone of <c>ASource</c>.
    ///	</summary>
    {$ENDREGION}
    procedure Clone(ASource: TObjectDataset);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns underlying model object from the current row.
    ///	</summary>
    {$ENDREGION}
    function GetCurrentModel<T: class>: T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns newly created list of data containing only filtered items.
    ///	</summary>
    {$ENDREGION}
    function GetFilteredDataList<T: class>: IList<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Sets the list which will represent the data for the dataset. Dataset
    ///	  will create it's fields based on model's public or published 
    ///	  properties which are marked with <c>[Column]</c> attribute.
    ///	</summary>
    {$ENDREGION}
    procedure SetDataList<T: class>(ADataList: IList<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the total count of filtered records.
    ///	</summary>
    {$ENDREGION}
    property FilterCount: Integer read GetFilterCount;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Checks if dataset is sorted.
    ///	</summary>
    {$ENDREGION}
    property Sorted: Boolean read FSorted;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Sorting conditions separated by commas. Can set  different sort order
    ///	  for multiple fields - <c>Asc</c> stands for ascending, <c>Desc</c> -
    ///	  descending.
    ///	</summary>
    ///	<example>
    ///	  <code>
    ///	MyDataset.Sort := 'Name, Id Desc, Description Asc';</code>
    ///	</example>
    {$ENDREGION}
    property Sort: string read GetSort write SetSort;

    property DataList: IObjectList read FDataList;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Type info of the column attribute. If entity's properties are annotated with this attribute,
    ///   then dataset will use these properties as fields.
    ///   If ColumnAttributeClassInfo is null, all the entity's published properties will be used as dataset fields.
    ///	</summary>
    ///	<example>
    ///	  <code>
    ///	MyDataset.ColumnAttributeClassInfo := ColumnAttribute.ClassInfo</code>
    ///	</example>
    {$ENDREGION}
    property ColumnAttributeClassInfo: PTypeInfo read FColumnAttributeTypeInfo write FColumnAttributeTypeInfo;
  published
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Default length for the string type field in the dataset.
    ///	</summary>
    ///	<remarks>
    ///	  Defaults to <c>250</c> if not set.
    ///	</remarks>
    {$ENDREGION}
    property DefaultStringFieldLength: Integer read FDefaultStringFieldLength write FDefaultStringFieldLength default 250;
    property Filter;
    property Filtered;
    property FilterOptions;


    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnAfterFilter: TNotifyEvent read FOnAfterFilter write FOnAfterFilter;
    property OnBeforeFilter: TNotifyEvent read FOnBeforeFilter write FOnBeforeFilter;
    property OnAfterSort: TNotifyEvent read FOnAfterSort write FOnAfterSort;
    property OnBeforeSort: TNotifyEvent read FOnBeforeSort write FOnBeforeSort;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Variants,
  Types,
  Math,
  Spring.Reflection,
  Spring.Reflection.ValueConverters,
  Spring.SystemUtils,
  Spring.Persistence.ObjectDataset.ExprParser.Functions;

type
  EObjectDatasetException = class(Exception);
  
  {$WARNINGS OFF}
  TWideCharSet = set of Char;
  {$WARNINGS ON}


{ TObjectDataset }

procedure TObjectDataset.Clone(ASource: TObjectDataset);
begin
  if Active then
    Close;

  FItemTypeInfo := ASource.FItemTypeInfo;
  FDataList := ASource.DataList;
  IndexList.DataList := ASource.IndexList.DataList;

  FilterOptions := ASource.FilterOptions;
  Filter := ASource.Filter;
  Filtered := ASource.Filtered;
  Open;
  if ASource.Sorted then
    Sort := ASource.Sort;
end;

function TObjectDataset.CompareRecords(const Item1, Item2: TValue;
  AIndexFieldList: IList<TIndexFieldInfo>): Integer;
var
  i: Integer;
  LFieldInfo: TIndexFieldInfo;
  LValue1, LValue2: TValue;
begin
  Result := 0;

  for i := 0 to AIndexFieldList.Count - 1 do
  begin
    LFieldInfo := AIndexFieldList[i];
    LValue1 := LFieldInfo.RttiProperty.GetValue(Item1);
    LValue2 := LFieldInfo.RttiProperty.GetValue(Item2);

    Result := Spring.Persistence.ObjectDataset.Abstract.CompareValue(LValue1, LValue2);
    if LFieldInfo.Descending then
      Result := -Result;

    if Result <> 0 then
      Exit;
  end;
end;

function TObjectDataset.ConvertPropertyValueToVariant(const AValue: TValue): Variant;
begin
  {TODO -oOwner -cGeneral : could use some routine from Spring in future}
  Result := ValueToVariant(AValue);
end;

constructor TObjectDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TCollections.CreateList<TRttiProperty>;
  FFilterParser := TExprParser.Create;
  FFilterParser.OnGetVariable := ParserGetVariableValue;
  FFilterParser.OnExecuteFunction := ParserGetFunctionValue;
  FDefaultStringFieldLength := 250;
  FCtx := TRttiContext.Create;
end;

function TObjectDataset.CreateIndexList(const ASortText: string): IList<TIndexFieldInfo>;
var
  LText, LItem: string;
  LSplittedFields: TStringDynArray;
  LIndexFieldItem: TIndexFieldInfo;
  iPos: Integer;
begin
  Result := TCollections.CreateList<TIndexFieldInfo>;
  LSplittedFields := SplitString(ASortText, [','], True);
  for LText in LSplittedFields do
  begin
    LItem := UpperCase(LText);
    LIndexFieldItem.Descending := PosEx('DESC', LItem) > 1;
    LItem := Trim(LText);
    iPos := PosEx(' ', LItem);
    if iPos > 1 then
      LItem := Copy(LItem, 1, iPos - 1);

    LIndexFieldItem.Field := FindField(LItem);
    if not FProperties.TryGetFirst(LIndexFieldItem.RttiProperty,
        PropertyFinder(LIndexFieldItem.Field.FieldName)) then
      raise EObjectDatasetException.CreateFmt('Field %d used for sorting not found in the dataset.', [LIndexFieldItem.Field.Name]);
    LIndexFieldItem.CaseInsensitive := True;
    Result.Add(LIndexFieldItem);
  end;
end;

function TObjectDataset.DataListCount: Integer;
begin
  Result := 0;
  if Assigned(FDataList) then
    Result := FDataList.Count;
end;

destructor TObjectDataset.Destroy;
begin
  FFilterParser.Free;
  inherited Destroy;
end;

procedure TObjectDataset.DoAfterOpen;
begin
  if Filtered then
  begin
    UpdateFilter;
    First;
  end;
  inherited DoAfterOpen;
end;

procedure TObjectDataset.DoDeleteRecord(Index: Integer);
begin
  IndexList.DeleteModel(Index);
end;

procedure TObjectDataset.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  LItem: TValue;
begin
  LItem := IndexList.GetModel(Index);
  Value := InternalGetFieldValue(Field, LItem);
end;

procedure TObjectDataset.DoOnAfterFilter;
begin
  if Assigned(FOnAfterFilter) then
    FOnAfterFilter(Self);
end;

procedure TObjectDataset.DoOnAfterSort;
begin
  if Assigned(FOnAfterSort) then
    FOnAfterSort(Self);
end;

procedure TObjectDataset.DoOnBeforeFilter;
begin
  if Assigned(FOnBeforeFilter) then
    FOnBeforeFilter(Self);
end;

procedure TObjectDataset.DoOnBeforeSort;
begin
  if Assigned(FOnBeforeSort) then
    FOnBeforeSort(Self);
end;

procedure TObjectDataset.DoPostRecord(Index: Integer; Append: Boolean);
var
  LItem: TValue;
  LConvertedValue: TValue;
  LValueFromVariant: TValue;
  LFieldValue: Variant;
  i: Integer;
  LProp: TRttiProperty;
  LField: TField;
  LNeedsSort: Boolean;
begin
  if State = dsInsert then
    LItem := TActivator.CreateInstance(FItemTypeInfo)
  else
    LItem := IndexList.GetModel(Index);

  LNeedsSort := False;

  for i := 0 to ModifiedFields.Count - 1 do
  begin
    LField := ModifiedFields[i];

    if not LNeedsSort and Sorted then
    begin
      LNeedsSort := FieldInSortIndex(LField);
    end;

    // Fields not found in dictionary are calculated or lookup fields, do not post them
    if FProperties.TryGetFirst(LProp, PropertyFinder(LField.FieldName)) then begin
      LFieldValue := LField.Value;
      if VarIsNull(LFieldValue) then
      begin
        LProp.SetValue(LItem, TValue.Empty);
      end
      else
      begin
        LValueFromVariant := TValue.FromVariant(LFieldValue);

     //   if TUtils.TryConvert(LValueFromVariant, nil, LProp, LItem.AsObject, LConvertedValue) then
        if TValueConverter.Default.TryConvertTo(LValueFromVariant, LProp.PropertyType.Handle, LConvertedValue) then
          LProp.SetValue(LItem, LConvertedValue);
      end;
    end;
  end;

  if State = dsInsert then
  begin
    if Append then
      Index := IndexList.AddModel(LItem)
    else
      IndexList.InsertModel(LItem, Index);
  end;

  DoFilterRecord(Index);
  if Sorted and LNeedsSort then
    InternalSetSort(Sort, Index);

  SetCurrent(Index);
end;

function TObjectDataset.FieldInSortIndex(AField: TField): Boolean;
var
  i: Integer;
begin
  if Sorted and Assigned(FIndexFieldList) then
  begin
    for i := 0 to FIndexFieldList.Count - 1 do
    begin
      if (AField = FIndexFieldList[i].Field) then
      begin
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TObjectDataset.GetChangedSortText(const ASortText: string): string;
begin
  Result := ASortText;

  if EndsStr(' ', Result) then
    Result := Copy(Result, 1, Length(Result) - 1)
  else
    Result := Result + ' ';
end;

function TObjectDataset.GetCurrentDataList: IObjectList;
begin
  Result := DataList;
end;

function TObjectDataset.GetCurrentModel<T>: T;
begin
  Result := System.Default(T);
  if Active and (Index > -1) and (Index < RecordCount) then
  begin
    Result := IndexList.GetModel(Index).AsType<T>;
  end;
end;

function TObjectDataset.GetFilterCount: Integer;
begin
  Result := 0;
  if Filtered then
    Result := IndexList.Count;
end;

function TObjectDataset.GetFilteredDataList<T>: IList<T>;
var
  i: Integer;
begin
  Result := TCollections.CreateList<T>;
  for i := 0 to IndexList.Count - 1 do
  begin
    Result.Add(IndexList.GetModel(i).AsType<T>);
  end;
end;

function TObjectDataset.GetRecordCount: Integer;
begin
  Result := IndexList.Count;
end;

function TObjectDataset.GetSort: string;
begin
  Result := FSort;
end;

procedure TObjectDataset.InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo);
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  FProperties.Clear;

  LType := FCtx.GetType(AItemTypeInfo);
  for LProp in LType.GetProperties do
  begin
    if not (LProp.Visibility in [mvPublic, mvPublished]) then
      Continue;

    if Assigned(FColumnAttributeTypeInfo) then
    begin
      for LAttrib in LProp.GetAttributes do
      begin
        if (LAttrib.ClassInfo = FColumnAttributeTypeInfo) then
        begin
          FProperties.Add(LProp);
          Break;
        end;
      end;
    end
    else
    begin
      if (LProp.Visibility = mvPublished) then
      begin
        FProperties.Add(LProp);
      end;
    end;
  end;
end;

function TObjectDataset.InternalGetFieldValue(AField: TField; const AItem: TValue): Variant;
var
  LProperty: TRttiProperty;
begin
  if not FProperties.Any then
    InitRttiPropertiesFromItemType(AItem.TypeInfo);

  // Fields not found in dictionary are calculated or lookup fields, do not post them
  if FProperties.TryGetFirst(LProperty, PropertyFinder(AField.FieldName)) then
    Result := ConvertPropertyValueToVariant(LProperty.GetValue(AItem));
end;

procedure TObjectDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  if Fields.Count > 0 then
  begin
    LoadFieldDefsFromFields(Fields, FieldDefs);
  end
  else
  begin
    LoadFieldDefsFromItemType;
  end;
end;

procedure TObjectDataset.InternalOpen;
begin
  inherited InternalOpen;


  if {$IF CompilerVersion >=27}((FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles))
    {$ELSE}DefaultFields{$IFEND} then
    CreateFields;

  Reserved := Pointer(FieldListCheckSum);
  BindFields(True);
  SetRecBufSize;
end;

procedure TObjectDataset.InternalSetSort(const AValue: string; AIndex: Integer);
var
  Pos: Integer;
  LDataList: IObjectList;
  LOwnedDatalist: ICollectionOwnership;
  LOwnsObjectsProp: Boolean;
  LChanged: Boolean;
begin
  if IsEmpty then
    Exit;

  DoOnBeforeSort;

  LChanged := AValue <> FSort;
  FIndexFieldList := CreateIndexList(AValue);

  Pos := Current;
  LDataList := FDataList;
  LOwnsObjectsProp := Supports(LDataList, ICollectionOwnership, LOwnedDatalist);
  try
    if LOwnsObjectsProp then
    begin
      LOwnedDatalist.OwnsObjects := False;
    end;
    if LChanged then
      TMergeSort.Sort(IndexList, CompareRecords, FIndexFieldList)
    else
      TInsertionSort.Sort(AIndex, IndexList, CompareRecords, FIndexFieldList);

    FSorted := FIndexFieldList.Count > 0;
    FSort := AValue;

  finally
    if LOwnsObjectsProp then
      LOwnedDatalist.OwnsObjects := True;

    SetCurrent(Pos);
  end;
  DoOnAfterSort;
end;

function TObjectDataset.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDataList);
end;

procedure TObjectDataset.LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
var
  i: integer;
  LField: TField;
  LFieldDef: TFieldDef;
begin
  for I := 0 to Fields.Count - 1 do
  begin
    LField := Fields[I];
    if FieldDefs.IndexOf(LField.FieldName) = -1 then
    begin
      LFieldDef := FieldDefs.AddFieldDef;
      LFieldDef.Name := LField.FieldName;
      LFieldDef.DataType := LField.DataType;
      LFieldDef.Size := LField.Size;
      if LField.Required then
        LFieldDef.Attributes := [DB.faRequired];
      if LField.ReadOnly then
        LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadonly];
      if (LField.DataType = ftBCD) and (LField is TBCDField) then
        LFieldDef.Precision := TBCDField(LField).Precision;
      if LField is TObjectField then
        LoadFieldDefsFromFields(TObjectField(LField).Fields, LFieldDef.ChildDefs);
    end;
  end;
end;

procedure TObjectDataset.LoadFieldDefsFromItemType;
var
  LProp: TRttiProperty;
 // LAttrib: TCustomAttribute;
  LPropPrettyName: string;
  LFieldType: TFieldType;
  LFieldDef: TObjectDatasetFieldDef;
  LLength, LPrecision, LScale: Integer;
  LRequired, LDontUpdate, LHidden: Boolean;

  procedure DoGetFieldType(ATypeInfo: PTypeInfo);
  var
    LTypeInfo: PTypeInfo;
  begin
    case ATypeInfo.Kind of
      tkInteger:
      begin
        LLength := -2;
        if (ATypeInfo = TypeInfo(Word)) then
        begin
          LFieldType := ftWord;
        end
        else if (ATypeInfo = TypeInfo(SmallInt)) then
        begin
          LFieldType := ftSmallint;
        end
        else
        begin
          LFieldType := ftInteger;
        end;
      end;
      tkEnumeration:
      begin
        if (ATypeInfo = TypeInfo(Boolean)) then
        begin
          LFieldType := ftBoolean;
          LLength := -2;
        end
        else
        begin
          LFieldType := ftWideString;
          if LLength = -2 then
            LLength := FDefaultStringFieldLength;
        end;
      end;
      tkFloat:
      begin
        if (ATypeInfo = TypeInfo(TDate)) then
        begin
          LFieldType := ftDate;
          LLength := -2;
        end
        else if (ATypeInfo = TypeInfo(TDateTime)) then
        begin
          LFieldType := ftDateTime;
          LLength := -2;
        end
        else if (ATypeInfo = TypeInfo(Currency)) then
        begin
          LFieldType := ftCurrency;
          LLength := -2;
        end
        else if (ATypeInfo = TypeInfo(TTime)) then
        begin
          LFieldType := ftTime;
          LLength := -2;
        end
        else if (LPrecision <> -2) or (LScale <> -2) then
        begin
          LFieldType := ftBCD;
          LLength := -2;
        end
        else
        begin
          LFieldType := ftFloat;
          LLength := -2;
        end;
      end;
      tkString, tkLString, tkChar:
      begin
        LFieldType := ftString;
        if LLength = -2 then
          LLength := FDefaultStringFieldLength;
      end;
      tkVariant, tkArray, tkDynArray:
      begin
        LFieldType := ftVariant;
        LLength := -2;
      end;
      tkClass:
      begin
        if TypeInfo(TStringStream) = ATypeInfo then
          LFieldType := ftMemo
        else
          LFieldType := ftBlob;
        LLength := -2;
        LDontUpdate := True;
      end;
      tkRecord:
      begin
        if TType.TryGetNullableTypeInfo(ATypeInfo, LTypeInfo) then
          DoGetFieldType(LTypeInfo);
      end;
      tkInt64:
      begin
        LFieldType := ftLargeint;
        LLength := -2;
      end;
      tkUString, tkWString, tkWChar, tkSet:
      begin
        LFieldType := ftWideString;
        if LLength = -2 then
          LLength := FDefaultStringFieldLength;
      end;
    end;
  end;

begin
  InitRttiPropertiesFromItemType(FItemTypeInfo);

  if not FProperties.Any then
    raise EObjectDatasetException.Create(SColumnPropertiesNotSpecified);
  for LProp in FProperties do
  begin
    LPropPrettyName := LProp.Name;
    LLength := -2;
    LPrecision := -2;
    LScale := -2;
    LRequired := False;
    LDontUpdate := False;
    LHidden := False;
    {TODO -oOwner -cGeneral : what to do with values from ColumnAttribute?}
    {for LAttrib in LProp.GetAttributes do
    begin
      if LAttrib is ColumnAttribute then
      begin
        if (ColumnAttribute(LAttrib).ColumnName <> '') then
          LPropPrettyName := ColumnAttribute(LAttrib).ColumnName;

        if (ColumnAttribute(LAttrib).Length <> 0) then
          LLength := ColumnAttribute(LAttrib).Length;

        if (ColumnAttribute(LAttrib).Precision <> 0) then
          LPrecision := ColumnAttribute(LAttrib).Precision;

        if (ColumnAttribute(LAttrib).Scale <> 0) then
          LScale := ColumnAttribute(LAttrib).Scale;

        if (ColumnAttribute(LAttrib).Description <> '') then
          LPropPrettyName := ColumnAttribute(LAttrib).Description;

        LRequired := ( cpRequired in ColumnAttribute(LAttrib).Properties );
        LDontUpdate := ( cpDontInsert in ColumnAttribute(LAttrib).Properties )
          or ( cpDontUpdate in ColumnAttribute(LAttrib).Properties );

        LHidden :=  ( cpHidden in ColumnAttribute(LAttrib).Properties );

        Break;
      end;
    end;}

    LFieldType := ftWideString;

    DoGetFieldType(LProp.PropertyType.Handle);

    LFieldDef := FieldDefs.AddFieldDef as TObjectDatasetFieldDef;
    LFieldDef.Name := LProp.Name;
    LFieldDef.SetRealDisplayName(LPropPrettyName);

    LFieldDef.DataType := LFieldType;
    if LLength <> -2 then
      LFieldDef.Size := LLength;

    LFieldDef.Required := LRequired;

    if LFieldType in [ftFMTBcd, ftBCD] then
    begin
      if LPrecision <> -2 then
        LFieldDef.Precision := LPrecision;

      if LScale <> -2 then
        LFieldDef.Size := LScale;
    end;

    if LDontUpdate then
      LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadOnly];

    LFieldDef.Visible := not LHidden;

    if not LProp.IsWritable then
      LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadOnly];
  end;
end;

function TObjectDataset.StreamToVariant(AStream: TStream): OleVariant;
var
  DataPtr: Pointer;
begin
  Result := VarArrayCreate([0, AStream.Size], varByte);
  DataPtr := VarArrayLock(Result);
  try
    AStream.ReadBuffer(DataPtr^, AStream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

function TObjectDataset.ParserGetFunctionValue(Sender: TObject; const FuncName: string;
  const Args: Variant; var ResVal: Variant): Boolean;
var
  LGetValueFunc: TFunctionGetValueProc;
begin
  Result := TFilterFunctions.TryGetFunction(FuncName, LGetValueFunc);
  if Result then
  begin
    ResVal := LGetValueFunc(Args);
  end;
end;

function TObjectDataset.ParserGetVariableValue(Sender: TObject; const VarName: string;
  var Value: Variant): Boolean;
var
  LField: TField;
begin
  Result := FilterCache.TryGetValue(VarName, Value);
  if not Result then
  begin
    LField := FindField(Varname);
    if Assigned(LField) then
    begin
      Value := InternalGetFieldValue(LField, FDataList[FFilterIndex]);
      FilterCache.Add(VarName, Value);
      Result := True;
    end;
  end;
end;

function TObjectDataset.PropertyFinder(
  const s: string): Spring.TPredicate<TRttiProperty>;
begin
  Result :=
    function (const prop: TRttiProperty): Boolean
      begin
        Result := prop.Name = s;
      end
end;

procedure TObjectDataset.RebuildPropertiesCache;
var
  LType: TRttiType;
  i: Integer;
begin
  FProperties.Clear;
  LType := FCtx.GetType(FItemTypeInfo);
  for i := 0 to Fields.Count - 1 do
  begin
    FProperties.Add(LType.GetProperty(Fields[i].FieldName) );
  end;
end;

function TObjectDataset.RecordConformsFilter: Boolean;
begin
  Result := True;
  if (FFilterIndex >= 0) and (FFilterIndex <  DataListCount ) then
  begin
    if Assigned(OnFilterRecord) then
      OnFilterRecord(Self, Result)
    else
    begin
      if (FFilterParser.Eval) then
      begin
        Result := FFilterParser.Value;
      end;
    end;
  end
  else
    Result := False;
end;

procedure TObjectDataset.RefreshFilter;
var
  i: Integer;
begin
  IndexList.Clear;
  if not IsFilterEntered then
  begin
    IndexList.Rebuild;
    Exit;
  end;

  for i := 0 to FDataList.Count - 1 do
  begin
    FFilterIndex := i;
    FilterCache.Clear;
    if RecordConformsFilter then
    begin
      IndexList.Add(i, FDataList[i]);
    end;
  end;
  FilterCache.Clear;
end;

procedure TObjectDataset.DoFilterRecord(AIndex: Integer);
begin
  if (IsFilterEntered) and (AIndex > -1) and (AIndex < RecordCount) then
  begin
    FilterCache.Clear;
    FFilterIndex := IndexList[AIndex].DataListIndex;
    if not RecordConformsFilter then
    begin
      IndexList.Delete(AIndex);
    end;
  end;
end;

procedure TObjectDataset.SetDataList<T>(ADataList: IList<T>);
begin
  FItemTypeInfo := TypeInfo(T);
  FDataList := ADataList as IObjectList;
  IndexList.DataList := FDataList;
end;


procedure TObjectDataset.SetFilterText(const Value: string);
begin
  if (Value = Filter) then
    Exit;

  if Active then
  begin
    CheckBrowseMode;
    inherited SetFilterText(Value);

    if Filtered then
    begin
      UpdateFilter;
      First;
    end
    else
    begin
      UpdateFilter;
      Resync([]);
      First;
    end;
  end
  else
  begin
    inherited SetFilterText(Value);
  end;
end;

procedure TObjectDataset.SetSort(const Value: string);
begin
  CheckActive;
  if State in dsEditModes then
    Post;

  UpdateCursorPos;
  InternalSetSort(Value);
  Resync([]);
end;

procedure TObjectDataset.UpdateFilter;
var
  LSaveState: TDataSetState;
begin
  if not Active then
    Exit;

  DoOnBeforeFilter;

  if IsFilterEntered then
  begin
    FFilterParser.EnableWildcardMatching := not (foNoPartialCompare in FilterOptions);
    FFilterParser.CaseInsensitive := foCaseInsensitive in FilterOptions;

    if foCaseInsensitive in FilterOptions then
      FFilterParser.Expression := AnsiUpperCase(Filter)
    else
      FFilterParser.Expression := Filter;
  end;

  LSaveState := SetTempState(dsFilter);
  try
    RefreshFilter;
  finally
    RestoreState(LSaveState);
  end;

  DisableControls;
  try
    First;
    if Sorted then
      InternalSetSort( GetChangedSortText(Sort) );   //must use mergesort so we change sort text
  finally
    EnableControls;
  end;
  UpdateCursorPos;
  Resync([]);
  First;

  DoOnAfterFilter;
end;

function TObjectDataset.ValueToVariant(const value: TValue): Variant;
var
  LStream: TStream;
  LValue: TValue;
  LPersist: IStreamPersist;
begin
  Result := Null;
  case value.Kind of
    tkEnumeration:
    begin
      if value.TypeInfo = TypeInfo(Boolean) then
        Result := value.AsBoolean
      else
        Result := value.AsOrdinal;
    end;
    tkFloat:
    begin
      if (value.TypeInfo = TypeInfo(TDateTime)) then
        Result := value.AsType<TDateTime>
      else if (value.TypeInfo = TypeInfo(TDate)) then
        Result := value.AsType<TDate>
      else
        Result := value.AsExtended;
    end;
    tkRecord:
    begin
      if TType.IsNullableType(value.TypeInfo) then
        if TType.TryGetNullableValue(value, LValue) then
          Result := ValueToVariant(LValue);
    end;
    tkClass:
    begin
      if (value.AsObject <> nil) then
      begin
        if (value.AsObject is TStream) then
        begin
          LStream := TStream(value.AsObject);
          LStream.Position := 0;
          Result := StreamToVariant(LStream);
        end
        else if Supports(value.AsObject, IStreamPersist, LPersist) then
        begin
          LStream := TMemoryStream.Create;
          try
            LPersist.SaveToStream(LStream);
            LStream.Position := 0;
            Result := StreamToVariant(LStream);
          finally
            LStream.Free;
          end;
        end;
      end;
    end;
    tkInterface: Result := value.AsInterface;
    else
    begin
      Result := value.AsVariant;
    end;
  end;
end;

end.
