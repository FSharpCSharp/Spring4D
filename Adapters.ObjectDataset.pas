unit Adapters.ObjectDataset;

interface

uses
  Adapters.ObjectDataset.Abstract
  ,Adapters.ObjectDataset.ExprParser
  ,Classes
  ,Spring.Collections
  ,Rtti
  ,DB
  ,TypInfo
  ;

type
  TObjectDataset = class(TAbstractObjectDataset)
  private
    FDataList: IList;
    FProperties: IList<TRttiProperty>;
    FDefaultStringFieldLength: Integer;
    FItemTypeInfo: PTypeInfo;
    FFilterParser: TExprParser;
    FSorted: Boolean;
    FSort: string;
    FFilterIndex: Integer;
    FCacheArray: TArray<TValue>;

    FOnAfterFilter: TNotifyEvent;
    FOnBeforeFilter: TNotifyEvent;

    function GetSort: string;
    procedure SetSort(const Value: string);
    function GetFilterCount: Integer;
  protected
    procedure DoAfterOpen; override;
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
    procedure DoPostRecord(Index: Integer); override;
    procedure RebuildPropertiesCache(); override;

    procedure DoOnAfterFilter(); virtual;
    procedure DoOnBeforeFilter(); virtual;


    function ConvertPropertyValueToVariant(const AValue: TValue): Variant; virtual;
    procedure InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo); virtual;
    procedure UpdateFilter(); override;
    function ParserGetVariableValue(Sender: TObject; const VarName: string; var Value: Variant): Boolean; virtual;
    function RecordFilter: Boolean; override;

    function GetCurrentDataList(): IList; override;
    function GetRecordCount: Integer; override;
    function DataListCount(): Integer; override;

    function InternalGetFieldValue(AField: TField; const AItem: TValue): Variant; virtual;
    procedure LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs); virtual;
    procedure LoadFieldDefsFromItemType; virtual;
    procedure RefreshFilter(); virtual;
    procedure DoFilterRecord(ADataListIndex: Integer); virtual;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function  IsCursorOpen: Boolean; override;
    procedure SetFilterText(const Value: string); override;

    function CompareRecords(const Item1, Item2: TValue; AIndexFieldList: IList<TIndexFieldInfo>): Integer; virtual;
    procedure InternalSetSort(AIndexFieldList: IList<TIndexFieldInfo>); virtual;
    function CreateIndexList(const ASortText: string): IList<TIndexFieldInfo>;
    procedure QuickSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>); virtual;
    procedure MergeSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns newly created list of data containing only filtered items.
    ///	</summary>
    {$ENDREGION}
    function GetFilteredDataList<T: class>(): IList<T>;

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

    property DataList: IList read FDataList;
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
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

uses
  Core.Utils
  ,Mapping.RttiExplorer
  ,Mapping.Attributes
  ,SysUtils
  ,Spring.Reflection.ValueConverters
  ,StrUtils
  ,Variants
  ,Core.Reflection
  ;

type
  EObjectDatasetException = class(Exception);
  
  {$WARNINGS OFF}
  TWideCharSet = set of Char;
  {$WARNINGS ON}

{ TObjectDataset }




function SplitString(const AText: string; const ADelimiters: TWideCharSet;
  const ARemoveEmptyEntries: Boolean): TArray<string>;
var
  LResCount, I, LLag,
    LPrevIndex, LCurrPiece: NativeInt;
  LPiece: string;
begin
  { Initialize, set all to zero }
  SetLength(Result , 0);

  { Do nothing for empty strings }
  if System.Length(AText) = 0 then
    Exit;

  { Determine the length of the resulting array }
  LResCount := 0;

  for I := 1 to System.Length(AText) do
    if CharInSet(AText[I], ADelimiters) then
      Inc(LResCount);

  { Set the length of the output split array }
  SetLength(Result, LResCount + 1);

  { Split the string and fill the resulting array }
  LPrevIndex := 1;
  LCurrPiece := 0;
  LLag := 0;

  for I := 1 to System.Length(AText) do
    if CharInSet(AText[I], ADelimiters) then
    begin
      LPiece := System.Copy(AText, LPrevIndex, (I - LPrevIndex));

      if ARemoveEmptyEntries and (System.Length(LPiece) = 0) then
        Inc(LLag)
      else
        Result[LCurrPiece - LLag] := LPiece;

      { Adjust prev index and current piece }
      LPrevIndex := I + 1;
      Inc(LCurrPiece);
    end;

  { Copy the remaining piece of the string }
  LPiece := Copy(AText, LPrevIndex, System.Length(AText) - LPrevIndex + 1);

  { Doom! }
  if ARemoveEmptyEntries and (System.Length(LPiece) = 0) then
    Inc(LLag)
  else
    Result[LCurrPiece - LLag] := LPiece;

  { Re-adjust the array for the missing pieces }
  if LLag > 0 then
    System.SetLength(Result, LResCount - LLag + 1);
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
    LValue1 := LFieldInfo.RttiProperty.GetValue(TRttiExplorer.GetRawPointer(Item1));
    LValue2 := LFieldInfo.RttiProperty.GetValue(TRttiExplorer.GetRawPointer(Item2));

    Result := CompareValue(LValue1, LValue2);
    if LFieldInfo.Descending then
      Result := -Result;

    if Result <> 0 then
      Exit;
  end;
end;

function TObjectDataset.ConvertPropertyValueToVariant(const AValue: TValue): Variant;
begin
  Result := TUtils.AsVariant(AValue);
end;

constructor TObjectDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TCollections.CreateList<TRttiProperty>;
  FFilterParser := TExprParser.Create;
  FFilterParser.OnGetVariable := ParserGetVariableValue;
  FDefaultStringFieldLength := 250;
end;

function TObjectDataset.CreateIndexList(const ASortText: string): IList<TIndexFieldInfo>;
var
  LText, LItem: string;
  LSplittedFields: TArray<string>;
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
    LIndexFieldItem.RttiProperty := FProperties[LIndexFieldItem.Field.Index];
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
var
  LDataListIndex: Integer;
begin
  if not Filtered then
    FDataList.Delete(Index)
  else
  begin
    LDataListIndex := FilteredIndexes[Index];
    FilterList.Delete(Index);
    FilteredIndexes.Delete(Index);
    FDataList.Delete(LDataListIndex);
  end;
end;

procedure TObjectDataset.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  LItem: TValue;
  LDataList: IList;
begin
  LDataList := GetCurrentDataList();
  if Assigned(LDataList) then
  begin
    LItem := LDataList[Index];
    Value := InternalGetFieldValue(Field, LItem);
  end;
end;

procedure TObjectDataset.DoOnAfterFilter;
begin
  if Assigned(FOnAfterFilter) then
    FOnAfterFilter(Self);
end;

procedure TObjectDataset.DoOnBeforeFilter;
begin
  if Assigned(FOnBeforeFilter) then
    FOnBeforeFilter(Self);
end;

procedure TObjectDataset.DoPostRecord(Index: Integer);
var
  LItem: TValue;
  LConvertedValue: TValue;
  LValueFromVariant: TValue;
  LFieldValue: Variant;
  i: Integer;
  LProp: TRttiProperty;
  LField: TField;
  LDataList: IList;
begin
  LDataList := GetCurrentDataList();
  if State = dsInsert then
    LItem := TRttiExplorer.CreateType(FItemTypeInfo)
  else
    LItem := LDataList[Index];

  for i := 0 to ModifiedFields.Count - 1 do
  begin
    LField := ModifiedFields[i];
    LProp := FProperties[LField.FieldNo - 1];
    LFieldValue := LField.Value;
    LValueFromVariant := TUtils.FromVariant(LFieldValue);
    if TValueConverter.Default.TryConvertTo(LValueFromVariant, LProp.PropertyType.Handle, LConvertedValue, TValue.Empty) then
      LProp.SetValue(TRttiExplorer.GetRawPointer(LItem), LConvertedValue);
  end;

  if State = dsInsert then
  begin
    FDataList.Add(LItem);
    Index := FDataList.Count - 1;
  end;

  DoFilterRecord(Index);
end;

function TObjectDataset.GetCurrentDataList: IList;
begin
  if Filtered then
    Result := FilterList
  else
    Result := DataList;
end;

function TObjectDataset.GetFilterCount: Integer;
begin
  Result := 0;
  if Filtered then
    Result := FilterList.Count;
end;

function TObjectDataset.GetFilteredDataList<T>: IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(False);
  Result.AsList.AddRange(FilterList);
end;

function TObjectDataset.GetRecordCount: Integer;
var
  LDataList: IList;
begin
  Result := 0;
  LDataList := GetCurrentDataList();
  if Assigned(LDataList) then
  begin
    Result := LDataList.Count;
  end;
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

  LType := TRttiContext.Create.GetType(AItemTypeInfo);
  for LProp in LType.GetProperties do
  begin
    if not (LProp.Visibility in [mvPublic, mvPublished]) then
      Continue;

    for LAttrib in LProp.GetAttributes do
    begin
      if (LAttrib is GetColumnAttributeClass) or (LAttrib is ColumnAttribute) then
      begin
        FProperties.Add(LProp);
        Break;
      end;
    end;
  end;
end;

function TObjectDataset.InternalGetFieldValue(AField: TField; const AItem: TValue): Variant;
var
  LProperty: TRttiProperty;
begin
  if FProperties.IsEmpty then
    InitRttiPropertiesFromItemType(AItem.TypeInfo);

  LProperty := FProperties[AField.FieldNo - 1];
  Result := ConvertPropertyValueToVariant(LProperty.GetValue(TRttiExplorer.GetRawPointer(AItem)));
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

  if DefaultFields then
    CreateFields;

  Reserved := Pointer(FieldListCheckSum);
  BindFields(True);
  SetRecBufSize();
end;

procedure TObjectDataset.InternalSetSort(AIndexFieldList: IList<TIndexFieldInfo>);
var
  Pos: DB.TBookmark;
begin
  if IsEmpty then
    Exit;
  Pos := Bookmark;
  try
    SetLength(FCacheArray, RecordCount);
   // MergeSort(0, RecordCount - 1, CompareRecords, AIndexFieldList);
    QuickSort(0, RecordCount - 1, CompareRecords, AIndexFieldList);
   // SetBufListSize(0);
    //try
      //SetBufListSize(BufferCount + 1);
   // except
    //  SetState(dsInactive);
   //   CloseCursor;
    //  raise;
    //end;
  finally
    SetLength(FCacheArray, 0);
    Bookmark := Pos;
  end;
end;

function TObjectDataset.IsCursorOpen: Boolean;
var
  LDataList: IList;
begin
  LDataList := GetCurrentDataList();
  Result := Assigned(LDataList);
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
    with LField do
      if FieldDefs.IndexOf(FieldName) = -1 then
      begin
        LFieldDef := FieldDefs.AddFieldDef;
        LFieldDef.Name := FieldName;
        LFieldDef.DataType := DataType;
        LFieldDef.Size := Size;
        if Required then
          LFieldDef.Attributes := [DB.faRequired];
        if ReadOnly then
          LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadonly];
        if (DataType = ftBCD) and (LField is TBCDField) then
          LFieldDef.Precision := TBCDField(LField).Precision;
        if LField is TObjectField then
          LoadFieldDefsFromFields(TObjectField(LField).Fields, LFieldDef.ChildDefs);
      end;
  end;
end;

procedure TObjectDataset.LoadFieldDefsFromItemType;
var
  i: Integer;
  LProp: TRttiProperty;
  LAttrib: TCustomAttribute;
  LPropPrettyName: string;
  LFieldType: TFieldType;
  LFieldDef: TFieldDef;
  LLength: Integer;
begin
  InitRttiPropertiesFromItemType(FItemTypeInfo);

  if FProperties.IsEmpty then
    raise EObjectDatasetException.Create(SColumnPropertiesNotSpecified);

  for i := 0 to FProperties.Count - 1 do
  begin
    LProp := FProperties[i];
    LPropPrettyName := LProp.Name;
    LLength := -2;
    for LAttrib in LProp.GetAttributes do
    begin
      if LAttrib is ColumnAttribute then
      begin
        LPropPrettyName := ColumnAttribute(LAttrib).Name;
        LLength := ColumnAttribute(LAttrib).Length;
       // LPrecision := ColumnAttribute(LAttrib).Precision;
       // LScale := ColumnAttribute(LAttrib).Scale;
        if (ColumnAttribute(LAttrib).Description <> '') then
          LPropPrettyName := ColumnAttribute(LAttrib).Description;
        Break;
      end;
    end;

    LFieldType := ftWideString;

    case LProp.PropertyType.TypeKind of
      tkInteger:
      begin
        LFieldType := ftInteger;
        LLength := -2;
      end;
      tkEnumeration:
      begin
        if (LProp.PropertyType.Handle = TypeInfo(Boolean)) then
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
        if (LProp.PropertyType.Handle = TypeInfo(TDate)) then
        begin
          LFieldType := ftDate;
          LLength := -2;
        end
        else if (LProp.PropertyType.Handle = TypeInfo(TDateTime)) then
        begin
          LFieldType := ftDateTime;
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
      tkVariant, tkClass, tkArray, tkDynArray:
      begin
        LFieldType := ftVariant;
        LLength := -2;
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

    LFieldDef := FieldDefs.AddFieldDef;
    LFieldDef.Name := LProp.Name;
    LFieldDef.DisplayName := LPropPrettyName;
    LFieldDef.DataType := LFieldType;
    if LLength <> -2 then
      LFieldDef.Size := LLength;

    if not LProp.IsWritable then
      LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadOnly];
  end;
end;


procedure TObjectDataset.MergeSort(ALow, AHigh: Integer; Compare: TCompareRecords;
  AIndexFieldList: IList<TIndexFieldInfo>);
var
  LDataList: IList;
  LCache: TArray<TValue>;

  procedure Merge(Low, Mid, High: Integer);
  var
    i, j, k: Integer;
  begin
    for i := Low to High do
    begin
      LCache[i] := LDataList[i];
    end;

    i := Low;
    j := Mid + 1;
    k := Low;

    while (i <= Mid) and (j <= High) do
    begin
      if (Compare(LCache[i], LCache[j], AIndexFieldList) <= 0) then
      begin
        LDataList[k] := LCache[i];
        Inc(i);
      end
      else
      begin
        LDataList[k] := LCache[j];
        Inc(j);
      end;
      Inc(k);
    end;

    while (i <= Mid) do
    begin
      LDataList[k] := LCache[i];
      Inc(k);
      Inc(i);
    end;

  end;

  procedure PerformMergeSort(ALowIndex, AHighIndex: Integer; CompareMethod: TCompareRecords;
    AIndexFields: IList<TIndexFieldInfo>);
  var
    iMid: Integer;
  begin
    if ALowIndex < AHighIndex then
    begin
      iMid:= (AHighIndex + ALowIndex) div 2;
      PerformMergeSort( ALowIndex, iMid, CompareMethod, AIndexFields );
      PerformMergeSort( iMid+1, AHighIndex, CompareMethod, AIndexFields );
      Merge(ALowIndex, iMid, AHighIndex);
    end;
  end;

begin
  LDataList := GetCurrentDataList;
  SetLength(LCache, LDataList.Count);
  PerformMergeSort(ALow, AHigh, Compare, AIndexFieldList);
end;

function TObjectDataset.ParserGetVariableValue(Sender: TObject; const VarName: string;
  var Value: Variant): Boolean;
var
  LField: TField;
begin
  Result := False;
  LField := FindField(Varname);
  if Assigned(LField) then
  begin
    Value := InternalGetFieldValue(LField, FDataList[FFilterIndex]);
    Result := True;
  end;
end;

procedure TObjectDataset.QuickSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>);
var
  LLow, LHigh: Integer;
  iPivot: Integer;
  LPivot: TValue;
  LDataList: IList;
begin
  if (RecordCount = 0) or ( (AHigh - ALow) <= 0 ) then
    Exit;

  LDataList := GetCurrentDataList();

  repeat
    LLow := ALow;
    LHigh := AHigh;
    iPivot := (ALow + (AHigh - ALow) shr 1);
    LPivot := LDataList[iPivot];

    repeat
      while Compare(LDataList[LLow], LPivot, AIndexFieldList) < 0 do
        Inc(LLow);

      while Compare(LDataList[LHigh], LPivot, AIndexFieldList) > 0 do
        Dec(LHigh);

      if LLow <= LHigh then
      begin
        if LLow <> LHigh then
        begin
          LDataList.Exchange(LLow, LHigh);
          if Filtered then
            FilteredIndexes.Exchange(LLow, LHigh);
        end;

        Inc(LLow);
        Dec(LHigh);
      end;
    until LLow > LHigh;

    if ALow < LHigh then
      QuickSort(ALow, LHigh, Compare, AIndexFieldList);
    ALow := LLow;
  until LLow >= AHigh;
end;


procedure TObjectDataset.RebuildPropertiesCache;
var
  LType: TRttiType;
  i: Integer;
begin
  FProperties.Clear;
  LType := TRttiContext.Create.GetType(FItemTypeInfo);
  for i := 0 to Fields.Count - 1 do
  begin
    FProperties.Add( LType.GetProperty(Fields[i].FieldName) );
  end;
end;

function TObjectDataset.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
  LValue: Variant;
begin
  Result := True;
  if (FFilterIndex >= 0) and (FFilterIndex <  DataListCount ) then
  begin
    SaveState := SetTempState(dsFilter);
    try
      if Assigned(OnFilterRecord) then
        OnFilterRecord(Self, Result)
      else
      begin
        if (IsFilterEntered) and (FFilterParser.Eval()) then
        begin
          FFilterParser.EnableWildcardMatching := not (foNoPartialCompare in FilterOptions);
          FFilterParser.CaseInsensitive := foCaseInsensitive in FilterOptions;
          LValue := FFilterParser.Value;
          Result := LValue;
        end;
      end;
    except
      InternalHandleException;
    end;
    RestoreState(SaveState);
  end
  else
    Result := False;
end;

procedure TObjectDataset.RefreshFilter;
var
  i: Integer;
begin
  FilterList.Clear;
  FilteredIndexes.Clear;
  for i := 0 to FDataList.Count - 1 do
  begin
    FFilterIndex := i;
    if RecordFilter then
    begin
      FilterList.Add(FDataList[i]);
      FilteredIndexes.Add(i);
    end;
  end;
end;

procedure TObjectDataset.DoFilterRecord(ADataListIndex: Integer);
begin
  if (Filtered) and (ADataListIndex > -1) and (ADataListIndex < DataListCount) then
  begin
    FFilterIndex := ADataListIndex;
    if RecordFilter then
    begin
      if not FilterList.Contains(FDataList[ADataListIndex]) then
      begin
        FilterList.Add(FDataList[ADataListIndex]);
        FilteredIndexes.Add(ADataListIndex);
      end;
    end;
  end;
end;

procedure TObjectDataset.SetDataList<T>(ADataList: IList<T>);
begin
  FItemTypeInfo := TypeInfo(T);
  FDataList := ADataList.AsList;
  FilterList := TCollections.CreateObjectList<T>(False).AsList;
  FilteredIndexes.Clear;
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
      UpdateFilter();
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
var
  LIndexFieldList: IList<TIndexFieldInfo>;
begin
  CheckActive;
  if State in dsEditModes then
    Post;

  FSort := Value;
  UpdateCursorPos;
  LIndexFieldList := CreateIndexList(Value);
  InternalSetSort(LIndexFieldList);
  FSorted := LIndexFieldList.Count > 0;
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
    if foCaseInsensitive in FilterOptions then
      FFilterParser.Expression := AnsiUpperCase(Filter)
    else
      FFilterParser.Expression := Filter;
  end;

  LSaveState := SetTempState(dsFilter);
  try
    RefreshFilter();
  finally
    RestoreState(LSaveState);
  end;

  DisableControls;
  try
    First;
    if Sorted then
      SetSort(Sort);
  finally
    EnableControls;
  end;
  UpdateCursorPos;
  Resync([]);
  First;

  DoOnAfterFilter;
end;

end.
