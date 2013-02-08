unit Adapters.ObjectDataset.Abstract;

interface

uses
  DB
  ,Classes
  ,Spring.Collections
  ,Rtti
  ;

type
  PVariantList = ^TVariantList;
  TVariantList = array [0 .. 0] of OleVariant;
  PArrayRecInfo = ^TArrayRecInfo;

  TArrayRecInfo = record
    Index: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  TIndexFieldInfo = record
    Field: TField;
    RttiProperty: TRttiProperty;
    Descending: Boolean;
    CaseInsensitive: Boolean;
  end;

  TAttributeClass = class of TCustomAttribute;

  TCompareRecords = function(const Item1, Item2: Integer; AIndexFieldList: IList<TIndexFieldInfo>): Integer of object;

  TAbstractObjectDataset = class(TDataset)
  private
    FRowBufSize: Integer;
    FFilterBuffer: TRecordBuffer;
    FOldValueBuffer: TRecordBuffer;
    FReadOnly: Boolean;
    FCurrent: Integer;
    FInternalOpen: Boolean;
    FModifiedFields: IList<TField>;
    FFilteredIndexes: IList<Integer>;
    function GetIndex: Integer;
    procedure SetIndex(const Value: Integer);
  protected
    function GetColumnAttributeClass(): TAttributeClass; virtual;
    function IsFilterEntered(): Boolean;
    // Abstract methods
    procedure DoDeleteRecord(Index: Integer); virtual; abstract;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); virtual; abstract;
    procedure DoPostRecord(Index: Integer); virtual; abstract;
    function RecordFilter: Boolean; virtual; abstract;
    procedure UpdateFilter(); virtual; abstract;

    // Basic overrides
    function GetCanModify: Boolean; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Integer; override;
    procedure SetFiltered(Value: Boolean); override;

    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Integer); override;
    procedure SetCurrent(AValue: Integer); virtual;
    procedure SetRecBufSize(); virtual;
    function DataListCount(): Integer; virtual;

    // Abstract overrides
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;

    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
      override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;

    function InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual;
    procedure VariantToBuffer(Field: TField; Data: Variant; Buffer: Pointer; NativeFormat: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Current: Integer read FCurrent;
    property FilteredIndexes: IList<Integer> read FFilteredIndexes;
    property Index: Integer read GetIndex write SetIndex;
    property ModifiedFields: IList<TField> read FModifiedFields;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  public
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    {$IF CompilerVersion >= 23}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    {$IFEND}

    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
  end;

resourcestring
  SUnsupportedFieldType = 'Unsupported field type (%s) in field %s.';
  SIndexOutOfRange = 'Index out of range';
  SColumnPropertiesNotSpecified = 'Type does not have column properties';

implementation

uses
  Variants
  ,SysUtils
  ,Math
  ,Forms
  ,DBConsts
  ,FMTBcd
  ,ActiveX
  ,Windows
  ,WideStrUtils
  ,Mapping.Attributes
  ,Adapters.ObjectDataset.Blobs
  ,Contnrs
  ;

type
  EAbstractObjectDatasetException = class(Exception);

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TObjectList;
  Fld: TField;
  Bookmark: TBookmark;

  function CompareField(var Field: TField; Value: Variant): Boolean; {BG}
  var
    S: string;
  begin
    if Field.DataType in [ftString{$IFDEF UNICODE}, ftWideString{$ENDIF UNICODE}] then
    begin
      if Value = Null then
        Result := Field.IsNull
      else
      begin
        S := Field.AsString;
        if loPartialKey in Options then
          Delete(S, Length(Value) + 1, MaxInt);
        if loCaseInsensitive in Options then
          Result := AnsiSameText(S, Value)
        else
          Result := AnsiSameStr(S, Value);
      end;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
    begin
      Fld := TField(Fields.First);      {BG}
      Result := CompareField(Fld, KeyValues)  {BG}
    end
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
      begin
        Fld := TField(Fields[I]);                  {BG}
        Result := Result and CompareField(Fld, KeyValues[I]);  {BG}
      end;
    end;
  end;

begin
  Result := False;
  with DataSet do
  begin
    CheckBrowseMode;
    if IsEmpty then
      Exit;
  end;
  Fields := TObjectList.Create(False);
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then
      Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        with DataSet do
        begin
          First;
          while not Eof do
          begin
            Result := CompareRecord;
            if Result then
              Break;
            Next;
          end;
        end;
      finally
        if not Result and DataSet.BookmarkValid(TBookmark(Bookmark)) then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

function FieldListCheckSum(Dataset: TDataSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Dataset.Fields.Count - 1 do
    Result := Result + (Integer(Dataset.Fields[I]) shr (I mod 16));
end;

{ TCustomObjectDataset }

function TAbstractObjectDataset.AllocRecordBuffer: TRecordBuffer;
begin
  if not (csDestroying in ComponentState) then
  begin
    Result := AllocMem(FRowBufSize);
    Initialize(PVariantList(Result + sizeof(TArrayRecInfo))^, Fields.Count);
  end
  else
    Result := nil;
end;

function TAbstractObjectDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if Assigned(Bookmark) and (PInteger(Bookmark)^ >= 0) and
    (PInteger(Bookmark)^ < DataListCount {RecordCount}) then
    Result := True
  else
    Result := False;
end;

function TAbstractObjectDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  LRetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
begin
  Result := LRetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1
    else
      Result := 0;
  end;
end;

constructor TAbstractObjectDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalOpen := False;
  FReadOnly := False;
  FModifiedFields := TCollections.CreateList<TField>();
  FFilteredIndexes := TCollections.CreateList<Integer>;
end;


function TAbstractObjectDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TSvBlobStream.Create(Field as TBlobField, Mode);
end;

{$IF CompilerVersion >=23}
procedure TCustomObjectDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TAbstractObjectDataset.DataEvent(Event: TDataEvent; Info: Integer);
{$IFEND}
begin
  case Event of
    deLayoutChange:
      if Active and Assigned(Reserved) and
        (FieldListCheckSum(Self) <> Integer(Reserved)) then
        Reserved := nil;
  end;
  inherited;
end;

function TAbstractObjectDataset.DataListCount: Integer;
begin
  Result := 0;
end;

destructor TAbstractObjectDataset.Destroy;
begin
  inherited Destroy;
end;

procedure TAbstractObjectDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
      Fields.Count);

  InitRecord(FOldValueBuffer);
  inherited DoOnNewRecord;
end;

procedure TAbstractObjectDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
  FreeMem(Buffer);
end;

function TAbstractObjectDataset.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  RecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := ActiveBuffer;
    {dsEdit :
    begin
      if FEditBuffer = nil then
        RecBuf := ActiveBuffer
      else
        RecBuf := FEditBuffer;
    end;                      }
    dsNewValue, dsInsert, dsEdit:
      RecBuf := ActiveBuffer;

    dsCalcFields, dsInternalCalc:
      RecBuf := CalcBuffer;

    dsFilter:
      RecBuf := FFilterBuffer;
  end;
  Result := RecBuf <> nil;
end;

function TAbstractObjectDataset.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;

procedure TAbstractObjectDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PArrayRecInfo(Buffer)^.Index;
end;

function TAbstractObjectDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PArrayRecInfo(Buffer)^.BookmarkFlag;
end;

function TAbstractObjectDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TAbstractObjectDataset.GetColumnAttributeClass: TAttributeClass;
begin
  Result := ColumnAttribute;
end;

function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
var
  LRecBuf: TRecordBuffer;
  LData: Variant;

  procedure RefreshBuffers;
  begin
    Reserved := Pointer(FieldListCheckSum(Self));
   // UpdateCursorPos;
    Resync([]);
  end;

  function DataToInt64: Int64;
  begin
    {TODO -oLinas -cGeneral : get rid of ActiveX dependency}
    if Decimal(LData).sign > 0 then
      Result := -1 * Decimal(LData).Lo64
    else
      Result := Decimal(LData).Lo64;
  end;

begin
 // if not Assigned(Reserved) then
 //   RefreshBuffers;
{  if (State = dsOldValue) and (FModifiedFields.IndexOf(Field) <> -1) then
  begin
    Result := True;
    LRecBuf := FOldValueBuffer;
  end
  else}
    Result := GetActiveRecBuf(LRecBuf);

  if not Result then
    Exit;

  LData := PVariantList(LRecBuf + sizeof(TArrayRecInfo))^[Field.Index];

  if VarIsEmpty(LData) then
  begin
    DoGetFieldValue(Field, PArrayRecInfo(LRecBuf)^.Index, LData);
    if VarIsEmpty(LData) then
      LData := Null;

    if VarType(LData) = varInt64 then
      PVariantList(LRecBuf + sizeof(TArrayRecInfo))[Field.Index] := DataToInt64
    else
      PVariantList(LRecBuf + sizeof(TArrayRecInfo))[Field.Index] := LData;
  end;

  Result := not VarIsNull(LData);
  if Result and (Buffer <> nil) then
    VariantToBuffer(Field, LData, Buffer, NativeFormat);
end;

function TAbstractObjectDataset.GetIndex: Integer;
var
  LRecBuf: TRecordBuffer;
begin
  Result := -1;
  CheckActive;
  if GetActiveRecBuf(LRecBuf) and (PArrayRecInfo(LRecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(LRecBuf)^.Index;
 // Result := RecNo;
 // if Result > -1 then
//    dec(Result);
end;

function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(Field, Buffer, True);
end;

function TAbstractObjectDataset.GetRecNo: Longint;
var
  LRecBuf: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(LRecBuf) and (PArrayRecInfo(LRecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(LRecBuf)^.Index + 1;

  if IsFilterEntered then
    Result := FFilteredIndexes.IndexOf(Result-1) + 1;     {TODO -oOwner -cCategory : Maybe better use dictionary?}
end;

function TAbstractObjectDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  if Filtered then
    FFilterBuffer := Buffer;

  Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TAbstractObjectDataset.GetRecordCount: Integer;
begin
  Result := -1;
end;

function TAbstractObjectDataset.GetRecordSize: Word;
begin
  Result := sizeof(TArrayRecInfo);
end;

procedure TAbstractObjectDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  inherited;
end;

procedure TAbstractObjectDataset.InternalClose;
begin
  FInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if FOldValueBuffer <> nil then
  begin
    try
      Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
        Fields.Count);
      FreeMem(FOldValueBuffer);
    finally
      FOldValueBuffer := nil;
    end;
  end;
end;

procedure TAbstractObjectDataset.InternalDelete;
var
  LRecBuf: TRecordBuffer;
begin
  GetActiveRecBuf(LRecBuf);
  DoDeleteRecord(PArrayRecInfo(LRecBuf)^.Index);
end;

procedure TAbstractObjectDataset.InternalEdit;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
      Fields.Count);
end;

procedure TAbstractObjectDataset.InternalFirst;
begin
  FCurrent := -1;
//  if IsFilterEntered then
//    FCurrent := FilteredIndexes.FirstOrDefault(-1);
end;

function TAbstractObjectDataset.InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  LRecCount: Integer;
  Accept: Boolean;
begin
  Accept := True;
  try
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          LRecCount := DataListCount {RecordCount};

          if FCurrent + 1 >= LRecCount then
          begin
            Inc(FCurrent);
            Accept := False;
          end
          else
          begin
            repeat
              Inc(FCurrent);
              if IsFilterEntered then
                Accept := RecordFilter;
            until Accept or (FCurrent > LRecCount - 1);

            if not Accept then
            begin
              Result := grEOF;
              FCurrent := FCurrent - 1;
            end;
          end;



          {if FCurrent < LRecCount then
            inc(FCurrent);

          if FCurrent >= LRecCount then
            Result := grEOF
          else
          begin
            if IsFilterEntered then
              Accept := RecordFilter();
          end;  }
        end;
      gmPrior:
        begin
         { if FCurrent <= 0 then
            FCurrent := -1
          else
          begin
            LRecCount := RecordCount;
            FCurrent := Min(FCurrent - 1, LRecCount - 1);

            if IsFilterEntered then
              Accept := RecordFilter();
          end;

          if FCurrent < 0 then
            Result := grBOF;   }
          LRecCount :=  DataListCount {RecordCount};

          if FCurrent <= 0 then
          begin
            Result := grBOF;
            FCurrent := -1;
          end
          else
          begin
            repeat
             // FCurrent := Min(FCurrent - 1, LRecCount - 1);
              Dec(FCurrent);
              if IsFilterEntered then
                Accept := RecordFilter;
           //   FCurrent := Min(FCurrent - 1, LRecCount - 1);
            until Accept or (FCurrent < 0);
            if not Accept then
            begin
              Result := grBOF;
              FCurrent := -1;
            end
            else
            begin
              if FCurrent >= LRecCount then
              begin
                Result := grEOF;
              end;
            end;
          end;
        end;

      gmCurrent:
        begin
          LRecCount := DataListCount {RecordCount};
          if FCurrent < 0 then
            Result := grBOF
          else if FCurrent >= LRecCount then
          begin
            Result := grEOF;
           // FCurrent := LRecCount - 1;
          end
          else if IsFilterEntered then
          begin
            Accept := RecordFilter();
          end;
        end;
    end;

    if not Accept then
    begin
      Result := grEOF;
    end
    else
    begin
      if (State in [dsFilter]) and (GetMode <> gmCurrent) and (FCurrent > -1) and (FCurrent < DataListCount) then
        FilteredIndexes.Add(FCurrent);
    end;

    if Result = grOK then
    begin
      PArrayRecInfo(Buffer)^.Index := FCurrent;
      PArrayRecInfo(Buffer)^.BookmarkFlag := bfCurrent;

      Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
      GetCalcFields(Buffer);
    end;

  except
    if DoCheck then
      raise ;
    Result := grError;
  end;
end;

procedure TAbstractObjectDataset.InternalGotoBookmark(Bookmark: Pointer);
var
  LOld: Integer;
begin
  LOld := FCurrent;
  FCurrent := PInteger(Bookmark)^;
  if Filtered and (FilteredIndexes.Count > 0) then
  begin
    if not RecordFilter then
      FCurrent := LOld;
  end;
end;

procedure TAbstractObjectDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TAbstractObjectDataset.InternalInitFieldDefs;

  procedure InitFieldDefsFromFields(AFields: TFields; AFieldDefs: TFieldDefs);
  var
    I: Integer;
    LField: TField;
    LFieldDef: TFieldDef;
  begin
    for I := 0 to AFields.Count - 1 do
    begin
      LField := AFields[I];
      if AFieldDefs.IndexOf(LField.FieldName) = -1 then
      begin
        LFieldDef := AFieldDefs.AddFieldDef;
        LFieldDef.Name := LField.FieldName;
        LFieldDef.DataType := LField.DataType;
        LFieldDef.Size := LField.Size;
        if LField.Required then
          LFieldDef.Attributes := [faRequired];
        if LField.ReadOnly then
          LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadonly];
        if (LField.DataType = ftBCD) and (LField is TBCDField) then
          LFieldDef.Precision := TBCDField(LField).Precision;
        if LField is TObjectField then
          InitFieldDefsFromFields(TObjectField(LField).Fields, LFieldDef.ChildDefs);
      end;
    end;
  end;
begin
  FieldDefs.Clear;
  InitFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TAbstractObjectDataset.InternalInitRecord(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  for I := 0 to Fields.Count - 1 do
    PVariantList(Buffer + sizeof(TArrayRecInfo))[I] := Null;
end;

procedure TAbstractObjectDataset.InternalLast;
begin
 // if IsFilterEntered then
 //   FCurrent := FilteredIndexes.LastOrDefault(RecordCount)
 // else
    FCurrent := DataListCount {RecordCount};
end;

procedure TAbstractObjectDataset.InternalOpen;
begin
  FInternalOpen := True;
  FCurrent := -1;

  BookmarkSize := sizeof(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;
  Reserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  SetRecBufSize();
end;

procedure TAbstractObjectDataset.InternalPost;
var
  LRecBuf: TRecordBuffer;
begin
  UpdateCursorPos;
  GetActiveRecBuf(LRecBuf);

  if PArrayRecInfo(LRecBuf)^.BookmarkFlag = bfEof then
    DoPostRecord(-1)
  else
    DoPostRecord(PArrayRecInfo(LRecBuf)^.Index);
end;

procedure TAbstractObjectDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    FCurrent := PArrayRecInfo(Buffer)^.Index;
end;

function TAbstractObjectDataset.IsCursorOpen: Boolean;
begin
  Result := FInternalOpen;
end;

function TAbstractObjectDataset.IsFilterEntered: Boolean;
begin
  Result := (Filtered) and (Trim(Filter) <> '');
end;

function TAbstractObjectDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TAbstractObjectDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := inherited Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TAbstractObjectDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer)^.Index := PInteger(Data)^
  else
    PArrayRecInfo(Buffer)^.Index := -1;
end;

procedure TAbstractObjectDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PArrayRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TAbstractObjectDataset.SetCurrent(AValue: Integer);
begin
  FCurrent := AValue;
end;

procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
  SetFieldData(Field, Buffer, True);
end;

procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);

  procedure BufferToVar(var AData: Variant);
  begin
    case Field.DataType of
      ftInterface:
        AData := IUnknown(Buffer^);
      ftIDispatch:
        AData := IDispatch(Buffer^);
      ftVariant:
        AData := Variant(Buffer^);
      ftString, ftFixedChar, ftGuid:
        AData := AnsiString(PAnsiChar(Buffer));
      ftWideString, ftFixedWideChar:
        AData := WideString(PWideChar(Buffer));
      ftAutoInc, ftInteger:
        AData := Longint(Buffer^);
      ftSmallint:
        AData := SmallInt(Buffer^);
      ftWord:
        AData := Word(Buffer^);
      ftBoolean:
        AData := WordBool(Buffer^);
      ftFloat, ftCurrency:
        AData := Double(Buffer^);
      ftBlob, ftMemo, ftGraphic, ftWideMemo:
        AData := Variant(Buffer^);

      ftDate, ftTime, ftDateTime:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(AData).VDate, False)
        else
          AData := TDateTime(Buffer^);

      ftBCD:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(AData).VCurrency, False)
        else
          AData := Currency(Buffer^);

      ftBytes, ftVarBytes:
        if NativeFormat then
          DataConvert(Field, Buffer, @AData, False)
        else
          AData := Variant(Buffer^);

      ftLargeInt:
        begin
          TVarData(AData).VType := VT_DECIMAL;
          Decimal(AData).Lo64 := Int64(Buffer^);
        end;

    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;

var
  LData: Variant;
  LRecBuf: TRecordBuffer;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  GetActiveRecBuf(LRecBuf);

  if Field.FieldNo > 0 then
  begin
    if ReadOnly and not(State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);

    Field.Validate(Buffer);

    if FModifiedFields.IndexOf(Field) = -1 then
    begin
      PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))[Field.Index] :=
        Field.Value;
      FModifiedFields.Add(Field);
    end;
  end;

  if Buffer = nil then
    LData := Null
  else
    BufferToVar(LData);

  PVariantList(LRecBuf + sizeof(TArrayRecInfo))[Field.Index] := LData;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

procedure TAbstractObjectDataset.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    inherited SetFiltered(Value);
    if Filtered then
    begin
      UpdateFilter;
    end;
  end;
end;

procedure TAbstractObjectDataset.SetIndex(const Value: Integer);
begin
  if (Value < 0) or (Value >= DataListCount) then
    raise EAbstractObjectDatasetException.Create(SIndexOutOfRange);

  FCurrent := Value;
 // RecNo := Value + 1;
end;

procedure TAbstractObjectDataset.SetRecBufSize;
begin
  FRowBufSize := SizeOf(TArrayRecInfo) + (Fields.Count * SizeOf(Variant));
end;

procedure TAbstractObjectDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value :=  Min(max(Value, 1), RecordCount);
  if IsFilterEntered then
    Value := FilteredIndexes[Value];

  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TAbstractObjectDataset.VariantToBuffer(Field: TField; Data: Variant; Buffer: Pointer;
  NativeFormat: Boolean);

  procedure CurrToBuffer(const C: Currency);
    begin
      if NativeFormat then
        DataConvert(Field, @C, Buffer, True)
      else
        Currency(Buffer^) := C;
    end;

var
  LLength: Integer;

begin
  case Field.DataType of
    ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        if (VarType(Data) = varString) then
        begin
          LLength := Min(Length(Data), Field.Size);
          PAnsiChar(Buffer)[LLength] := #0;
          Move(PChar(string(Data))^, PChar(Buffer)^, LLength);
        end
        else
        begin
          {$IF CompilerVersion >= 20}
          LLength := Length(Data);
          {$ELSE}
          l := SysStringLen(tagVariant(Data).bStrVal);
          {$IFEND}
          if LLength = 0 then
            PAnsiChar(Buffer)[0] := #0
          else
            WideCharToMultiByte(0, 0, tagVariant(Data).bStrVal, LLength + 1, Buffer,
              Field.Size, nil, nil);
        end;
      end;
    ftFixedWideChar, ftWideString:
      if tagVariant(Data).bStrVal = nil then
        PWideChar(Buffer)[0] := #0
      else
        WStrCopy(Buffer, tagVariant(Data).bStrVal);
    ftSmallint:
      if tagVariant(Data).vt = VT_UI1 then
        SmallInt(Buffer^) := Byte(tagVariant(Data).cVal)
      else
        SmallInt(Buffer^) := tagVariant(Data).iVal;
    ftWord:
      if tagVariant(Data).vt = VT_UI1 then
        Word(Buffer^) := tagVariant(Data).bVal
      else
        Word(Buffer^) := tagVariant(Data).uiVal;
    ftAutoInc, ftInteger:
      Integer(Buffer^) := Data;
    ftFloat, ftCurrency:
      if tagVariant(Data).vt = VT_R8 then
        Double(Buffer^) := tagVariant(Data).dblVal
      else
        Double(Buffer^) := Data;
    ftFMTBCD:
      TBcd(Buffer^) := VarToBcd(Data);
    ftBCD:
      if tagVariant(Data).vt = VT_CY then
        CurrToBuffer(tagVariant(Data).cyVal)
      else
        CurrToBuffer(Data);
    ftBoolean:
      begin
        VarAsType(Data, VT_BOOL);
        WordBool(Buffer^) := tagVariant(Data).vbool;
      end;
    ftDate, ftTime, ftDateTime:
      begin
        VarAsType(Data, VT_DATE);
        if NativeFormat then
          DataConvert(Field, @date, Buffer, True)
        else
          TOleDate(Buffer^) := Data;
      end;
    ftBytes, ftVarBytes:
      if NativeFormat then
        DataConvert(Field, @Data, Buffer, True)
      else
        OleVariant(Buffer^) := Data;
    ftInterface:
      IUnknown(Buffer^) := Data;
    ftIDispatch:
      IDispatch(Buffer^) := Data;
    ftLargeInt:
      if Decimal(Data).sign > 0 then
        LargeInt(Buffer^) := -1 * Decimal(Data).Lo64
      else
        LargeInt(Buffer^) := Decimal(Data).Lo64;
    ftBlob .. ftTypedBinary, ftVariant, ftWideMemo:
      OleVariant(Buffer^) := Data;
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
      Field.DisplayName]);
  end;

end;

end.
