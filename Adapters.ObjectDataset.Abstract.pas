unit Adapters.ObjectDataset.Abstract;

interface

uses
  DB
  ,Classes
  ,Spring.Collections
  ,Rtti
  ,Core.Algorythms.Sort
  ,Generics.Collections
  ,Adapters.ObjectDataset.IndexList
  ;

type
  PVariantList = ^TVariantList;
  TVariantList = array [0 .. 0] of OleVariant;
  PArrayRecInfo = ^TArrayRecInfo;

  TArrayRecInfo = record
    Index: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  TAttributeClass = class of TCustomAttribute;

  TObjectDatasetFieldDef = class(TFieldDef)
  private
    FDisplayName: string;
    FVisible: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    procedure SetRealDisplayName(const Value: string);
    property Visible: Boolean read FVisible write FVisible;
  end;

  TObjectDatasetFieldDefs = class(TFieldDefs)
  protected
    function GetFieldDefClass: TFieldDefClass; override;
  end;

  TAbstractObjectDataset = class(TDataset)
  private
    FRowBufSize: Integer;
    FFilterBuffer: TRecordBuffer;
    FOldValueBuffer: TRecordBuffer;
    FReadOnly: Boolean;
    FCurrent: Integer;
    FInternalOpen: Boolean;
    FModifiedFields: IList<TField>;
    FFieldsCache: IDictionary<string,TField>;
    FFilterCache: IDictionary<string, Variant>;
    FIndexList: TODIndexList;
    FInsertIndex: Integer;
    {$IF CompilerVersion >=24}
    FReserved: Pointer;
    {$IFEND}
    function GetIndex: Integer;
    procedure SetIndex(const Value: Integer);
  protected
    function GetColumnAttributeClass(): TAttributeClass; virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Are filter set and filter text entered?
    ///	</summary>
    {$ENDREGION}
    function IsFilterEntered(): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Are FilterIndexed fields available?
    ///	</summary>
    {$ENDREGION}
    function IsFiltered(): Boolean;
    // Abstract methods
    procedure DoDeleteRecord(Index: Integer); virtual; abstract;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); virtual; abstract;
    procedure DoPostRecord(Index: Integer; Append: Boolean); virtual; abstract;
    function RecordConformsFilter: Boolean; virtual; abstract;
    procedure UpdateFilter(); virtual; abstract;
    procedure RebuildPropertiesCache(); virtual; abstract;

    // Basic overrides
    function GetCanModify: Boolean; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Integer; override;
    function GetFieldDefsClass: TFieldDefsClass; override;
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; override;
    procedure SetFiltered(Value: Boolean); override;

    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Integer); override;
    procedure SetCurrent(AValue: Integer); virtual;
    procedure SetRecBufSize(); virtual;
    procedure RebuildFieldCache();
    function DataListCount(): Integer; virtual;
    function GetCurrentDataList(): IObjectList; virtual; abstract;

    // Abstract overrides
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure BindFields(Binding: Boolean); override;

    {$IF CompilerVersion >=24}
    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); override;
    {$ELSE}
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    {$IFEND}
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalInsert; override;
    procedure DoBeforeInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
      override;
    {$IF CompilerVersion >=24}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; override;
    {$ELSE}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override;
    {$IFEND}

    function InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual;
    {$IF CompilerVersion >=24}
    procedure VariantToBuffer(Field: TField; Data: Variant; Buffer: TValueBuffer; NativeFormat: Boolean); virtual;
    {$ELSE}
    procedure VariantToBuffer(Field: TField; Data: Variant; Buffer: Pointer; NativeFormat: Boolean); virtual;
    {$IFEND}
    function FieldListCheckSum(): NativeInt; virtual;
  protected
    property FilterCache: IDictionary<string, Variant> read FFilterCache write FFilterCache;
    {$IF CompilerVersion >=24}
    property Reserved: Pointer read FReserved write FReserved;
    {$IFEND}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents current cursor index in the dataset.
    ///	</summary>
    {$ENDREGION}
    property Current: Integer read FCurrent;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents the current index of the dataset.
    ///	</summary>
    {$ENDREGION}
    property Index: Integer read GetIndex write SetIndex;

    property IndexList: TODIndexList read FIndexList;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents modified fields of the current record.
    ///	</summary>
    {$ENDREGION}
    property ModifiedFields: IList<TField> read FModifiedFields;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  public
    function FindField(const FieldName: string): TField; reintroduce;

    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    {$IF CompilerVersion >= 23}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: LongInt); override;
    {$IFEND}

    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean; virtual;
    {$IF CompilerVersion >= 25}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; override;
    {$ELSEIF CompilerVersion >= 24}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; override;
    {$ELSE}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    {$IFEND}
    {$IF CompilerVersion >= 24}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); override;
    {$ELSE}
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
    {$IFEND}
  published
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

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
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
  ,DBConsts
  ,FMTBcd
  ,WideStrUtils
  ,Mapping.Attributes
  ,Adapters.ObjectDataset.Blobs
  ,Adapters.ObjectDataset.ActiveX
  ,Contnrs
  ,Generics.Defaults
  ,Core.Reflection
  {$IFDEF MSWINDOWS}
  ,Windows
  ,ActiveX
  {$ENDIF}
  ;

type
  EAbstractObjectDatasetException = class(Exception);

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TObjectList{$IF CompilerVersion >= 24}<TField>{$IFEND};
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
      Fld := TField(Fields.First);
      Result := CompareField(Fld, KeyValues)
    end
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
      begin
        Fld := TField(Fields[I]);
        Result := Result and CompareField(Fld, KeyValues[I]);
      end;
    end;
  end;

begin
  Result := False;
  DataSet.CheckBrowseMode;
  if DataSet.IsEmpty then
    Exit;

  Fields := TObjectList{$IF CompilerVersion >= 24}<TField>{$IFEND}.Create(False);
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
        DataSet.First;
        while not DataSet.Eof do
        begin
          Result := CompareRecord;
          if Result then
            Break;
          DataSet.Next;
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

procedure TAbstractObjectDataset.BindFields(Binding: Boolean);
begin
  inherited BindFields(Binding);
  RebuildFieldCache();
end;

function TAbstractObjectDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  LValue: TValue;
begin
  LValue := PObject(Bookmark)^;
  Result := Assigned(Bookmark) and (not LValue.IsEmpty);
  if Result then
  begin
    Result := IndexList.ContainsModel(LValue);
  end;
end;

function TAbstractObjectDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  LRetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
var
  LValue1, LValue2: TValue;
begin
  Result := LRetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    LValue1 := PObject(Bookmark1)^;
    LValue2 := PObject(Bookmark2)^;
    Result := CompareValue(LValue1, LValue2);
   // Result := PInteger(Bookmark1)^ - PInteger(Bookmark2)^;
  end;
end;

constructor TAbstractObjectDataset.Create(AOwner: TComponent);
var
  LCaseInsensitiveComparer: IEqualityComparer<string>;
begin
  inherited Create(AOwner);
  FInternalOpen := False;
  FReadOnly := False;
  FModifiedFields := TCollections.CreateList<TField>();
  FIndexList := TODIndexList.Create();

  LCaseInsensitiveComparer := TEqualityComparer<string>.Construct(
    function(const Left, Right: string): Boolean
    begin
      Result := SameText(Left, Right);
    end,
    function(const Value: string): Integer
    var s: string;
    begin
      s := UpperCase(Value);
      Result := BobJenkinsHash(s[1], Length(s) * SizeOf(s[1]), 0);
    end
  );
  FFieldsCache := TCollections.CreateDictionary<string,TField>(500, LCaseInsensitiveComparer);
  FFilterCache := TCollections.CreateDictionary<string,Variant>(500,LCaseInsensitiveComparer);
end;


function TAbstractObjectDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TSvBlobStream.Create(Field as TBlobField, Mode);
end;

{$IF CompilerVersion >=23}
procedure TAbstractObjectDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TAbstractObjectDataset.DataEvent(Event: TDataEvent; Info: LongInt);
{$IFEND}
begin
  case Event of
    deLayoutChange:
      if Active then
      begin
        if Assigned(Reserved) and (FieldListCheckSum <> NativeInt(Reserved)) then
          Reserved := nil;
      end;
  end;
  inherited;
end;

function TAbstractObjectDataset.DataListCount: Integer;
begin
  Result := 0;
end;

destructor TAbstractObjectDataset.Destroy;
begin
  FIndexList.Free;
  inherited Destroy;
end;

procedure TAbstractObjectDataset.DoBeforeInsert;
begin
  FInsertIndex := Max(RecNo - 1, 0);
  inherited;
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

function TAbstractObjectDataset.FieldListCheckSum: NativeInt;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Fields.Count - 1 do
  begin
    Result := Result + (NativeInt(Fields[I]) shr (I mod 16));
  end;
end;

function TAbstractObjectDataset.FindField(const FieldName: string): TField;
begin
  if not FFieldsCache.TryGetValue(FieldName, Result) then
    Result := nil;
end;

procedure TAbstractObjectDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
  FreeMem(Buffer);
end;

function TAbstractObjectDataset.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
// 2014-06-01, Pue:
// this Pointer hardcast is needed for XE4 and above and should be no harm below...
// XE4 and above change the Buffers from TRecordBuffer (PByte) to TRecBuf (NativeInt)

  RecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := Pointer(ActiveBuffer);

    dsNewValue, dsInsert, dsEdit:
      RecBuf := Pointer(ActiveBuffer);

    dsCalcFields, dsInternalCalc:
      RecBuf := Pointer(CalcBuffer);

    dsFilter:
      RecBuf := Pointer(FFilterBuffer);
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
  PObject(Data)^ := IndexList.GetModel(PArrayRecInfo(Buffer)^.Index).AsObject;
 // PInteger(Data)^ := IndexList[PArrayRecInfo(Buffer)^.Index];
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

function TAbstractObjectDataset.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldDef);
end;

{$IF CompilerVersion >= 25}
function TAbstractObjectDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer;
  NativeFormat: Boolean): Boolean;
{$ELSEIF CompilerVersion >= 24}
function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: TValueBuffer;
  NativeFormat: Boolean): Boolean;
{$ELSE}
function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
{$IFEND}
var
  LRecBuf: TRecordBuffer;
  LData: Variant;

  procedure RefreshBuffers;
  begin
    Reserved := Pointer(FieldListCheckSum);
    UpdateCursorPos;
    Resync([]);
  end;

  function DataToInt64: Int64;
  begin
    {DONE -oLinas -cGeneral : get rid of ActiveX dependency}
    if PDecimal(@LData)^{Decimal(LData)}.sign > 0 then
      Result := -1 * PDecimal(@LData)^{Decimal(LData)}.Lo64
    else
      Result := PDecimal(@LData)^{Decimal(LData)}.Lo64;
  end;

begin
  if not Assigned(Reserved) then
    RefreshBuffers;

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

{$IF CompilerVersion >= 25}
function TAbstractObjectDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
{$ELSEIF CompilerVersion >= 24}
function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
{$ELSE}
function TAbstractObjectDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
{$IFEND}
begin
  Result := GetFieldData(Field, Buffer, True);
end;

function TAbstractObjectDataset.GetFieldDefsClass: TFieldDefsClass;
begin
  Result := TObjectDatasetFieldDefs;
end;

function TAbstractObjectDataset.GetIndex: Integer;
var
  LRecBuf: TRecordBuffer;
begin
  Result := -1;
  CheckActive;
  if GetActiveRecBuf(LRecBuf) and (PArrayRecInfo(LRecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(LRecBuf)^.Index;
end;

function TAbstractObjectDataset.GetRecNo: Longint;
var
  LRecBuf: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(LRecBuf) and (PArrayRecInfo(LRecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(LRecBuf)^.Index + 1;
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


{$IF CompilerVersion >=24}
procedure TAbstractObjectDataset.InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean);
{$ELSE}
procedure TAbstractObjectDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
{$IFEND}
begin
  DoPostRecord(Current, Append);
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
  if GetActiveRecBuf(LRecBuf) then
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
end;

function TAbstractObjectDataset.InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  LRecCount: Integer;
begin
  try
    LRecCount := IndexList.Count;
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          if FCurrent < LRecCount then
            Inc(FCurrent);
          if FCurrent >= LRecCount then
          begin
            Result := grEOF;
          end;
        end;
      gmPrior:
        begin
          if FCurrent <=0 then
            FCurrent := -1
          else
          begin
            FCurrent := Min(FCurrent - 1, LRecCount - 1);
          end;

          if FCurrent < 0 then
            Result := grBOF;
        end;

      gmCurrent:
        begin
          if FCurrent < 0 then
            Result := grBOF
          else if FCurrent >= LRecCount then
          begin
            Result := grEOF;
            FCurrent := LRecCount;
          end;
        end;
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
  LValue: TValue;
begin
  LValue := PObject(Bookmark)^; // PInteger(Bookmark)^;
  FCurrent := IndexList.IndexOfModel(LValue);
end;

procedure TAbstractObjectDataset.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
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

procedure TAbstractObjectDataset.InternalInsert;
begin
  inherited;
end;

procedure TAbstractObjectDataset.InternalLast;
begin
  FCurrent := RecordCount;
end;

procedure TAbstractObjectDataset.InternalOpen;
begin
  FInternalOpen := True;
  FCurrent := -1;

  BookmarkSize := sizeof(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;
 { Reserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  SetRecBufSize(); }
end;

procedure TAbstractObjectDataset.InternalPost;
var
  LRecBuf: TRecordBuffer;
begin
  inherited InternalPost;
  UpdateCursorPos;
  GetActiveRecBuf(LRecBuf);

  case PArrayRecInfo(LRecBuf)^.BookmarkFlag of
    bfEOF: DoPostRecord(-1, True);
    bfInserted: DoPostRecord(FInsertIndex, False)
    else
      DoPostRecord(PArrayRecInfo(LRecBuf)^.Index, False)
  end;
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

function TAbstractObjectDataset.IsFiltered: Boolean;
begin
  Result := Filtered;
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

procedure TAbstractObjectDataset.RebuildFieldCache;
var
  i: Integer;
begin
  FFieldsCache.Clear;
  for i := 0 to Fields.Count - 1 do
  begin
    Fields[i].DisplayLabel := FieldDefs[i].DisplayName;
    Fields[i].Visible := (FieldDefs[i] as TObjectDatasetFieldDef).Visible;
    FFieldsCache.Add(Fields[i].FieldName, Fields[i]);
  end;
end;

{$IF CompilerVersion >=24}
procedure TAbstractObjectDataset.SetBookmarkData(Buffer: TRecordBuffer;
  Data: TBookmark);
{$ELSE}
procedure TAbstractObjectDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
{$IFEND}
{var
  LValue: TValue;}
begin
  inherited;
  //we dont need this method because we get FInsertIndex OnBeforeInsert event
  {if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
  begin
    LValue := PObject(Data)^;
    PArrayRecInfo(Buffer)^.Index := IndexList.IndexOfModel(LValue);
  end
  else
    PArrayRecInfo(Buffer)^.Index := -1;}
end;

procedure TAbstractObjectDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PArrayRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TAbstractObjectDataset.SetCurrent(AValue: Integer);
begin
  FCurrent := AValue;
end;

{$IF CompilerVersion >= 24}
procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: TValueBuffer);
{$ELSE}
procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: Pointer);
{$IFEND}
begin
  SetFieldData(Field, Buffer, True);
end;

{$IF CompilerVersion >= 24}
procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean);
{$ELSE}
procedure TAbstractObjectDataset.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
{$IFEND}

  {$IF CompilerVersion >= 24}
  procedure BufferToVar(var AData: Variant);
  var
    LUnknown: IUnknown;
    LDispatch: IDispatch;
    TempBuff: TValueBuffer;
  begin
    case Field.DataType of
      ftString, ftFixedChar, ftGuid:
        AData := AnsiString(PAnsiChar(Buffer));
      ftWideString, ftFixedWideChar:
        AData := WideString(PWideChar(Buffer));
      ftAutoInc, ftInteger:
        AData := TBitConverter.ToLongInt(Buffer);
      ftSmallInt:
        AData := TBitConverter.ToSmallInt(Buffer);
      ftWord:
        AData := TBitConverter.ToWord(Buffer);
      ftBoolean:
        AData := TBitConverter.ToWordBool(Buffer);
      ftFloat, ftCurrency:
        AData := TBitConverter.ToDouble(Buffer);
      ftBlob, ftMemo, ftGraphic, ftVariant, ftWideMemo:
        AData := TBitConverter.ToVariant(Buffer);
      ftInterface:
      begin
        Move(Buffer[0], LUnknown, SizeOf(IUnknown));
        AData := LUnknown;
      end;
      ftIDispatch:
      begin
        Move(Buffer[0], LDispatch, SizeOf(IDispatch));
        AData := LDispatch;
      end;
      ftDate, ftTime, ftDateTime:
        if NativeFormat then
        begin
          SetLength(TempBuff, SizeOf(TVarData(AData).VDate));
          DataConvert(Field, Buffer, TempBuff, False);
          TVarData(AData).VDate := TBitConverter.ToDouble(TempBuff);
        end
        else
          AData := TBitConverter.ToDouble(Buffer);
      ftBCD:
        if NativeFormat then
        begin
          SetLength(TempBuff, SizeOf(TVarData(AData).VCurrency));
          DataConvert(Field, Buffer, TempBuff, False);
          TVarData(AData).VCurrency := TBitConverter.ToCurrency(TempBuff);
        end
        else
          AData := TBitConverter.ToCurrency(Buffer);
      ftBytes, ftVarBytes:
        if NativeFormat then
        begin
          TempBuff := BytesOf(@AData, SizeOf(Variant));
          DataConvert(Field, Buffer, TempBuff, False);
          AData := TBitConverter.ToVariant(TempBuff);
        end
        else
          Move(Buffer[0], AData, SizeOf(OleVariant));
      ftLargeInt:
        AData := TBitConverter.ToLargeInt(Buffer);
      else
        DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
          Field.DisplayName]);
    end;

  end;
  {$ELSE}
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
  {$IFEND}

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
    if Active then
    begin
      CheckBrowseMode;
      inherited SetFiltered(Value);
      if Value then
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
      inherited SetFiltered(Value);
    end;
  end;
end;

procedure TAbstractObjectDataset.SetIndex(const Value: Integer);
begin
  if (Value < 0) or (Value >= RecordCount) then
    raise EAbstractObjectDatasetException.Create(SIndexOutOfRange);

  FCurrent := Value;
end;

procedure TAbstractObjectDataset.SetRecBufSize;
begin
  FRowBufSize := SizeOf(TArrayRecInfo) + (Fields.Count * SizeOf(Variant));
end;

procedure TAbstractObjectDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value :=  Min(max(Value, 1), RecordCount);

  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

{$IF CompilerVersion >=24}
procedure TAbstractObjectDataset.VariantToBuffer(Field: TField; Data: Variant; Buffer: TValueBuffer;
  NativeFormat: Boolean);
var
  TempBuff: TValueBuffer;
  PData: Pointer;

  procedure CurrToBuffer(const C: Currency);
  var
    LBuff: TValueBuffer;
  begin
    if NativeFormat then
    begin
      SetLength(LBuff, SizeOf(Currency));
      TBitConverter.FromCurrency(C, LBuff);
      DataConvert(Field, LBuff, Buffer, True)
    end
    else
      TBitConverter.FromCurrency(C, Buffer);
  end;


begin
  case Field.DataType of
    ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        {$IFDEF MSWINDOWS}
        WideCharToMultiByte(0, 0, tagVariant(Data).bStrVal, SysStringLen(tagVariant(Data).bStrVal)+1,
          @Buffer[0], Field.Size, nil, nil);
        {$ELSE}
        TempBuff := TEncoding.Default.GetBytes(String(tagVariant(Data).bStrVal)));
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
        {$ENDIF}

      end;
    ftFixedWideChar, ftWideString:
      begin
        TempBuff := TEncoding.Unicode.GetBytes(tagVariant(Data).bstrVal);
        SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
        TempBuff[Length(TempBuff) - 2] := 0;
        TempBuff[Length(TempBuff) - 1] := 0;
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
      end;
    ftSmallint:
      if tagVariant(Data).vt = VT_UI1 then
        TBitConverter.FromSmallInt(Byte(tagVariant(Data).cVal), Buffer)
      else
        TBitConverter.FromSmallInt(tagVariant(Data).iVal, Buffer);
    ftWord:
      if tagVariant(Data).vt = VT_UI1 then
        TBitConverter.FromWord(tagVariant(Data).bVal, Buffer)
      else
        TBitConverter.FromWord(tagVariant(Data).uiVal, Buffer);
    ftAutoInc, ftInteger:
      TBitConverter.FromInteger(Data, Buffer);
    ftFloat, ftCurrency:
      if tagVariant(Data).vt = VT_R8 then
        TBitConverter.FromDouble(tagVariant(Data).dblVal, Buffer)
      else
        TBitConverter.FromDouble(Data, Buffer);
    ftFMTBCD:
      TBitConverter.FromBcd(VarToBcd(Data), Buffer);
    ftBCD:
      if tagVariant(Data).vt = VT_CY then
        CurrToBuffer(tagVariant(Data).cyVal)
      else
        CurrToBuffer(Data);
    ftBoolean:
      TBitConverter.FromWordBool(tagVariant(Data).vbool, Buffer);
    ftDate, ftTime, ftDateTime:
      if NativeFormat then
      begin
        SetLength(TempBuff, SizeOf(Double));
        TBitConverter.FromDouble(data, TempBuff);
        DataConvert(Field, TempBuff, Buffer, True);
      end
      else
        TBitConverter.FromDouble(tagVariant(Data).date, Buffer);
    ftBytes, ftVarBytes:
      if NativeFormat then
      begin
        PData := VarArrayLock(Data);
        try
          DataConvert(Field, BytesOf(PData, VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1), Buffer, True);
        finally
          VarArrayUnlock(Data);
        end;
      end
      else
        TBitConverter.FromVariant(Data, Buffer);
    ftInterface:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IUnknown));
        Move(TempBuff[0], Buffer[0], SizeOf(IUnknown));
      end;
    ftIDispatch:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IDispatch));
        Move(TempBuff[0], Buffer[0], SizeOf(IDispatch));
      end;
    ftLargeInt:
    begin
      if PDecimal(@Data)^.sign > 0 then
        TBitConverter.FromLargeInt(-1*PDecimal(@Data)^.Lo64, Buffer)
      else
        TBitConverter.FromLargeInt(PDecimal(@Data)^.Lo64, Buffer);
    end;
    ftBlob..ftTypedBinary, ftVariant, ftWideMemo: TBitConverter.FromVariant(Data, Buffer);
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
      Field.DisplayName]);
  end;
end;
{$ELSE}
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
            StrCopy(Buffer, tagVariant(Data).bStrVal);
          //  WideCharToMultiByte(0, 0, tagVariant(Data).bStrVal, LLength + 1, Buffer,
           //   Field.Size, nil, nil);
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
{$IFEND}
{ TObjectDatasetFieldDefs }

function TObjectDatasetFieldDefs.GetFieldDefClass: TFieldDefClass;
begin
  Result := TObjectDatasetFieldDef;
end;

{ TObjectDatasetFieldDef }


function TObjectDatasetFieldDef.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TObjectDatasetFieldDef.SetRealDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;



end.
