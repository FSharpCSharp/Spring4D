unit Adapters.ObjectDataset;

interface

uses
  Adapters.ObjectDataset.Abstract
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
  protected
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
    procedure DoPostRecord(Index: Integer); override;
    function ConvertPropertyValueToVariant(const AValue: TValue): Variant; virtual;
    procedure InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo); virtual;

    function  GetRecordCount: Integer; override;

    procedure LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs); virtual;
    procedure LoadFieldDefsFromItemType; virtual;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function  IsCursorOpen: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDataList<T: class>(ADataList: IList<T>);
    property DataList: IList read FDataList;
  published
    property DefaultStringFieldLength: Integer read FDefaultStringFieldLength write FDefaultStringFieldLength default 250;

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
  ;

type
  EObjectDatasetException = class(Exception);

{ TObjectDataset }

function TObjectDataset.ConvertPropertyValueToVariant(const AValue: TValue): Variant;
begin
  Result := TUtils.AsVariant(AValue);
end;

constructor TObjectDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TCollections.CreateList<TRttiProperty>;
  FDefaultStringFieldLength := 250;
end;

destructor TObjectDataset.Destroy;
begin
  inherited Destroy;
end;

procedure TObjectDataset.DoDeleteRecord(Index: Integer);
begin
  FDataList.Delete(Index);
end;

procedure TObjectDataset.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  LItem: TValue;
  LProperty: TRttiProperty;
begin
  LItem := DataList[Index];
  if FProperties.IsEmpty then
    InitRttiPropertiesFromItemType(LItem.TypeInfo);

  LProperty := FProperties[Field.FieldNo - 1];
  Value := ConvertPropertyValueToVariant(LProperty.GetValue(TRttiExplorer.GetRawPointer(LItem)));
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
begin
  if State = dsInsert then
    LItem := TRttiExplorer.CreateType(FItemTypeInfo)
  else
    LItem := FDataList[Index];

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
  end;
end;

function TObjectDataset.GetRecordCount: Integer;
begin
  Result := -1;
  if Assigned(FDataList) then
    Result := FDataList.Count;
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
  SetCurrent(-1);
  BookmarkSize := SizeOf(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  if DefaultFields then
    CreateFields;

  BindFields(True);
  SetRecBufSize();
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

procedure TObjectDataset.SetDataList<T>(ADataList: IList<T>);
begin
  FItemTypeInfo := TypeInfo(T);
  FDataList := ADataList.AsList;
end;

end.
