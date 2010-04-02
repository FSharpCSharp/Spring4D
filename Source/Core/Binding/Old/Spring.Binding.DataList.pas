{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.Binding.DataList;

interface

uses
  DB,
  Classes,
  TypInfo,
  RTTI,
  Spring.Notifications;

type
  /// <remarks>
  /// IDataHolder need to be reconsidered. Paul.
  /// </remarks>
  IDataHolder = interface
    ['{31457624-A625-4CA0-A12D-8FDC7305047F}']
    procedure SetObject(value: TObject);
    function GetItemType: TClass;
    function AsObject(Value: Integer): TObject;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property ItemType: TClass read GetItemType;
  end;

  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Index: Integer;
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;

  TObjectDataSet = class(TDataSet)
  private
    fRttiCtx: TRttiContext;
    fDataHolder: IDataHolder;
    function GetDataHolder: IDataHolder;
    procedure SetDataHolder(const Value: IDataHolder);
    function GetActiveRecordBuffer: PChar;
    function PropertyIsNull(obj: TObject; prop: TRttiProperty): Boolean;
  protected
    // record data and status
    fIsTableOpen: Boolean;
    fRecordSize: Integer; // actual data + housekeeping
    fCurrent: Integer;
    // dataset virtual methods
    procedure InternalInitFieldDefs; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeDelete; override;
    procedure DoClose; virtual;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
  public
    property DataHolder: IDataHolder read GetDataHolder write SetDataHolder;
    constructor Create(AOwner: TComponent); override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure UpdateData;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Forms,
  StrUtils,
  Spring.System;

{ TObjectDataSet }

procedure TObjectDataSet.InternalInitFieldDefs;
var
  ctx: TRttiContext;
  typeinfo: TRttiType;
  prop: TRttiProperty;
  sl: TStringList;
begin
  inherited;
  FieldDefs.Clear;
  ctx := TRttiContext.Create;
  sl := TStringList.Create;
  try
    typeinfo := ctx.GetType(DataHolder.ItemType);
    for prop in typeinfo.GetProperties do
    begin
      case prop.PropertyType.TypeKind of
        tkInteger, tkInt64:
          FieldDefs.Add(prop.Name, ftInteger);
        tkString, tkLString, tkWString, tkUString, tkWChar:
          // *** How to resolve size problem to strings?
          // *** Maybe to use attributes on property?
          FieldDefs.Add(prop.Name, ftString, 255);
        tkFloat:
          begin
            if prop.PropertyType.Name = 'TDateTime' then
              FieldDefs.Add(prop.Name, ftDateTime)
            else if prop.PropertyType.Name = 'Currency' then
            begin
              FieldDefs.Add(prop.Name, DB.ftCurrency);
              TFloatField(FieldDefs[FieldDefs.Count - 1]).Currency := True;
            end else if prop.PropertyType.Name = 'Single' then
              FieldDefs.Add(prop.Name, DB.ftSingle)
            else if prop.PropertyType.Name = 'Extended' then
              FieldDefs.Add(prop.Name, DB.ftExtended)
            else if prop.PropertyType.Name = 'Double' then
              FieldDefs.Add(prop.Name, ftFloat);
          end;
        tkRecord:
          begin
            if prop.PropertyType.Name = 'TNullable<System.string>' then
              FieldDefs.Add(prop.Name, ftString, 255);
            if prop.PropertyType.Name = 'TNullable<System.TDateTime>' then
              FieldDefs.Add(prop.Name, ftDateTime);
            if prop.PropertyType.Name = 'TNullable<System.Currency>' then
              FieldDefs.Add(prop.Name, ftCurrency);
            if prop.PropertyType.Name = 'TNullable<System.Single>' then
              FieldDefs.Add(prop.Name, DB.ftSingle);
            if prop.PropertyType.Name = 'TNullable<System.Extended>' then
              FieldDefs.Add(prop.Name, DB.ftExtended);
            if prop.PropertyType.Name = 'TNullable<System.Double>' then
              FieldDefs.Add(prop.Name, ftFloat);
          end;
        {
        tkSet:
          begin
            prop.en
            GetSetNames( propInfo^.PropType^, sl );
            if sl.Count = 0 then
              FieldDefs.Add( prop.Name, ftString, 255 )
            else
              FieldDefs.Add( prop.Name, ftString, TotalLenWithCommas( sl ) );
          end;
        tkEnumeration:
          begin
            GetEnumNames( propInfo^.PropType^, sl );
            if sl.Count = 0 then
              FieldDefs.Add( prop.Name, ftString, 255 )
            else
              FieldDefs.Add( prop.Name, ftString, MaxStrLen( sl ) + 1 );
          end;
        }
      end;
    end;
  finally
    sl.Free;
    ctx.Free;
  end;
end;

function TObjectDataSet.PropertyIsNull(obj: TObject; prop: TRttiProperty): Boolean;
var
  nullable: TRttiType;
  field: TRttiField;
begin
  Result := True;
  if prop.PropertyType.IsRecord then
  begin
    nullable := fRttiCtx.GetType(prop.PropertyType.Handle);
    field := nullable.GetField('fHasValue');
    if Assigned(field) then
      Result := field.GetValue(prop.GetValue(obj).GetReferenceToRawData).AsString = '';
  end else
    Result := False;
end;

{
  Get property values from FieldName and set field value.
}
function TObjectDataSet.GetFieldData(field: TField;
  Buffer: Pointer): Boolean;
var
  tmpStr: AnsiString;
  propInfo: TRttiProperty;
  typeInfo: TRttiType;
  recInfo: PRecInfo;
  obj: TObject;
  sourceBuffer: PChar;
  nullable: TRttiType;
  objField: TRttiField;
  referenceToRawData: Pointer;
begin
  Result := False;
  sourceBuffer := GetActiveRecordBuffer;
  if not Assigned(SourceBuffer) or not Assigned(Buffer) then
    exit;
  typeInfo := fRttiCtx.GetType(DataHolder.ItemType);
  propInfo := typeInfo.GetProperty(Field.FieldName);
  try
    Result := True;
    recInfo := PRecInfo(sourceBuffer);
    obj := DataHolder.AsObject(recInfo.Index);

    // *** if the nullable type not propInfo.GetValue(Obj).HasValue then do notting
    // *** else do the case below....
    if PropertyIsNull(obj, propInfo) then
      Exit;

    objField := nil;
    referenceToRawData := nil;
    if ContainsStr(propInfo.PropertyType.Name ,'TNullable<') then
    begin
      nullable := fRttiCtx.GetType(propInfo.PropertyType.Handle);
      objField := nullable.GetField('fValue');
      referenceToRawData := propInfo.GetValue(obj).GetReferenceToRawData;
    end;

    case Field.DataType of
      ftInteger:
        if propInfo.PropertyType.Name = 'TNullable<System.Integer>' then
          Double(Buffer^) := objField.GetValue(referenceToRawData).AsInteger
        else
          Integer(Buffer^) := propInfo.GetValue(obj).AsInteger;
      ftDateTime:
        if propInfo.PropertyType.Name = 'TNullable<System.TDateTime>' then
          Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(
            objField.GetValue(referenceToRawData).AsExtended))
        else
          Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(
            propInfo.GetValue(obj).AsExtended));
      ftCurrency:
        if propInfo.PropertyType.Name = 'TNullable<System.Currency>' then
          Double(Buffer^) := objField.GetValue(referenceToRawData).AsCurrency
        else
          Double(Buffer^) := propInfo.GetValue(obj).AsCurrency;
      DB.ftSingle:
        if propInfo.PropertyType.Name = 'TNullable<System.Single>' then
          Double(Buffer^) := objField.GetValue(referenceToRawData).AsExtended
        else
          Double(Buffer^) := propInfo.GetValue(obj).AsExtended;
      ftFloat:
        if propInfo.PropertyType.Name = 'TNullable<System.Double>' then
          Double(Buffer^) := objField.GetValue(referenceToRawData).AsExtended
        else
          Double(Buffer^) := propInfo.GetValue(obj).AsExtended;
      DB.ftExtended:
        if propInfo.PropertyType.Name = 'TNullable<System.Extended>' then
          Double(Buffer^) := objField.GetValue(referenceToRawData).AsExtended
        else
          Double(Buffer^) := propInfo.GetValue(obj).AsExtended;
      ftString:
        if propInfo.PropertyType.Name = 'TNullable<System.string>' then
        begin
          tmpStr := AnsiString(objField.GetValue(referenceToRawData).AsString);
          StrMove(Buffer, PAnsiChar(tmpStr), Length(tmpStr) + 1);
        end
        else
          begin
            tmpStr := AnsiString(propInfo.GetValue(obj).AsString);
            StrMove(Buffer, PAnsiChar(tmpStr), Length(tmpStr) + 1);
          end;
    end;
  finally
    propInfo.Free;
    typeInfo.Free;
  end;
end;

procedure TObjectDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  propInfo: TRttiProperty;
  typeInfo: TRttiType;
  recInfo: PRecInfo;
  obj: TObject;
  sourceBuffer: PChar;
  nullable: TRttiType;
  objField: TRttiField;
  referenceToRawData: Pointer;
begin
  sourceBuffer := GetActiveRecordBuffer;
  if not Assigned(SourceBuffer) or not Assigned(Buffer) then
    exit;
  typeInfo := fRttiCtx.GetType(DataHolder.ItemType);
  propInfo := typeInfo.GetProperty(Field.FieldName);
  try
    recInfo := PRecInfo(sourceBuffer);
    obj := DataHolder.AsObject(recInfo.Index);

    objField := nil;
    referenceToRawData := nil;
    if ContainsStr(propInfo.PropertyType.Name ,'TNullable<') then
    begin
      nullable := fRttiCtx.GetType(propInfo.PropertyType.Handle);
      objField := nullable.GetField('fValue');
      referenceToRawData := propInfo.GetValue(obj).GetReferenceToRawData;
    end;
    // *** if field is null then has to set null on property if is nullable property
    // *** else do the case below....
    case Field.DataType of
      ftInteger:
        if propInfo.PropertyType.Name = 'TNullable<System.Integer>' then
          objField.SetValue(referenceToRawData, TValue.From<Integer>(Integer(Buffer^)))
        else
          propInfo.SetValue(Obj, Integer(Buffer^));
      ftDateTime:
        if propInfo.PropertyType.Name = 'TNullable<TDateTime>' then
          objField.SetValue(referenceToRawData,
            TValue.From<TDateTime>(TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)))))
        else
          propInfo.SetValue(Obj,
            TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^))));
      ftCurrency:
        if propInfo.PropertyType.Name = 'TNullable<System.Currency>' then
          objField.SetValue(referenceToRawData, TValue.From<Currency>(Double(Buffer^)))
        else
          propInfo.SetValue(Obj, Double(Buffer^));
      ftFloat:
        if propInfo.PropertyType.Name = 'TNullable<System.Double>' then
          objField.SetValue(referenceToRawData, TValue.From<Double>(Double(Buffer^)))
        else
          propInfo.SetValue(Obj, Double(Buffer^));
      DB.ftSingle:
        if propInfo.PropertyType.Name = 'TNullable<System.Single>' then
          objField.SetValue(referenceToRawData, TValue.From<Single>(Single(Buffer^)))
        else
          propInfo.SetValue(Obj, Single(Buffer^));
      ftString:
        if propInfo.PropertyType.Name = 'TNullable<System.string>' then
          objField.SetValue(referenceToRawData, TValue.From<string>(string(StrPas(PAnsiChar(Buffer)))))
        else
          propInfo.SetValue(Obj, string(StrPas(PAnsiChar(Buffer))));
    end;
  finally
    propInfo.Free;
    typeInfo.Free;
    DataEvent(deDataSetChange, 0); // update others controls
    CheckBrowseMode; // Change state of record
  end;
end;

////////////////// here is generic /////////////////////////

function TObjectDataSet.GetActiveRecordBuffer: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Result := nil
      else
        Result := PChar(ActiveBuffer);
    dsEdit, dsInsert:
      Result := PChar(ActiveBuffer);
  else
    Result := nil;
  end;
end;

function TObjectDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := nil;
  ReallocMem(Result, FRecordSize);
end;

procedure TObjectDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, FRecordSize, 0);
end;

procedure TObjectDataSet.FreeRecordBuffer (var Buffer: TRecordBuffer);
begin
  ReallocMem(Buffer, 0);
end;

procedure TObjectDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer).Bookmark;
end;

function TObjectDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer).BookmarkFlag;
end;

function TObjectDataSet.GetRecNo: Integer;
begin
  Result := FCurrent + 1;
end;

function TObjectDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if fCurrent < fDataHolder.Count - 1 then
        Inc(fCurrent)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if fCurrent > 0 then
        Dec(fCurrent)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if fCurrent >= fDataHolder.Count then
        Result := grEOF;
  end;
  if Result = grOK then // read the data
    with PRecInfo(Buffer)^ do
    begin
      Index := fCurrent;
      BookmarkFlag := bfCurrent;
      Bookmark := fCurrent;
    end;
end;

function TObjectDataSet.GetRecordCount: Integer;
begin
  Result := fDataHolder.Count;
end;

function TObjectDataSet.GetRecordSize: Word;
begin
  Result := 4; // actual data without house-keeping
end;

procedure TObjectDataSet.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  DoClose;
  FIsTableOpen := False;
end;

procedure TObjectDataSet.DoClose;
begin
  FCurrent := -1;
end;

procedure TObjectDataSet.InternalFirst;
begin
  if IsEmpty then
    FCurrent := -1
  else
    FCurrent := 0;
end;

procedure TObjectDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  if (Bookmark <> nil) then
    FCurrent := Integer (Bookmark);
end;

procedure TObjectDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TObjectDataSet.InternalLast;
begin
  FCurrent := fDataHolder.Count;
end;

procedure TObjectDataSet.InternalOpen;
begin
  if not Assigned(DataHolder) then
    raise Exception.Create('List is not defined!');
  FCurrent := -1;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);
  FRecordSize := SizeOf(TRecInfo);
  BookmarkSize := SizeOf(Integer);
  FIsTableOpen := True;
end;

procedure TObjectDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  FCurrent := PRecInfo(Buffer).Index;
end;

function TObjectDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsTableOpen;
end;

procedure TObjectDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer).Bookmark := PInteger(Data)^;
end;

procedure TObjectDataSet.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PRecInfo(Buffer).BookmarkFlag := Value;
end;

procedure TObjectDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 0) or (Value > fDataHolder.Count) then
    raise Exception.Create('SetRecNo: out of range');
  FCurrent := Value - 1;
end;

procedure TObjectDataSet.UpdateData;
begin
  DataEvent(deDataSetChange, 0);
  CheckBrowseMode;
end;

function TObjectDataSet.GetDataHolder: IDataHolder;
begin
  Result := fDataHolder;
end;

procedure TObjectDataSet.SetDataHolder(const Value: IDataHolder);
begin
  if fDataHolder <> Value then
  begin
    if Active then
      Close;
    fDataHolder := Value;
  end;
end;

procedure TObjectDataSet.DoBeforeInsert;
begin
  SysUtils.Abort;
end;

constructor TObjectDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRttiCtx := TRttiContext.Create;
end;

destructor TObjectDataSet.Destroy;
begin
  fRttiCtx.Free;
  inherited;
end;

procedure TObjectDataSet.DoBeforeDelete;
begin
  SysUtils.Abort;
end;

end.
