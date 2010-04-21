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

unit Spring.Binding.DB;

interface

uses
  Classes,
  Windows,
  TypInfo,
  SysUtils,
  RTTI,
  DB,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Notifications,
  Spring.Binding;

type
  TInitializeFieldsEvent = reference to procedure(sender: TDataSet);

  /// <summary>
  /// TBindableDataSet
  /// </summary>
  /// Consider IFieldBindingManager, IFieldBufferManager
  TBindableDataSet = class abstract(TDataSet)
  protected
    type
      PRecInfo = ^TRecInfo;
      TRecInfo = record
        Index: Integer;
        Bookmark: Longint;
        BookmarkFlag: TBookmarkFlag;
      end;

      const
        fCRecordSize = 4; // actual data without house-keeping
  private
    fContext: TRttiContext;
    fOnInitializeFields: TDataSetNotifyEvent;
    fFieldMappings: TDictionary<string, IBindable>;
  protected
    fIsOpen: Boolean;
    fRecordBufferSize: Integer; // actual data without housekeeping
    fActiveRecordIndex: Integer;
    function GetActiveRecordBuffer: TRecordBuffer;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeDelete; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecordSize: Word; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
  protected
    { Abstract Methods }
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    property FieldMappings: TDictionary<string, IBindable> read fFieldMappings;
    property OnInitializeFields: TDataSetNotifyEvent read fOnInitializeFields write fOnInitializeFields;
  end;

resourcestring
  SBindingSourceMissing = 'Binding Source is missing.';

implementation

uses
  Forms,
  StrUtils,
  Spring.Helpers;

function TryCopyValueToFieldBuffer(const value: TValue; buffer: Pointer): Boolean;
var
  p: Pointer;
  underlyingValue: TValue;
  n: Integer;
begin
  if value.IsEmpty then Exit(False);
  Result := True;
  p := value.GetReferenceToRawData;
  if not IsManaged(value.TypeInfo) then
  begin
    CopyMemory(buffer, p, value.DataSize);
  end
  else if value.TypeInfo.Kind in [tkUString, tkWString] then  // TEMP
  begin
    n := ByteLength(PString(p)^);
    StrLCopy(PWideChar(buffer), PPointer(p)^, n);  // BUG
  end
  else if value.TypeInfo.Kind in [tkString, tkLString] then
  begin
    StrLCopy(PAnsiChar(buffer), PPointer(p)^, Length(PAnsiString(p)^));
  end
  else if TryGetUnderlyingValue(value, underlyingValue) then
  begin
    Result := TryCopyValueToFieldBuffer(underlyingValue, buffer);
  end
  else
  begin
    Result := False;
  end;
end;

procedure MakeFieldBufferToValue(buffer: Pointer; valueType: PTypeInfo; var value: TValue);
var
  us: UnicodeString;
  ansi: AnsiString;
  p: Pointer;
  underlyingType: PTypeInfo;
  valueBuffer: TBytes;
begin
  if buffer = nil then // TODO: TEMP (Special CASE: TNullable<T>)
  begin
    TValue.Make(nil, valueType, value);
    Exit;
  end;
  p := buffer;
  if valueType.Kind in [tkUString, tkWString, tkWChar] then
  begin
    us := StrPas(PChar(buffer));
    p := Pointer(us);
  end
  else if valueType.Kind in [tkLString, tkString, tkChar] then
  begin
    ansi := StrPas(PAnsiChar(buffer));
    p := Pointer(ansi);
  end
  else if TryGetUnderlyingTypeInfo(valueType, underlyingType) then
  begin
    SetLength(valueBuffer, GetTypeData(valueType).RecSize);
    ZeroMemory(PByte(valueBuffer), Length(valueBuffer));
    if not IsManaged(underlyingType) then
    begin
      Move(PByte(p)^, PByte(valueBuffer)^, Length(valueBuffer) - SizeOf(string));
    end
    else if underlyingType.Kind in [tkWString, tkUString] then
    begin
      us := StrPas(PChar(p));
      PPointer(@valueBuffer[0])^ := Pointer(us);
    end;
    if buffer <> nil then
    begin
      PString(PByte(valueBuffer) + Length(valueBuffer) - SizeOf(string))^ := '@';
    end;
    p := PByte(valueBuffer);
    TValue.Make(p, valueType, value);    // TODO: TValue.Make, p or @p?
    Exit;
  end;
  TValue.Make(@p, valueType, value);
end;

{ TBindableDataSet }

constructor TBindableDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fContext := TRttiContext.Create;
  fFieldMappings := TDictionary<string, IBindable>.Create;
  fRecordBufferSize := SizeOf(TRecInfo);
  BookmarkSize := SizeOf(Integer);
end;

destructor TBindableDataSet.Destroy;
begin
  fFieldMappings.Free;
  fContext.Free;
  inherited;
end;

procedure TBindableDataSet.InternalInitFieldDefs;
begin
  if Assigned(fOnInitializeFields) then
  begin
    fOnInitializeFields(Self);
  end;
end;

function TBindableDataSet.GetFieldData(field: TField;
  Buffer: Pointer): Boolean;
var
  bindable: IBindable;
begin
  Result := fFieldMappings.TryGetValue(field.FieldName, bindable) and
    TryCopyValueToFieldBuffer(bindable.Value, buffer);
end;

procedure TBindableDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  bindable: IBindable;
  value: TValue;
begin
  if not fFieldMappings.TryGetValue(field.FieldName, bindable) then Exit;
  MakeFieldBufferToValue(buffer, bindable.ValueType, value);
  bindable.SetValue(value);
  DataEvent(deFieldChange, Integer(Field));
end;

function TBindableDataSet.GetActiveRecordBuffer: TRecordBuffer;
begin
  case State of
    dsBrowse:
      begin
        if IsEmpty then
          Result := nil
        else
          Result := ActiveBuffer;
      end;
    dsEdit, dsInsert:
      begin
        Result := ActiveBuffer;
      end;
    else
      begin
        Result := nil;
      end;
  end;
end;

function TBindableDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(fRecordBufferSize);
end;

procedure TBindableDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  ZeroMemory(Buffer, fRecordBufferSize);
end;

procedure TBindableDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
end;

procedure TBindableDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer).Bookmark;
end;

function TBindableDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer).BookmarkFlag;
end;

function TBindableDataSet.GetRecNo: Integer;
begin
  Result := fActiveRecordIndex + 1;
end;

// SURE??
function TBindableDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if fActiveRecordIndex < RecordCount - 1 then
        Inc(fActiveRecordIndex)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if fActiveRecordIndex > 0 then
        Dec(fActiveRecordIndex)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if fActiveRecordIndex >= RecordCount then
        Result := grEOF;
  end;
  if Result = grOK then // read the data
  with PRecInfo(Buffer)^ do
  begin
    Index := fActiveRecordIndex;
    BookmarkFlag := bfCurrent;
    Bookmark := fActiveRecordIndex;
  end;
end;

function TBindableDataSet.GetRecordCount: Integer;
begin
  Result := 1; // TODO: GetRecordCount
end;

function TBindableDataSet.GetRecordSize: Word;
begin
  Result := fCRecordSize;
end;

procedure TBindableDataSet.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
  begin
    DestroyFields;
  end;
  fActiveRecordIndex := -1;
  fIsOpen := False;
end;

procedure TBindableDataSet.InternalFirst;
begin
  if IsEmpty then
    fActiveRecordIndex := -1
  else
    fActiveRecordIndex := 0;
end;

procedure TBindableDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  if (Bookmark <> nil) then
  begin
    fActiveRecordIndex := Integer(Bookmark);
  end;
end;

procedure TBindableDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TBindableDataSet.InternalLast;
begin
  fActiveRecordIndex := RecordCount; // SURE?
end;

procedure TBindableDataSet.InternalOpen;
begin
  fActiveRecordIndex := -1;
  InternalInitFieldDefs;
  if DefaultFields then
  begin
    CreateFields;
  end;
  BindFields(True);
  fIsOpen := True;
end;

procedure TBindableDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fActiveRecordIndex := PRecInfo(Buffer).Index;
end;

function TBindableDataSet.IsCursorOpen: Boolean;
begin
  Result := fIsOpen;
end;

procedure TBindableDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer).Bookmark := PInteger(Data)^;
end;

procedure TBindableDataSet.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PRecInfo(Buffer).BookmarkFlag := Value;
end;

procedure TBindableDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 0) or (Value > RecordCount) then
    raise Exception.Create('SetRecNo: out of range');
  fActiveRecordIndex := Value - 1;
end;

procedure TBindableDataSet.DoBeforeInsert;
begin
  SysUtils.Abort;
end;

procedure TBindableDataSet.DoBeforeDelete;
begin
  SysUtils.Abort;
end;

end.
