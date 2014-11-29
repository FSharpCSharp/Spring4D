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

unit Spring.Persistence.Core.Utils;

interface

uses
  AnsiStrings,
  Classes,
  DB,
  Graphics,
  Rtti,
  SysUtils,
  TypInfo,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Collections;

type
  TUtils = class sealed
  public
    class function LoadFromStreamToVariant(AStream: TStream): OleVariant;

    class function AsVariant(const AValue: TValue): Variant;
    class function FromVariant(const AValue: Variant): TValue;
    class function ColumnFromVariant(const AValue: Variant; const AColumn: TColumnData; ASession: TObject; AEntity: TObject): TValue;

    class function GetResultsetFromVariant(const AValue: Variant): IDBResultset;

    class function TryConvert(const AFrom: TValue; AManager: TObject; ARttiMember: TRttiNamedObject; AEntity: TObject; var AResult: TValue): Boolean;

    class function TryGetNullableTypeValue(const ANullable: TValue; out AValue: TValue): Boolean;
    class function TryGetLazyTypeValue(const ALazy: TValue; out AValue: TValue): Boolean;
    class function TryGetPrimaryKeyColumn(AColumns: IList<TColumnData>; out AColumn: TColumnData): Boolean;
    class function TryLoadFromStreamToPictureValue(AStream: TStream; out APictureValue: TValue): Boolean;
    class function TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean;
    class function TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;

    class function IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean; overload;
    class function IsEnumerable(ATypeInfo: PTypeInfo; out AEnumeratorMethod: TRttiMethod): Boolean; overload;
    class function IsEnumerable(ATypeInfo: PTypeInfo): Boolean; overload;
    class function IsEnumerable(value: TValue; out objectList: IObjectList): Boolean; overload;
    class function IsNullableType(ATypeInfo: PTypeInfo): Boolean;
    class function IsLazyType(ATypeInfo: PTypeInfo): Boolean;
    class function IsPageType(ATypeInfo: PTypeInfo): Boolean;

    class function SameObject(ALeft, ARight: TObject): Boolean;
    class function SameStream(ALeft, ARight: TStream): Boolean;

    class procedure SetNullableValue(ARttiMember: TRttiNamedObject; const AFrom: TValue; var AResult: TValue);
  end;

implementation

uses
  GIFImg,
  jpeg,
  pngimage,
  StrUtils,
  Variants,
  Spring.Reflection,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.Types,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection.Activator,
  Spring
  ;

type
  THackedSession = class(TSession);

{ TUtils }

class function TUtils.AsVariant(const AValue: TValue): Variant;
var
  LStream: TStream;
  LValue: TValue;
  LPersist: IStreamPersist;
begin
  Result := Null;
  case AValue.Kind of
    tkEnumeration:
    begin
      if AValue.TypeInfo = TypeInfo(Boolean) then
        Result := AValue.AsBoolean
      else
        Result := AValue.AsOrdinal;
    end;
    tkFloat:
    begin
      if (AValue.TypeInfo = TypeInfo(TDateTime)) then
        Result := AValue.AsType<TDateTime>
      else if (AValue.TypeInfo = TypeInfo(TDate)) then
        Result := AValue.AsType<TDate>
      else
        Result := AValue.AsExtended;
    end;
    tkRecord:
    begin
      if IsNullableType(AValue.TypeInfo) then
      begin
        if TryGetNullableTypeValue(AValue, LValue) then
          Result := TUtils.AsVariant(LValue);
      end;
    end;
    tkClass:
    begin
      if (AValue.AsObject <> nil) then
      begin
        if (AValue.AsObject is TStream) then
        begin
          LStream := TStream(AValue.AsObject);
          LStream.Position := 0;
          Result := LoadFromStreamToVariant(LStream);
        end
        else if (AValue.AsObject is TPicture) then
        begin
          LStream := TMemoryStream.Create;
          try
            TPicture(AValue.AsObject).Graphic.SaveToStream(LStream);
            LStream.Position := 0;
            Result := LoadFromStreamToVariant(LStream);
          finally
            LStream.Free;
          end;
        end   //somehow this started to fail recently. needed to add additional condition for TPicture
        else if Supports(AValue.AsObject, IStreamPersist, LPersist) then
        begin
          LStream := TMemoryStream.Create;
          try
            LPersist.SaveToStream(LStream);
            LStream.Position := 0;
            Result := LoadFromStreamToVariant(LStream);
          finally
            LStream.Free;
          end;
        end;
      end;
    end;
    tkInterface: ;//
    else
    begin
      Result := AValue.AsVariant;
    end;
  end;
end;


class function TUtils.FromVariant(const AValue: Variant): TValue;
var
  bStream: TMemoryStream;
  ptr: Pointer;
  iDim: Integer;
begin
  if VarIsArray(AValue) then
  begin
    //make stream from variant
    bStream := TMemoryStream.Create;
    iDim := VarArrayDimCount(AValue);
    ptr := VarArrayLock(AValue);
    try
      bStream.Write(ptr^, VarArrayHighBound(AValue, iDim) + 1);
    finally
      VarArrayUnlock(AValue);
    end;
    Result := bStream;
  end
  else
  begin
    case VarType(AValue) of
      273 {FMTBcdVariantType.VarType}: Result := Double(AValue); //Oracle sometimes returns this vartype for some columns
      else
        Result := TValue.FromVariant(AValue);
    end;
  end;
end;

class function TUtils.GetResultsetFromVariant(
  const AValue: Variant): IDBResultset;
var
  LIntf: IInterface;
begin
  LIntf := AValue;
  Result := LIntf as IDBResultset;
end;

class function TUtils.ColumnFromVariant(const AValue: Variant;
  const AColumn: TColumnData; ASession: TObject; AEntity: TObject): TValue;
var
  LEmbeddedEntityResultset: IDBResultset;
  LIntf: IInterface;
  LNewEntity: TObject;
  LSession: THackedSession;
  LEnumMethod: TRttiMethod;
  LList: TValue;
begin
  case VarType(AValue) of
    varUnknown:
    begin
      LEmbeddedEntityResultset := GetResultsetFromVariant(AValue);
      LSession := THackedSession(ASession);
      if IsEnumerable(AColumn.TypeInfo, LEnumMethod) then
      begin
        LList := TRttiExplorer.GetMemberValueDeep(AEntity, AColumn.MemberName);
        LIntf := LList.AsInterface;
        if TRttiExplorer.GetLastGenericArgumentType(AColumn.TypeInfo).IsInstance then
          LSession.SetInterfaceList(LIntf, LEmbeddedEntityResultset, AColumn.TypeInfo)
        else
          LSession.SetSimpleInterfaceList(LIntf, LEmbeddedEntityResultset, AColumn.TypeInfo);
        Result := TValue.From(LIntf);
      end
      else
      begin
        LNewEntity := TActivator.CreateInstance(AColumn.TypeInfo).AsObject;
        LSession.DoMapEntity(LNewEntity, LEmbeddedEntityResultset, nil);
        Result := TValue.From(LNewEntity, LNewEntity.ClassType);
      end;
    end
    else
    begin
      Result := FromVariant(AValue);
    end;
  end;
end;

class function TUtils.TryGetLazyTypeValue(const ALazy: TValue; out AValue: TValue): Boolean;
var
  lazyType: TRttiType;
  lazyField: TRttiField;
  lazy: ILazy;
begin
  Result := False;
  if ALazy.Kind = tkRecord then
  begin
    lazyType := TType.GetType(ALazy);
    lazyField := lazyType.GetField('FLazy');
    lazy := lazyField.GetValue(ALazy.GetReferenceToRawData).AsInterface as ILazy;
    Result := (lazy <> nil) and (lazy.IsValueCreated);
    if Result then
    begin
      AValue := lazy.Value;
    end;
  end;
end;

class function TUtils.TryGetNullableTypeValue(const ANullable: TValue; out AValue: TValue): Boolean;
var
  LRttiType: TRttiType;
  LValueField: TRttiField;
  LFields: TArray<TRttiField>;
  LHasValue: TValue;
  LRef: Pointer;
begin
  Result := False;
  if ANullable.Kind = tkRecord then
  begin
    LRttiType := TType.GetType(ANullable);
    LFields := LRttiType.GetFields;
    //get FHasValue field
    LValueField := LFields[1]; // LRttiType.GetField('FHasValue');
    LRef := ANullable.GetReferenceToRawData;
    LHasValue := LValueField.GetValue(LRef);
    Result := ((LHasValue.TypeInfo = TypeInfo(string)) and ((LHasValue.AsString = '@'))); //Spring Nullable

    if Result then
    begin
      LValueField := LFields[0]; // LRttiType.GetField('FValue');
      AValue := LValueField.GetValue(LRef);
    end;
  end;
end;

class function TUtils.TryGetPrimaryKeyColumn(AColumns: IList<TColumnData>;
  out AColumn: TColumnData): Boolean;
var
  LCol: TColumnData;
begin
  for LCol in AColumns do
  begin
    if (cpPrimaryKey in LCol.Properties) then
    begin
      AColumn := LCol;
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TUtils.IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean;
begin
  Result := IsEnumerable(AObject.ClassInfo, AEnumeratorMethod);
end;

class function TUtils.IsEnumerable(ATypeInfo: PTypeInfo;
  out AEnumeratorMethod: TRttiMethod): Boolean;
var
  LCtx: TRttiContext;
begin
  AEnumeratorMethod := LCtx.GetType(ATypeInfo).GetMethod('GetEnumerator');
  Result := Assigned(AEnumeratorMethod);
end;

class function TUtils.IsEnumerable(ATypeInfo: PTypeInfo): Boolean;
var
  LEnumeratorMethod: TRttiMethod;
begin
  Result := IsEnumerable(ATypeInfo, LEnumeratorMethod);
end;

class function TUtils.IsEnumerable(value: TValue; out objectList: IObjectList): Boolean;
begin
  Result := value.IsInterface;
  if Result then
    Result := Supports(value.AsInterface, IObjectList, objectList);
end;

class function TUtils.IsLazyType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ( PosEx('Lazy', string(ATypeInfo.Name)) = 1 ) and (ATypeInfo.Kind = tkRecord);
end;

class function TUtils.IsNullableType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ( PosEx('Nullable', string(ATypeInfo.Name)) = 1 ) and (ATypeInfo.Kind = tkRecord);
end;

class function TUtils.IsPageType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo.Kind = tkInterface) and ( PosEx('IDBPage<', string(ATypeInfo.Name)) = 1 );
end;

class function TUtils.LoadFromStreamToVariant(AStream: TStream): OleVariant;
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

class function TUtils.SameObject(ALeft, ARight: TObject): Boolean;
//var
//  LEnumMethod: TRttiMethod;
begin
  Result := ALeft = ARight;
  if Result then
    Exit;

  Result := (ALeft <> nil) and (ARight <> nil);
  if not Result then
    Exit;

  //check for supported types
  Result := (ALeft.ClassType = ARight.ClassType);
  if Result then
  begin
    if (ALeft is TPicture) then
    begin
      Result := TPicture(ALeft).Graphic = TPicture(ARight).Graphic;
      if Result then
        Exit;

      Result := (TPicture(ALeft).Graphic <> nil) and (TPicture(ARight).Graphic <> nil);
      if Result then
      begin
        Result := TPicture(ALeft).Graphic.Equals(TPicture(ARight).Graphic);
      end;
    end
    else if (ALeft is TStream) then
    begin
      Result := SameStream(TStream(ALeft), TStream(ARight));
    end;
    {else if (IsEnumerable(ALeft, LEnumMethod)) then
    begin

    end;}
  end;
end;

class function TUtils.SameStream(ALeft, ARight: TStream): Boolean;
const
  Block_Size = 4096;
var
  Buffer_1: array[0..Block_Size-1] of byte;
  Buffer_2: array[0..Block_Size-1] of byte;
  Buffer_Length: Integer;
begin
  Result := False;

  if ALeft.Size <> ARight.Size then
    Exit;

  while ALeft.Position < ALeft.Size do
  begin
    Buffer_Length := ALeft.Read(Buffer_1, Block_Size);
    ARight.Read(Buffer_2, Block_Size);

    if not CompareMem(@Buffer_1, @Buffer_2, Buffer_Length) then
      Exit;
  end;

  Result := True;
end;

class procedure TUtils.SetNullableValue(ARttiMember: TRttiNamedObject; const AFrom: TValue; var AResult: TValue);
var
  LRecord: TRttiRecordType;
  LFields: TArray<TRttiField>;
  LValueField, LHasValueField: TRttiField;
  LValue: TValue;
  LResultRef: Pointer;
  bFree: Boolean;
begin
  bFree := False;
  LRecord := TRttiExplorer.GetAsRecord(ARttiMember);
  if Assigned(LRecord) then
  begin
    LFields := LRecord.GetFields;
    Assert(Length(LFields) = 2);
    LValueField := LFields[0];
    LHasValueField := LFields[1];
    TValue.MakeWithoutCopy(nil, LRecord.Handle, AResult);
    LResultRef := AResult.GetReferenceToRawData;
    if AFrom.IsEmpty then
    begin
      TValue.From<string>('').ExtractRawData(PByte(LResultRef) + LHasValueField.Offset); // LHasValueField.SetValue(LResultRef, '') //Spring Nullable
    end
    else
    begin
      TValue.From<string>('@').ExtractRawData(PByte(LResultRef) + LHasValueField.Offset);
        //LHasValueField.SetValue(LResultRef, '@')  //Spring Nullable

      //get type from Nullable<T> and set value to this type
      if AFrom.TryConvert(LValueField.FieldType.Handle, LValue, bFree) then
        LValue.ExtractRawData(PByte(LResultRef) + LValueField.Offset); // faster than LValueField.SetValue(LResultRef, LValue);

      if bFree then
      begin
        FreeValueObject(LValue);
      end;
    end;
  end;
end;

const
  MinGraphicSize = 44; //we may test up to & including the 11th longword

function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    else if {$IFDEF DELPHIXE2_UP}AnsiStrings.{$ENDIF}StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
      GraphicClass := TGIFImage
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  Buffer: PByte;
  CurPos: Int64;
  BytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    Buffer := TCustomMemoryStream(Stream).Memory;
    CurPos := Stream.Position;
    Inc(Buffer, CurPos);
    Result := FindGraphicClass(Buffer^, Stream.Size - CurPos, GraphicClass);
    Exit;
  end;
  GetMem(Buffer, MinGraphicSize);
  try
    BytesRead := Stream.Read(Buffer^, MinGraphicSize);
    Stream.Seek(-BytesRead, soCurrent);
    Result := FindGraphicClass(Buffer^, BytesRead, GraphicClass);
  finally
    FreeMem(Buffer);
  end;
end;

class function TUtils.TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;
var
  LGraphic: TGraphic;
  LGraphicClass: TGraphicClass;
  LStream: TMemoryStream;
begin
  Result := False;
  LGraphic := nil;
  LStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    LStream.CopyFrom(AStream, AStream.Size);
    if LStream.Size = 0 then
    begin
      AToPicture.Assign(nil);
      Exit(True);
    end;
    if not FindGraphicClass(LStream.Memory^, LStream.Size, LGraphicClass) then
      Exit;
     // raise EInvalidGraphic.Create(SInvalidImage);
    LGraphic := LGraphicClass.Create;
    LStream.Position := 0;
    LGraphic.LoadFromStream(LStream);
    AToPicture.Assign(LGraphic);
    Result := True;
  finally
    LStream.Free;
    LGraphic.Free;
  end;
end;

class function TUtils.TryLoadFromStreamToPictureValue(AStream: TStream;
  out APictureValue: TValue): Boolean;
var
  LPic: TPicture;
begin
  LPic := TPicture.Create;
  Result := TryLoadFromStreamSmart(AStream, LPic);
  if Result then
    APictureValue := LPic;
end;

class function TUtils.TryConvert(const AFrom: TValue; AManager: TObject; ARttiMember: TRttiNamedObject; AEntity: TObject; var AResult: TValue): Boolean;
var
  LTypeInfo: PTypeInfo;
  bFree: Boolean;
begin
  bFree := False;
  LTypeInfo := ARttiMember.GetTypeInfo;
  if (AFrom.TypeInfo <> LTypeInfo) then
  begin
    case LTypeInfo.Kind of
      tkRecord:
      begin
        if IsNullableType(LTypeInfo) then
        begin
          SetNullableValue(ARttiMember, AFrom, AResult);
          Exit(True);
        end
        else if IsLazyType(LTypeInfo) then
        begin
          //AFrom value must be ID of lazy type
          if AFrom.IsEmpty then
            raise EORMColumnCannotBeNull.Create('Column for lazy type cannot be null');
          Exit(True);
        end;

      end;
    end;
    Result := AFrom.TryConvert(LTypeInfo, AResult, bFree);
  end
  else
  begin
    Result := True;
    AResult := AFrom;
  end;
end;

class function TUtils.TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean;
var
  LGraphic: TGraphic;
  LGraphicClass: TGraphicClass;
  LStream: TMemoryStream;
  LField: TBlobField;
begin
  Assert(AField is TBlobField);
  Assert(Assigned(AToPicture));
  Result := False;
  LField := AField as TBlobField;
  LGraphic := nil;
  LStream := TMemoryStream.Create;
  try
    LField.SaveToStream(LStream);
    if LStream.Size = 0 then
    begin
      AToPicture.Assign(nil);
      Exit(True);
    end;
    if not FindGraphicClass(LStream.Memory^, LStream.Size, LGraphicClass) then
      Exit;
      //raise EInvalidGraphic.Create(SInvalidImage);
    LGraphic := LGraphicClass.Create;
    LStream.Position := 0;
    LGraphic.LoadFromStream(LStream);
    AToPicture.Assign(LGraphic);
    Result := True;
  finally
    LStream.Free;
    LGraphic.Free;
  end;
end;

end.
