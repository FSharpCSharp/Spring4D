(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Core.Utils;

interface

uses
  Rtti, TypInfo, DB, Graphics, Classes, SysUtils, Mapping.Attributes;

type
  TUtils = class sealed
  public
    class function AsVariant(const AValue: TValue): Variant;
    class function FromVariant(const AValue: Variant): TValue;
    class function TryConvert(const AFrom: TValue; AManager: TObject; ARttiMember: TRttiNamedObject; AEntity: TObject; var AResult: TValue): Boolean;

    class function TryLoadFromStreamToPictureValue(AStream: TStream; out APictureValue: TValue): Boolean;
    class function TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean;
    class function TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;

    class function IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean; overload;
    class function IsEnumerable(ATypeInfo: PTypeInfo; out AEnumeratorMethod: TRttiMethod): Boolean; overload;
    class function SameObject(ALeft, ARight: TObject): Boolean;
    class function SameStream(ALeft, ARight: TStream): Boolean;
    class procedure SetLazyValue(ARttiMember: TRttiNamedObject; AManager: TObject; const AID: TValue; AEntity: TObject; var AResult: TValue);
    class function IsNullableType(ATypeInfo: PTypeInfo): Boolean;
    class function IsLazyType(ATypeInfo: PTypeInfo): Boolean;
    class function TryGetNullableTypeValue(const ANullable: TValue; out AValue: TValue): Boolean;
    class function TryGetLazyTypeValue(const ALazy: TValue; out AValue: TValue): Boolean;
    class function InitLazyRecord(const AFrom: TValue; ATo: PTypeInfo; ARttiMember: TRttiNamedObject; AEntity: TObject): TValue;
  end;

implementation

uses
  Core.Reflection
  ,Core.Exceptions
  ,Core.EntityManager
  ,Mapping.RttiExplorer
  ,Core.EntityCache
  ,jpeg
  ,pngimage
  ,GIFImg
  ,Variants
  ,Generics.Collections
  ,StrUtils
  ;

{ TUtils }

class function TUtils.AsVariant(const AValue: TValue): Variant;
var
  LStream: TStream;
  DataPtr: Pointer;
  LRes: OleVariant;
  LHasValueField, LValueField: TRttiField;
begin
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
      Result := Null;
      if IsNullableType(AValue.TypeInfo) then
      begin
        LHasValueField := AValue.GetType().GetField('FHasValue');
        if LHasValueField.GetValue(AValue.GetReferenceToRawData).AsBoolean then
        begin
          LValueField := AValue.GetType().GetField('FValue');
          Result := TUtils.AsVariant(LValueField.GetValue(AValue.GetReferenceToRawData));
        end;
      end;
    end;
    tkClass:
    begin
      Result := Null;
      if (AValue.AsObject <> nil) then
      begin
        LStream := AValue.AsObject as TStream;

        LRes := VarArrayCreate([0, LStream.Size], varByte);
        DataPtr := VarArrayLock(LRes);
        try
          LStream.ReadBuffer(DataPtr^, LStream.Size);
        finally
          VarArrayUnlock(LRes);
        //  LStream.Free;
        end;

        Result := LRes;
      end;
    end
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
    bStream := TMemoryStream.Create();
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
    Result := TValue.FromVariant(AValue);
end;

class function TUtils.TryGetLazyTypeValue(const ALazy: TValue; out AValue: TValue): Boolean;
var
  LRttiType: TRttiType;
  LValueField: TRttiField;
  LInterfaceMethod: TRttiMethod;
begin
  Result := False;
  if ALazy.Kind = tkRecord then
  begin
    LRttiType := ALazy.GetType();
    LValueField := LRttiType.GetField('FLazy');
    AValue := LValueField.GetValue(ALazy.GetReferenceToRawData);
    Result := (AValue.AsInterface <> nil);
    if Result then
    begin
      LRttiType := AValue.GetType;
      LInterfaceMethod := LRttiType.AsInterface.GetMethod('ValueCreated');
      Result := LInterfaceMethod.Invoke(AValue, []).AsBoolean;
      if Result then
      begin
        LInterfaceMethod := LRttiType.AsInterface.GetMethod('GetValue');
        AValue := LInterfaceMethod.Invoke(AValue, []);
      end;
    end;
  end;
end;

class function TUtils.TryGetNullableTypeValue(const ANullable: TValue; out AValue: TValue): Boolean;
var
  LRttiType: TRttiType;
  LValueField: TRttiField;
begin
  Result := False;
  if ANullable.Kind = tkRecord then
  begin
    LRttiType := ANullable.GetType();
    LValueField := LRttiType.GetField('FHasValue');
    Result := LValueField.GetValue(ANullable.GetReferenceToRawData).AsBoolean;
    if Result then
    begin
      LValueField := LRttiType.GetField('FValue');
      AValue := LValueField.GetValue(ANullable.GetReferenceToRawData);
    end;
  end;
end;

class function TUtils.IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean;
begin
  Result := IsEnumerable(AObject.ClassInfo, AEnumeratorMethod);
end;

class function TUtils.InitLazyRecord(const AFrom: TValue; ATo: PTypeInfo;
  ARttiMember: TRttiNamedObject; AEntity: TObject): TValue;
begin
  {TODO -oLinas -cGeneral : finish lazy record initialization}
end;

class function TUtils.IsEnumerable(ATypeInfo: PTypeInfo;
  out AEnumeratorMethod: TRttiMethod): Boolean;
var
  LCtx: TRttiContext;
begin
  AEnumeratorMethod := LCtx.GetType(ATypeInfo).GetMethod('GetEnumerator');
  Result := Assigned(AEnumeratorMethod);
end;

class function TUtils.IsLazyType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := StartsText('Lazy<', string(ATypeInfo.Name)) or StartsText('LazyObject<', string(ATypeInfo.Name));
end;

class function TUtils.IsNullableType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := StartsText('Nullable<', string(ATypeInfo.Name));
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

class procedure TUtils.SetLazyValue(ARttiMember: TRttiNamedObject; AManager: TObject; const AID: TValue; AEntity: TObject; var AResult: TValue);
var
  LRecord: TRttiRecordType;
  LValueField: TRttiField;
  LCol: ColumnAttribute;
begin
  AResult := TRttiExplorer.GetMemberValue(AEntity, ARttiMember);

  LRecord := TRttiExplorer.GetAsRecord(ARttiMember);
  //simple generic type
  LValueField := LRecord.GetField('FManager');
  LValueField.SetValue(AResult.GetReferenceToRawData, AManager);
  LValueField := LRecord.GetField('FID');
  LValueField.SetValue(AResult.GetReferenceToRawData, AID);
  LValueField := LRecord.GetField('FEntity');
  LValueField.SetValue(AResult.GetReferenceToRawData, AEntity);
  LCol := TEntityCache.Get(AEntity.ClassType).ColumnByMemberName(ARttiMember.Name);
  LValueField := LRecord.GetField('FColumn');
  LValueField.SetValue(AResult.GetReferenceToRawData, LCol);
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
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
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
  LValue: TValue;
  LRecord: TRttiRecordType;
  LValueField, LHasValueField: TRttiField;
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
          LRecord := TRttiExplorer.GetAsRecord(ARttiMember);

          if Assigned(LRecord) then
          begin
            LValueField := LRecord.GetField('FValue');
            LHasValueField := LRecord.GetField('FHasValue');
            TValue.MakeWithoutCopy(nil, LTypeInfo, AResult);
            if AFrom.IsEmpty then
            begin
              LHasValueField.SetValue(AResult.GetReferenceToRawData, False);
            end
            else
            begin
              LHasValueField.SetValue(AResult.GetReferenceToRawData, True);
              //get type from Nullable<T> and set value to this type
              if AFrom.TryConvert(LValueField.FieldType.Handle, LValue, bFree) then
                LValueField.SetValue(AResult.GetReferenceToRawData, LValue);

              if bFree then
              begin
                FreeValueObject(LValue);
              end;
            end;
            Exit(True);
          end;
        end
        else if IsLazyType(LTypeInfo) then
        begin
          {TODO -oLinas -cGeneral : set lazy variables}
          //AFrom value must be ID of lazy type
          if AFrom.IsEmpty then
            raise EORMColumnCannotBeNull.Create('Column for lazy type cannot be null');

          SetLazyValue(ARttiMember, AManager, AFrom, AEntity, AResult);
          Exit(True);
        end;

      end;
    end;
    Result := AFrom.TryConvert(LTypeInfo, AResult, bFree);
    if bFree then
    begin
      FreeValueObject(LValue);
    end;
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
