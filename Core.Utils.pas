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
  Rtti, TypInfo, DB, Graphics, Classes, SysUtils;

type
  TUtils = class sealed
  public
    class function AsVariant(const AValue: TValue): Variant;
    class function FromVariant(const AValue: Variant): TValue;
    class function TryConvert(const AFrom: TValue; ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;

    class function TryLoadFromStreamToPictureValue(AStream: TStream; out APictureValue: TValue): Boolean;
    class function TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean;
    class function TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;

    class function IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean;
    class function SameObject(ALeft, ARight: TObject): Boolean;
    class function SameStream(ALeft, ARight: TStream): Boolean;
  end;

implementation

uses
  Core.Reflection
  ,jpeg
  ,pngimage
  ,GIFImg
  ,Variants
  ,Generics.Collections
  ;

{ TUtils }

class function TUtils.AsVariant(const AValue: TValue): Variant;
var
  LStream: TStream;
  DataPtr: Pointer;
  LRes: OleVariant;
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
    tkClass:
    begin
      Result := Null;
      if (AValue.AsObject <> nil) then
      begin
        LStream := AValue.AsObject as TStream;

        LRes := VarArrayCreate([0, LStream.Size-1], varByte);
        DataPtr := VarArrayLock(LRes);
        try
          LStream.ReadBuffer(DataPtr^, LStream.Size);
        finally
          VarArrayUnlock(LRes);
          LStream.Free;
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
      bStream.WriteBuffer(ptr^, VarArrayHighBound(AValue, iDim));
      Result := bStream;
    finally
      VarArrayUnlock(AValue);
    end;
  end
  else
    Result := TValue.FromVariant(AValue);
end;

class function TUtils.IsEnumerable(AObject: TObject; out AEnumeratorMethod: TRttiMethod): Boolean;
var
  LCtx: TRttiContext;
begin
  AEnumeratorMethod := LCtx.GetType(AObject.ClassInfo).GetMethod('GetEnumerator');
  Result := Assigned(AEnumeratorMethod);
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

class function TUtils.TryConvert(const AFrom: TValue; ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
begin
  if (AFrom.TypeInfo <> ATypeInfo) then
  begin
    Result := AFrom.TryConvert(ATypeInfo, AResult);
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
