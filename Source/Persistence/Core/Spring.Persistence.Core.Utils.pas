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
  Classes,
  DB,
  Rtti,
  TypInfo,
  Spring.Collections,
  Spring.Persistence.Core.Graphics,
  Spring.Persistence.Core.Interfaces;

type
  TUtils = class sealed
  public
    class function LoadFromStreamToVariant(const stream: TStream): OleVariant;

    class function AsVariant(const value: TValue): Variant;
    class function FromVariant(const value: Variant): TValue;

    class function TryConvert(const AFrom: TValue; targetTypeInfo: PTypeInfo; AEntity: TObject; var AResult: TValue): Boolean;

    class function TryGetLazyTypeValue(const ALazy: TValue; out AValue: TValue): Boolean;
    class function TryLoadFromStreamToPictureValue(AStream: TStream; out APictureValue: TValue): Boolean;
    class function TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean;
    class function TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;
  end;

implementation

uses
{$IF Defined(DELPHIXE2_UP) AND NOT Defined(NEXTGEN)}
  AnsiStrings,
{$IFEND}
{$IFNDEF FMX}
  GIFImg,
  Graphics,
  jpeg,
  pngimage,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  StrUtils,
  SysUtils,
  Variants,
  Spring,
  Spring.Reflection,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TUtils'}

class function TUtils.AsVariant(const value: TValue): Variant;
var
  stream: TStream;
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
      if value.TypeInfo = TypeInfo(TDateTime) then
        Result := value.AsType<TDateTime>
      else if value.TypeInfo = TypeInfo(TDate) then
        Result := value.AsType<TDate>
      else
        Result := value.AsExtended;
    end;
    tkRecord:
    begin
      if IsNullable(value.TypeInfo) then
        if value.TryGetNullableValue(LValue) then
          Exit(AsVariant(LValue));

      if TType.IsLazyType(value.TypeInfo) then
        if TryGetLazyTypeValue(value, LValue) then
          Exit(AsVariant(LValue));
    end;
    tkClass:
    begin
      if value.AsObject <> nil then
      begin
        if value.AsObject is TStream then
        begin
          stream := TStream(value.AsObject);
          stream.Position := 0;
          Result := LoadFromStreamToVariant(stream);
        end
        else if value.AsObject is TPicture then
        begin
          stream := TMemoryStream.Create;
          try
            TPicture(value.AsObject).Graphic.SaveToStream(stream);
            stream.Position := 0;
            Result := LoadFromStreamToVariant(stream);
          finally
            stream.Free;
          end;
        end   //somehow this started to fail recently. needed to add additional condition for TPicture
        else if Supports(value.AsObject, IStreamPersist, LPersist) then
        begin
          stream := TMemoryStream.Create;
          try
            LPersist.SaveToStream(stream);
            stream.Position := 0;
            Result := LoadFromStreamToVariant(stream);
          finally
            stream.Free;
          end;
        end;
      end;
    end;
    tkInterface: ;//
  else
    Result := value.AsVariant;
  end;
end;

class function TUtils.FromVariant(const value: Variant): TValue;
var
  stream: TMemoryStream;
  p: Pointer;
  i: Integer;
begin
  if VarIsArray(value) then
  begin
    // make stream from variant
    stream := TMemoryStream.Create;
    i := VarArrayDimCount(value);
    p := VarArrayLock(value);
    try
      stream.Write(p^, VarArrayHighBound(value, i) + 1);
    finally
      VarArrayUnlock(value);
    end;
    Result := stream;
  end
  else
    Result := TValue.FromVariant(value);
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
    Result := (lazy <> nil);
    if Result then
    begin
      AValue := lazy.Value;
    end;
  end;
end;

class function TUtils.LoadFromStreamToVariant(const stream: TStream): OleVariant;
var
  lock: Pointer;
begin
  Result := VarArrayCreate([0, stream.Size], varByte);
  lock := VarArrayLock(Result);
  try
    stream.ReadBuffer(lock^, stream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

{$IFNDEF FMX}

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

{$ENDIF}

class function TUtils.TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean;
var
  LGraphic: TGraphic;
  LGraphicClass: TGraphicClass;
  LStream: TMemoryStream;
begin
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
{$IFNDEF FMX}
    if not FindGraphicClass(LStream.Memory^, LStream.Size, LGraphicClass) then
      Exit(False);
{$ELSE}
    LGraphicClass := TBitmap;
{$ENDIF}
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

class function TUtils.TryConvert(const AFrom: TValue; targetTypeInfo: PTypeInfo; AEntity: TObject; var AResult: TValue): Boolean;
var
  freeAfterUse: Boolean;
begin
  if AFrom.TypeInfo <> targetTypeInfo then
  begin
    case targetTypeInfo.Kind of
      tkRecord:
      begin
        if IsNullable(targetTypeInfo) then
        begin
          TValue.Make(nil, targetTypeInfo, AResult);
          AResult.SetNullableValue(AFrom);
          Exit(True);
        end else
        if TType.IsLazyType(targetTypeInfo) then
        begin
          //AFrom value must be ID of lazy type
          if AFrom.IsEmpty then
            raise EORMColumnCannotBeNull.Create('Column for lazy type cannot be null');
          Exit(True);
        end;
      end;
    end;
    freeAfterUse := False;
    Result := Spring.Persistence.Core.Reflection.TryConvert(AFrom, targetTypeInfo, AResult, freeAfterUse);
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
{$IFNDEF FMX}
    if not FindGraphicClass(LStream.Memory^, LStream.Size, LGraphicClass) then
      Exit(False);
      //raise EInvalidGraphic.Create(SInvalidImage);
{$ELSE}
    LGraphicClass := TBitmap;
{$ENDIF}
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

{$ENDREGION}


end.
