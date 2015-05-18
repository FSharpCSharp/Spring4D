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
  Spring.Persistence.Core.Graphics;

type
  TUtils = class
  public
    class function TryConvert(const AFrom: TValue; targetTypeInfo: PTypeInfo; AEntity: TObject; var AResult: TValue): Boolean; static;

    class function TryLoadFromStreamToPictureValue(AStream: TStream; out APictureValue: TValue): Boolean; static;
    class function TryLoadFromBlobField(AField: TField; AToPicture: TPicture): Boolean; static;
    class function TryLoadFromStreamSmart(AStream: TStream; AToPicture: TPicture): Boolean; static;
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
  Spring.Persistence.Core.Reflection;


{$REGION 'TUtils'}

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
    APictureValue := LPic
  else
    LPic.Free;
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
        if IsLazyType(targetTypeInfo) then
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
