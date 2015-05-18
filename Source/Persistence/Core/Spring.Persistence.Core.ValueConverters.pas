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

unit Spring.Persistence.Core.ValueConverters;

interface

uses
  Spring,
  Spring.Reflection.ValueConverters;

type
  TStreamToVariantConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  TPictureToVariantConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

implementation

uses
  Classes,
  Variants,
  Spring.Persistence.Core.Graphics;

procedure RegisterConverters;
begin
  TValueConverterFactory.RegisterConverter(TypeInfo(TStream), TypeInfo(Variant),
    TStreamToVariantConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TPicture), TypeInfo(Variant),
    TPictureToVariantConverter);
end;

{$REGION 'TStreamToVariantConverter'}

function TStreamToVariantConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  stream: TStream;
begin
  stream := TStream(value.AsObject);
  stream.Position := 0;
  Result := TValue.From<Variant>(LoadFromStreamToVariant(stream));
end;

{$ENDREGION}


{$REGION 'TPictureToVariantConverter'}

function TPictureToVariantConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  stream: TStream;
begin
  stream := TMemoryStream.Create;
  try
    TPicture(value.AsObject).Graphic.SaveToStream(stream);
    stream.Position := 0;
    Result := TValue.From<Variant>(LoadFromStreamToVariant(stream));
  finally
    stream.Free;
  end;
end;

{$ENDREGION}


initialization
  RegisterConverters;

end.
