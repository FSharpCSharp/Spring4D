{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Persistence.Core.Graphics;

interface

uses
{$IFDEF MSWINDOWS}
  Graphics
{$ELSE}
  {$IFNDEF LINUX}
  Fmx.Graphics
  {$ELSE}
  Classes
  {$ENDIF}
{$ENDIF}
  ;

type

{$REGION 'Type overlays'}

{$IFDEF MSWINDOWS}
  TBitmap = Graphics.TBitmap;
  TPicture = Graphics.TPicture;
  TGraphic = Graphics.TGraphic;
  TGraphicClass = Graphics.TGraphicClass;
{$ELSE}
  {$IFNDEF LINUX}
  TBitmap = Fmx.Graphics.TBitmap;
  TPicture = TBitmap;
  TGraphic = TBitmap;
  TGraphicClass = class of TBitmap;
  {$ELSE}
  // hack to compile code on Linux - non functional yet
  TBitmap = class(TPersistent)
  private
    fHeight, fWidth: Integer;
  public
    procedure LoadFromFile(const fileName: string);

    property Height: Integer read fHeight write fHeight;
    property Width: Integer read fWidth write fWidth;
  end;
  TPicture = TBitmap;
  TGraphic = TBitmap;
  TGraphicClass = class of TBitmap;
  {$ENDIF}

{$ENDIF}

{$ENDREGION}


{$REGION 'Helpers'}

{$IFNDEF MSWINDOWS}
type
  TPictureHelper = class helper for TPicture
  private
    function GetGraphic: TBitmap; inline;
  public
    property Graphic: TBitmap read GetGraphic;
  end;
{$ENDIF}

{$ENDREGION}


implementation


{$REGION 'TPictureHelper'}

{$IFNDEF MSWINDOWS}
function TPictureHelper.GetGraphic: TBitmap;
begin
  Result := Self;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TBitmap'}

{$IFDEF LINUX}
procedure TBitmap.LoadFromFile(const fileName: string);
begin
end;
{$ENDIF}

{$ENDREGION}


end.
