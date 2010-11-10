{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Logging.Layouts;

interface

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Logging.Core;

type
  TLayoutBase = class abstract(TInterfacedObject, IOptionHandler, ILayout)
  protected
    function GetContentType: string; virtual;
    function GetHeader: string; virtual;
    function GetFooter: string; virtual;
    function GetIgnoresException: Boolean; virtual;
  public
    { IOptionHandler }
    procedure ActivateOptions; virtual; abstract;
    { ILayout }
    function Format(const event: TLoggingEvent): string; virtual; abstract;
    property ContentType: string read GetContentType;
    property Header: string read GetHeader;
    property Footer: string read GetFooter;
    property IgnoresException: Boolean read GetIgnoresException;
  end;

  // conversionPattern
  TPatternLayout = class(TLayoutBase)
  public
  end;

implementation


{$REGION 'TLayoutBase'}

function TLayoutBase.GetContentType: string;
begin
  Result := 'text/plain';
end;

function TLayoutBase.GetFooter: string;
begin
  Result := '';
end;

function TLayoutBase.GetHeader: string;
begin
  Result := '';
end;

function TLayoutBase.GetIgnoresException: Boolean;
begin
  Result := True;
end;

{$ENDREGION}

end.
