{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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
{                                                                           }
{                        KEEP IT SIMPLE & FAST!!!                           }
{                                                                           }
{***************************************************************************}

unit Spring.Logging.Layouts;

interface

uses
  Classes,
  SysUtils,
  Spring.System,
  Spring.ResourceStrings,
  Spring.Logging.Core;

type
  TLayoutBase = class(TInterfacedObject, IOptionHandler, ILayout)
  protected
    function GetContentType: string; virtual;
    function GetHeader: string; virtual;
    function GetFooter: string; virtual;
    function GetIgnoresException: Boolean; virtual;
  public
    { IOptionHandler }
    procedure ActivateOptions; virtual; abstract;
    { ILayout }
    procedure Format(writer: TTextWriter; const event: TLoggingEvent); virtual; abstract;
    property ContentType: string read GetContentType;
    property Header: string read GetHeader;
    property Footer: string read GetFooter;
    property IgnoresException: Boolean read GetIgnoresException;
  end;

  TSimpleLayout = class(TLayoutBase)
  public
    procedure ActivateOptions; override;
    procedure Format(writer: TTextWriter; const event: TLoggingEvent); override;
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


{$REGION 'TSimpleLayout'}

procedure TSimpleLayout.ActivateOptions;
begin
end;

procedure TSimpleLayout.Format(writer: TTextWriter; const event: TLoggingEvent);
begin
  TArgument.CheckNotNull(event, 'event');
  writer.Write(event.Level.Name);
  writer.Write(' - ');
  event.WriteRenderedMessage(writer);
  writer.WriteLine;
end;

{$ENDREGION}

end.
