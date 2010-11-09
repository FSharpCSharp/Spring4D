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

unit Spring.Logging.Utils;

interface

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Logging,
  Spring.Logging.Core;

type
  TOnlyOnceErrorHandler = class(TInterfacedObject, IErrorHandler)
  strict private
    fPrefix: string;
    fIsFirstTime: Boolean;
    function GetIsEnabled: Boolean;
    property IsEnabled: Boolean read GetIsEnabled;
  public
    constructor Create; overload;
    constructor Create(const prefix: string); overload;
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure Error(const msg: string; e: Exception; errorCode: TErrorCode); overload;
  end;

  TReusableStringWriter = class(TStringWriter)
  public
    procedure Reset(maxCapacity, defaultSize: Integer);
  end;

  TInternalLogger = class sealed(TObject)
  strict private
    class var
      fDebugEnabled: Boolean;
      fQuietMode: Boolean;
    class constructor Create;
  public
    class procedure Debug(const msg: string);
    class procedure Warn(const msg: string);
    class procedure Error(const msg: string);
    class procedure ErrorFmt(const format: string; const args: array of const);
    class property InternalDebugging: Boolean read fDebugEnabled write fDebugEnabled;
    class property QuietMode: Boolean read fQuietMode write fQuietMode;
  end;

implementation


{$REGION 'TOnlyOnceErrorHandler'}

constructor TOnlyOnceErrorHandler.Create;
begin
  Create('');
end;

constructor TOnlyOnceErrorHandler.Create(const prefix: string);
begin
  inherited Create;
  fPrefix := prefix;
end;

procedure TOnlyOnceErrorHandler.Error(const msg: string);
begin

end;

procedure TOnlyOnceErrorHandler.Error(const msg: string; e: Exception);
begin

end;

procedure TOnlyOnceErrorHandler.Error(const msg: string; e: Exception;
  errorCode: TErrorCode);
begin

end;

function TOnlyOnceErrorHandler.GetIsEnabled: Boolean;
begin

end;

{$ENDREGION}


{$IFDEF SUPPORTS_REGION} {$REGION 'TInternalLogger'} {$ENDIF}

class constructor TInternalLogger.Create;
begin
  fDebugEnabled := True;
  { TODO: Read Option from Configuration }
end;

class procedure TInternalLogger.Debug(const msg: string);
begin

end;

class procedure TInternalLogger.Error(const msg: string);
begin

end;

class procedure TInternalLogger.ErrorFmt(const format: string;
  const args: array of const);
var
  msg: string;
begin
  msg := SysUtils.Format(format, args);
  //...
end;

class procedure TInternalLogger.Warn(const msg: string);
begin

end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$REGION 'TReusableStringWriter'}

type
  {$HINTS OFF}
    TStringWriterHack = class(TTextWriter)
    private
      fBuilder: TStringBuilder;
      fOwnsBuilder: Boolean;
    end;
  {$HINTS ON}

procedure TReusableStringWriter.Reset(maxCapacity, defaultSize: Integer);
var
  stringBuilder: TStringBuilder;
begin
  stringBuilder := TStringWriterHack(Self).fBuilder;
  stringBuilder.Length := 0;
  if stringBuilder.Capacity > maxCapacity then
  begin
    stringBuilder.Capacity := defaultSize;
  end;
end;

{$ENDREGION}

end.
