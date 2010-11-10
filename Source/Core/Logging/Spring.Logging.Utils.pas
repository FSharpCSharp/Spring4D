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

end.
