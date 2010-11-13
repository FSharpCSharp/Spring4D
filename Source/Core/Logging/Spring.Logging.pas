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

unit Spring.Logging;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Logging.Core;

type
  ILogger = Spring.Logging.Core.ILogger;

  ELoggingException = Spring.Logging.Core.ELoggingException;

  /// <summary>
  /// TLoggerManager
  /// </summary>
  TLoggerManager = class sealed(TObject)
  strict private
    class var
      fInstance: TLoggerManager;
      fLoggerRepository: ILoggerRepository;
  private
    class constructor Create;
    class destructor Destroy;
    class function GetLoggerRepository: ILoggerRepository;
  public
    class procedure Configure(const fileName: string); overload;
    class procedure ResetConfiguration;
    class procedure Shutdown;
    class function FindLogger(const name: string): ILogger;
    class function GetLogger(const name: string): ILogger; overload;
    class function GetLogger(typeInfo: PTypeInfo): ILogger; overload;
    class property Instance: TLoggerManager read fInstance;
  end;

  (*
  TGlobalContext = class

  end;

  TThreadContext = class

  end;

  TLogicalThreadContext = class

  end;
  //*)

///	<summary>Returns the shared instance of the TLoggerManager.</summary>
function LoggerManager: TLoggerManager;

{$REGION 'Documentation'}
///	<summary>Returns the default logger instance.</summary>
///	<returns>The root logger.</returns>
///	<seealso cref="LoggerManager"></seealso>
///	<seealso cref="ILogger"></seealso>
{$ENDREGION}
function DefaultLogger: ILogger;

implementation

uses
  Rtti,
  Spring.Configuration,
  Spring.Configuration.Xml,
  Spring.Logging.Repositories,
  Spring.Logging.ResourceStrings;

{$REGION 'Routines'}

function LoggerManager: TLoggerManager;
begin
  Result := TLoggerManager.Instance;
end;

function DefaultLogger: ILogger;
begin
  Result := TLoggerManager.GetLoggerRepository.Root;
end;

{$ENDREGION}


{$REGION 'TLoggerManager'}

class constructor TLoggerManager.Create;
begin
  fInstance := TLoggerManager.Create;
  fLoggerRepository := TLoggerRepository.Create;
end;

class destructor TLoggerManager.Destroy;
begin
  fLoggerRepository := nil;
  FreeAndNil(fInstance);
end;

class procedure TLoggerManager.Configure(const fileName: string);
var
  configuration: IConfigurationNode;
begin
  configuration := TXmlConfiguration.Create(fileName);
  GetLoggerRepository.Configure(configuration);
end;

class function TLoggerManager.GetLogger(const name: string): ILogger;
begin
  Result := GetLoggerRepository.GetLogger(name);
end;

class function TLoggerManager.GetLogger(typeInfo: PTypeInfo): ILogger;
var
  context: TRttiContext;
  typeObj: TRttiType;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  context := TRttiContext.Create;
  try
    typeObj := context.GetType(typeInfo);
    if typeObj = nil then
    begin
      raise EArgumentException.CreateRes(@SNoTypeInformation);
    end;
    if typeObj.IsPublicType then
      Result := GetLogger(typeObj.QualifiedName)
    else
      Result := GetLogger(typeObj.Name);
  finally
    context.Free;
  end;
end;

class function TLoggerManager.FindLogger(const name: string): ILogger;
begin
  Result := GetLoggerRepository.FindLogger(name);
end;

class function TLoggerManager.GetLoggerRepository: ILoggerRepository;
begin
  Result := fLoggerRepository;
end;

class procedure TLoggerManager.ResetConfiguration;
begin

end;

class procedure TLoggerManager.Shutdown;
begin

end;

{$ENDREGION}

end.
