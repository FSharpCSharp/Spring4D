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
  XMLIntf,
  Spring,
  Spring.Collections,
  Spring.Logging.Core;

type
  ILogger = Spring.Logging.Core.ILogger;

  ELoggingException = Spring.Logging.Core.ELoggingException;

  /// <summary>
  /// TLoggingManager
  /// </summary>
  TLoggingManager = class sealed(TObject)
  strict private
    class var
      fInstance: TLoggingManager;
      fLoggerRepository: ILoggerRepository;
  private
    class constructor Create;
    class destructor Destroy;
    class function GetLoggerRepository: ILoggerRepository;
  public
    procedure Configure(const fileName: string); overload;
    procedure Configure(const node: IXmlNode); overload;
//    class function Initialize(const repository: ILoggerRepository);
    class function FindLogger(const name: string): ILogger;
    class procedure ResetConfiguration;
    class procedure Shutdown;
    class function GetLogger(const name: string): ILogger; overload;
    class function GetLogger(typeInfo: PTypeInfo): ILogger; overload;
    class property Instance: TLoggingManager read fInstance;
  end;

  (*
  TGlobalContext = class

  end;

  TThreadContext = class

  end;

  TLogicalThreadContext = class

  end;
  //*)

function LoggingManager: TLoggingManager;

implementation

uses
  Rtti,
  XMlDoc,
  Spring.Logging.Appenders,
  Spring.Logging.Layouts,
  Spring.Logging.Repositories,
  Spring.Logging.ResourceStrings;

{$REGION 'Routines'}

function LoggingManager: TLoggingManager;
begin
  Result := TLoggingManager.Instance;
end;

{$ENDREGION}


{$REGION 'TLoggingManager'}

class constructor TLoggingManager.Create;
begin
  fInstance := TLoggingManager.Create;
  fLoggerRepository := TLoggerRepository.Create;
end;

class destructor TLoggingManager.Destroy;
begin
  fLoggerRepository := nil;
  FreeAndNil(fInstance);
end;

procedure TLoggingManager.Configure(const fileName: string);
//var
//  xml: IXMLDocument;
//  xmlNode: IXMLNode;
begin
//  xml := LoadXMLDocument(fileName);
//  xmlNode := xml.DocumentElement;
end;

procedure TLoggingManager.Configure(const node: IXmlNode);
begin

end;

class function TLoggingManager.GetLogger(const name: string): ILogger;
begin
  Result := GetLoggerRepository.GetLogger(name);
end;

class function TLoggingManager.GetLogger(typeInfo: PTypeInfo): ILogger;
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

class function TLoggingManager.FindLogger(const name: string): ILogger;
begin
  Result := nil;
end;

class function TLoggingManager.GetLoggerRepository: ILoggerRepository;
begin
  Result := fLoggerRepository;
end;

class procedure TLoggingManager.ResetConfiguration;
begin

end;

class procedure TLoggingManager.Shutdown;
begin

end;

{$ENDREGION}

end.
