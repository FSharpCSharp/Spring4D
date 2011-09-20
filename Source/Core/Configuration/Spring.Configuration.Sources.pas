{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.spring4d.org                                           }
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

/// NOT READY
unit Spring.Configuration.Sources experimental;

interface

uses
  XMLIntf,
  IniFiles,
  Spring.Configuration;

type
  TXmlConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    fXml: IXMLDocument;
  public
    constructor Create(const fileName: string);
    function TryGetConfiguration(out configuration: IConfiguration): Boolean;
    function TrySetConfiguration(const configuration: IConfiguration): Boolean;
    function Merge(const configuration: IConfiguration): IConfiguration;
  end;

  TIniConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    fIni: TIniFile;
  public
    constructor Create(const fileName: string);
    function TryGetConfiguration(out configuration: IConfiguration): Boolean;
    function TrySetConfiguration(const configuration: IConfiguration): Boolean;
    function Merge(const configuration: IConfiguration): IConfiguration;
  end;

implementation

uses
  ActiveX,
  IOUtils,
  XMLDoc;

{$REGION 'TXmlConfigurationSource'}

constructor TXmlConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
  fXml := LoadXMLDocument(TPath.GetFullPath(fileName));
end;

function TXmlConfigurationSource.Merge(
  const configuration: IConfiguration): IConfiguration;
begin

end;

function TXmlConfigurationSource.TryGetConfiguration(
  out configuration: IConfiguration): Boolean;
begin

end;

function TXmlConfigurationSource.TrySetConfiguration(
  const configuration: IConfiguration): Boolean;
begin

end;

{$ENDREGION}


{$REGION 'TIniConfigurationSource'}

constructor TIniConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
  fIni := TIniFile.Create(TPath.GetFullPath(fileName));
end;

function TIniConfigurationSource.Merge(
  const configuration: IConfiguration): IConfiguration;
begin

end;

function TIniConfigurationSource.TryGetConfiguration(
  out configuration: IConfiguration): Boolean;
begin

end;

function TIniConfigurationSource.TrySetConfiguration(
  const configuration: IConfiguration): Boolean;
begin

end;

{$ENDREGION}

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
