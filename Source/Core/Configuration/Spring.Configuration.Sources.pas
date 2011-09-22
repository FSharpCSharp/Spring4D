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
    fXml: IXmlDocument;
  public
    constructor Create(const fileName: string);
    function GetConfiguration(const sectionName: string): IConfiguration;
    function TryGetConfiguration(const sectionName: string;
      out configuration: IConfiguration): Boolean;
  end;

  TIniConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    fIni: TIniFile;
  public
    constructor Create(const fileName: string);
    function GetConfiguration(const sectionName: string): IConfiguration;
    function TryGetConfiguration(const sectionName: string;
      out configuration: IConfiguration): Boolean;
  end;

implementation

uses
  IOUtils,
  XMLDoc,
  Spring.Configuration.Node;

{$REGION 'TXmlConfigurationSource'}

constructor TXmlConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
end;

function TXmlConfigurationSource.GetConfiguration(
  const sectionName: string): IConfiguration;
begin

end;

function TXmlConfigurationSource.TryGetConfiguration(const sectionName: string;
  out configuration: IConfiguration): Boolean;
begin

end;

{$ENDREGION}


{$REGION 'TIniConfigurationSource'}

constructor TIniConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
  fIni := TIniFile.Create(TPath.GetFullPath(fileName));
end;

function TIniConfigurationSource.GetConfiguration(
  const sectionName: string): IConfiguration;
begin

end;

function TIniConfigurationSource.TryGetConfiguration(const sectionName: string;
  out configuration: IConfiguration): Boolean;
begin

end;

{$ENDREGION}

end.
