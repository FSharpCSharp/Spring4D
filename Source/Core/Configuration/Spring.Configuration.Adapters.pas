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
unit Spring.Configuration.Adapters experimental;

interface

uses
  XMLIntf,
  IniFiles,
  Spring.Configuration;

type
  TXmlConfigurationAdapter = class(TInterfacedObject, IConfiguration)
  private
    fXml: IXmlDocument;
    fRoot: IConfiguration;
    function GetRoot: IConfiguration;
  public
    constructor Create(const fileName: string);
    property Root: IConfiguration read GetRoot implements IConfiguration;
  end;

  TIniConfigurationAdapter = class(TInterfacedObject, IConfiguration)
  private
    fIni: TIniFile;
    fRoot: IConfiguration;
    function GetRoot: IConfiguration;
  public
    constructor Create(const fileName: string);
    property Root: IConfiguration read GetRoot implements IConfiguration;
  end;

implementation

uses
  IOUtils,
  XMLDoc,
  Spring.Configuration.Node;

{$REGION 'TXmlConfigurationAdapter'}

constructor TXmlConfigurationAdapter.Create(const fileName: string);
begin
  inherited Create;
  fXml := LoadXMLDocument(TPath.GetFullPath(fileName));
end;

function TXmlConfigurationAdapter.GetRoot: IConfiguration;
begin
  if fRoot = nil then
  begin
    fRoot := TXmlConfiguration.Create(fXml.DocumentElement);
  end;
  Result := fRoot;
end;

{$ENDREGION}


{$REGION 'TIniConfigurationAdapter'}

constructor TIniConfigurationAdapter.Create(const fileName: string);
begin
  inherited Create;
  fIni := TIniFile.Create(TPath.GetFullPath(fileName));
end;

function TIniConfigurationAdapter.GetRoot: IConfiguration;
begin

end;

{$ENDREGION}

end.
