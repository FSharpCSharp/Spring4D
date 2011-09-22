{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
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

/// NOT READY
unit Spring.Configuration experimental;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Spring,
  Spring.Collections;

type
  IConfiguration = interface;
  IConfigurationSource = interface;
  IConfigurations = IEnumerable<IConfiguration>;

  IConfiguration = interface
    ['{E37F5A2C-D792-4FA8-9DB7-A00FE0D7E76D}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      function GetAttributes: IDictionary<string, TValue>;
      function GetChildren: IList<IConfiguration>;
    {$ENDREGION}
    function TryGetAttribute(const name: string; out value: TValue): Boolean;
    function GetConfiguratioinSection(const nodeName: string): IConfiguration;
    property Name: string read GetName;
    property Attributes: IDictionary<string, TValue> read GetAttributes;
    property Children: IList<IConfiguration> read GetChildren;
  end;

  IConfigurationSource = interface
    ['{1443C8DF-D07D-4DAC-9509-FB79F375C557}']
    function GetConfiguration(const sectionName: string): IConfiguration;
    function TryGetConfiguration(const sectionName: string;
      out configuration: IConfiguration): Boolean;
  end;

  IConfigurable = interface
    ['{FCB399EF-F27C-44E1-BB9C-1723BB1D4830}']
    procedure Configure(const configuration: IConfiguration);
    function TryConfigure(const configuration: IConfiguration): Boolean;
  end;

implementation

end.
