{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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
unit Spring.Configuration;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Spring,
  Spring.Collections;

type
  { Name, Type, Value, Children }
  IProperty = interface
  (*
    function GetName: string;
    function GetDescription: string;
    function GetIsReadOnly: Boolean;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property AsBCD: TBcd read GetAsBCD write SetAsBCD;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: UnicodeString read GetAsWideString write SetAsWideString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    //*)
  end;

  IConfigurationNode = interface;

  IConfigurationNodes = IEnumerable<IConfigurationNode>;

  IConfigurationNode = interface
    ['{E37F5A2C-D792-4FA8-9DB7-A00FE0D7E76D}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
//      function GetLocation: string;
//      function GetNamespace: string;
//      function GetPrefix: string;
//      function GetValue: TValue;
//      function GetIsReadOnly: Boolean;
      function GetAttributes: IDictionary<string, string>;
      function GetChildren: IList<IConfigurationNode>;
    {$ENDREGION}
//    function SelectNode(const xpath: string): IConfigurationNode;
//    function SelectNodes(const xpath: string): IConfigurationNodes;
    function TryGetAttribute(const name: string; out value: string): Boolean;
    function FindNode(const nodeName: string): IConfigurationNode;
    function FindNodes(const nodeName: string): IConfigurationNodes;
//    property Namespace: string read GetNamespace;
//    property Location: string read GetLocation;
//    property Prefix: string read GetPrefix;
//    property Value: TValue read GetValue;
//    property IsReadOnly: Boolean read GetIsReadOnly;
    property Name: string read GetName;
    property Attributes: IDictionary<string, string> read GetAttributes;
    property Children: IList<IConfigurationNode> read GetChildren;
//    property HasAttribute: Boolean;
//    property HasChildren: Boolean;
  end;

  IConfigurable = interface
    ['{55485576-3916-4B24-B32E-9A97D229D8F3}']
    procedure Configure(const configuration: IConfigurationNode);
  end;

  IConfigurationStore = interface
    ['{32FC1D2D-9DBF-4A96-A787-1FCEFAC203AD}']

  end;

  EConfigurationException = class(Exception);

function TryConfigure(const target: IInterface; const node: IConfigurationNode): Boolean;

implementation

function TryConfigure(const target: IInterface; const node: IConfigurationNode): Boolean;
var
  config: IConfigurable;
begin
  Result := (node <> nil) and Supports(target, IConfigurable, config);
  if Result then
  begin
    config.Configure(node);
  end;
end;

end.

