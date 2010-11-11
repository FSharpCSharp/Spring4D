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

unit Spring.Configuration.Xml;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  XmlIntf,
  XmlDoc,
  Spring,
  Spring.Collections,
  Spring.Configuration;

type
  TXmlNodeAdapter = class(TInterfacedObject, IConfigurationNode)
  private
    fNode: IXmlNode;
    fChildren: IList<IConfigurationNode>;
    fAttributes: IDictionary<string, string>;
    function GetName: string;
    function GetAttributes: IDictionary<string, string>;
    function GetChildren: IList<IConfigurationNode>;
  protected
    function GetConfigurationNode(const xmlNode: IXMLNode): IConfigurationNode;
  public
    constructor Create(const node: IXmlNode);
    function TryGetAttribute(const name: string; out value: string): Boolean;
    function FindNode(const nodeName: string): IConfigurationNode;
    function FindNodes(const nodeName: string): IConfigurationNodes;
    property Name: string read GetName;
    property Attributes: IDictionary<string, string> read GetAttributes;
    property Children: IList<IConfigurationNode> read GetChildren;
  end;

  TXmlConfiguration = class(TInterfacedObject, IConfigurationNode)
  private
    fXml: IXMLDocument;
    fRoot: IConfigurationNode;
    function GetRoot: IConfigurationNode;
  public
    constructor Create(const fileName: string);
    property Root: IConfigurationNode read GetRoot implements IConfigurationNode;
  end;

implementation

uses
  ActiveX,
  IOUtils,
  Variants;

{$REGION 'TXmlNodeAdapter'}

constructor TXmlNodeAdapter.Create(const node: IXmlNode);
begin
  inherited Create;
  fNode := node;
end;

function TXmlNodeAdapter.TryGetAttribute(const name: string;
  out value: string): Boolean;
begin
  Result := fNode.HasAttribute(name);
  if Result then
  begin
    value := VarToStrDef(fNode.Attributes[name], '');
  end;
end;

function TXmlNodeAdapter.FindNode(const nodeName: string): IConfigurationNode;
begin
  if not fNode.HasChildNodes then
  begin
    Exit(nil);
  end;
  Result := Children.FirstOrDefault(
    function(const node: IConfigurationNode): Boolean
    begin
      Result := SameText(node.Name, nodeName);
    end
  );
end;

function TXmlNodeAdapter.FindNodes(const nodeName: string): IConfigurationNodes;
begin
  Result := Children.Where(
    function(const node: IConfigurationNode): Boolean
    begin
      Result := SameText(node.Name, nodeName);
    end
  );
end;

function TXmlNodeAdapter.GetAttributes: IDictionary<string, string>;
var
  i: Integer;
begin
  if fAttributes = nil then
  begin
    fAttributes := TCollections.CreateDictionary<string, string>;
    with fNode.AttributeNodes do
    for i := 0 to Count - 1 do
    begin
      fAttributes.Add(Nodes[i].NodeName, Nodes[i].Text);
    end;
  end;
  Result := fAttributes;
end;

function TXmlNodeAdapter.GetChildren: IList<IConfigurationNode>;
var
  node: IConfigurationNode;
  i: Integer;
begin
  if fChildren = nil then
  begin
    fChildren := TCollections.CreateList<IConfigurationNode>;
    with fNode.ChildNodes do
    for i := 0 to Count - 1 do
    begin
      node := GetConfigurationNode(Nodes[i]);
      fChildren.Add(node);
    end;
  end;
  Result := fChildren;
end;

function TXmlNodeAdapter.GetConfigurationNode(
  const xmlNode: IXMLNode): IConfigurationNode;
begin
  Result := TXmlNodeAdapter.Create(xmlNode);
end;

function TXmlNodeAdapter.GetName: string;
begin
  Result := fNode.NodeName;
end;

{$ENDREGION}


{$REGION 'TXmlConfiguration'}

constructor TXmlConfiguration.Create(const fileName: string);
begin
  inherited Create;
  fXml := LoadXMLDocument(TPath.GetFullPath(fileName));
end;

function TXmlConfiguration.GetRoot: IConfigurationNode;
begin
  if fRoot = nil then
  begin
    fRoot := TXmlNodeAdapter.Create(fXml.DocumentElement);
  end;
  Result := fRoot;
end;

{$ENDREGION}

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
