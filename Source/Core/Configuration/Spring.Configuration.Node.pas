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

 unit Spring.Configuration.Node experimental;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  XMLIntf,
  Spring,
  Spring.Collections,
  Spring.Configuration;

type
  TConfigurationBase = class abstract(TInterfacedObject, IConfiguration)
  strict private
    fChildrens: IList<IConfiguration>;
    fAttributes: IDictionary<string, TValue>;
    function GetChildrens: IList<IConfiguration>;
    function GetAttributes: IDictionary<string, TValue>;
  protected
    function GetName: string; virtual; abstract;
    procedure DoSetChildrens(const value: IList<IConfiguration>); virtual; abstract;
    procedure DoSetAttributes(const value: IDictionary<string, TValue>); virtual; abstract;
  public
    function TryGetAttribute(const name: string; out value: TValue): Boolean;
    function GetConfiguratioinSection(const nodeName: string): IConfiguration;
    property Name: string read GetName;
    property Attributes: IDictionary<string, TValue> read GetAttributes;
    property Childrens: IList<IConfiguration> read GetChildrens;
  end;

  TXmlConfiguration = class(TConfigurationBase)
  strict private
    fNode: IXmlNode;
    function GetConfigurationNode(const xmlNode: IXmlNode): IConfiguration;
  protected
    function GetName: string; override;
    procedure DoSetChildrens(const value: IList<IConfiguration>); override;
    procedure DoSetAttributes(const value: IDictionary<string, TValue>); override;
  public
    constructor Create(const node: IXmlNode);
  end;

  EConfigurationException = class(Exception);

implementation

uses
  ActiveX,
  Variants,
  Generics.Collections;

{$REGION 'TConfigurationBase'}

function TConfigurationBase.TryGetAttribute(const name: string;
  out value: TValue): Boolean;
begin
  Result := Attributes.TryGetValue(name, value);
end;

function TConfigurationBase.GetConfiguratioinSection(
  const nodeName: string): IConfiguration;
begin
  Result := Childrens.FirstOrDefault(
    function(const node: IConfiguration): Boolean
    begin
      Result := SameText(node.Name, nodeName);
    end
  );
end;

function TConfigurationBase.GetAttributes: IDictionary<string, TValue>;
begin
  if fAttributes = nil then
  begin
    fAttributes := TCollections.CreateDictionary<string, TValue>;
    DoSetAttributes(fAttributes);
  end;
  Result := fAttributes;
end;

function TConfigurationBase.GetChildrens: IList<IConfiguration>;
begin
  if fChildrens = nil then
  begin
    fChildrens := TCollections.CreateList<IConfiguration>;
    DoSetChildrens(fChildrens);
  end;
  Result := fChildrens;
end;

{$ENDREGION}


{$REGION 'TXmlConfiguration'}

constructor TXmlConfiguration.Create(const node: IXmlNode);
begin
  inherited Create;
  fNode := node;
end;

procedure TXmlConfiguration.DoSetAttributes(
  const value: IDictionary<string, TValue>);
var
  i: Integer;
begin
  with fNode.AttributeNodes do
  for i := 0 to Count - 1 do
  begin
    value.Add(Nodes[i].NodeName, Nodes[i].Text);
  end;
end;

procedure TXmlConfiguration.DoSetChildrens(
  const value: IList<IConfiguration>);
var
  node: IConfiguration;
  i: Integer;
begin
  with fNode.ChildNodes do
    for i := 0 to Count - 1 do
    begin
      node := GetConfigurationNode(Nodes[i]);
      value.Add(node);
    end;
end;

function TXmlConfiguration.GetConfigurationNode(
  const xmlNode: IXmlNode): IConfiguration;
begin
  Result := TXmlConfiguration.Create(xmlNode);
end;

function TXmlConfiguration.GetName: string;
begin
  Result := fNode.NodeName;
end;

{$ENDREGION}

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.

