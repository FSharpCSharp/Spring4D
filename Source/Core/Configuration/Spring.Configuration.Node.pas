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

 unit Spring.Configuration.Node experimental;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Configuration;

type
  TConfigurationNode = class(TInterfacedObject, IConfigurationNode)
  private
    fName: string;
    fChildren: IList<IConfigurationNode>;
    fAttributes: IDictionary<string, string>;
    function GetName: string;
    function GetAttributes: IDictionary<string, string>;
    function GetChildren: IList<IConfigurationNode>;
  public
    function TryGetAttribute(const name: string; out value: string): Boolean;
    function FindNode(const nodeName: string): IConfigurationNode;
    function FindNodes(const nodeName: string): IConfigurationNodes;
    property Name: string read GetName;
    property Attributes: IDictionary<string, string> read GetAttributes;
    property Children: IList<IConfigurationNode> read GetChildren;
  end;

implementation

uses
  IOUtils,
  Variants,
  Generics.Collections;

{$REGION 'TConfigurationNode'}

function TConfigurationNode.TryGetAttribute(const name: string;
  out value: string): Boolean;
begin
  Result := Attributes.TryGetValue(name, value);
end;

function TConfigurationNode.FindNode(const nodeName: string): IConfigurationNode;
begin
  if Children.IsEmpty then
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

function TConfigurationNode.FindNodes(const nodeName: string): IConfigurationNodes;
begin
  Result := Children.Where(
    function(const node: IConfigurationNode): Boolean
    begin
      Result := SameText(node.Name, nodeName);
    end
  );
end;

function TConfigurationNode.GetAttributes: IDictionary<string, string>;
var
  i: Integer;
begin
  if fAttributes = nil then
  begin
    fAttributes := TCollections.CreateDictionary<string, string>;
  end;
  Result := fAttributes;
end;

function TConfigurationNode.GetChildren: IList<IConfigurationNode>;
begin
  if fChildren = nil then
  begin
    fChildren := TCollections.CreateList<IConfigurationNode>;
  end;
  Result := fChildren;
end;

function TConfigurationNode.GetName: string;
begin
  Result := fName;
end;

{$ENDREGION}

end.

