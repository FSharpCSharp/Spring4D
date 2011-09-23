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
  Spring,
  Spring.Collections,
  Spring.Configuration,
  Spring.Configuration.PropertyValue;

type
  TConfiguration = class abstract(TInterfacedObject, IConfiguration)
  strict private
    fChildren: IList<IConfiguration>;
    fProperties: IDictionary<string, TPropertyValue>;
    function GetChildren: IList<IConfiguration>;
    function GetProperties(Key: string): TPropertyValue;
    function GetPropertiesCollection: IDictionary<string, TPropertyValue>;
    procedure SetProperties(Key: string; const Value: TPropertyValue);
  protected
    function GetName: string; virtual; abstract;
    procedure DoSetChildren(const value: IList<IConfiguration>); virtual; abstract;
    procedure DoSetAttributes(const value: IDictionary<string, TPropertyValue>); virtual; abstract;
    property PropertiesCollection: IDictionary<string, TPropertyValue> read GetPropertiesCollection;
  public
    function TryGetAttribute(const name: string; out value: TPropertyValue): Boolean;
    function GetConfiguratioinSection(const nodeName: string): IConfiguration;
    property Name: string read GetName;
    property Properties[Key: string]: TPropertyValue read GetProperties write SetProperties;
    property Children: IList<IConfiguration> read GetChildren;
  end;

  EConfigurationException = class(Exception);

implementation

uses
  Generics.Collections;

{$REGION 'TConfigurationBase'}

function TConfiguration.TryGetAttribute(const name: string;
  out value: TPropertyValue): Boolean;
begin
  Result := PropertiesCollection.TryGetValue(name, value);
end;

function TConfiguration.GetConfiguratioinSection(
  const nodeName: string): IConfiguration;
begin
  Result := Children.FirstOrDefault(
    function(const node: IConfiguration): Boolean
    begin
      Result := SameText(node.Name, nodeName);
    end
  );
end;

function TConfiguration.GetProperties(Key: string): TPropertyValue;
begin
   Result := PropertiesCollection.Items[Key];
end;

function TConfiguration.GetPropertiesCollection: IDictionary<string, TPropertyValue>;
begin
  if fProperties = nil then
  begin
    fProperties := TCollections.CreateDictionary<string, TPropertyValue>;
    DoSetAttributes(fProperties);
  end;

  Result := fProperties;
end;

procedure TConfiguration.SetProperties(Key: string;
  const Value: TPropertyValue);
begin
  PropertiesCollection.Items[Key] := Value;
end;

function TConfiguration.GetChildren: IList<IConfiguration>;
begin
  if fChildren = nil then
  begin
    fChildren := TCollections.CreateList<IConfiguration>;
    DoSetChildren(fChildren);
  end;
  Result := fChildren;
end;

{$ENDREGION}

end.

