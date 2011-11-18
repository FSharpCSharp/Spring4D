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
  Spring.Collections.Lists,
  Spring.Configuration,
  Spring.Configuration.ConfigurationProperty;

type
  IConfigurationsList = interface;

  ///	<summary>Base implementation of IConfiguration interface.</summary>
  TConfiguration = class(TInterfacedObject, IConfiguration)
  strict private
    fName: string;
    fParent: IConfiguration;
    fChildren: IConfigurationsList;
    fProperties: IDictionary<string, TConfigurationProperty>;
  {$REGION 'Property Accessors'}
    function GetName: string;
    function GetParent: IConfiguration;
    function GetChildrenList: IConfigurationsList;
    function GetChildren: IConfigurations;
    function GetProperties: IDictionary<string, TConfigurationProperty>;
    //function GetProperties: IList<TConfigurationProperty>;
  {$ENDREGION}
  protected
    procedure SetName(const value: string); virtual;
  public
    constructor Create(const parent: IConfiguration);
    function TryGetProperty(const name: string; out value: TConfigurationProperty): Boolean;
    function GetProperty(const name: string): TConfigurationProperty;
    function TryGetChild(const name: string;
      out section: IConfiguration): Boolean;
    function GetChild(const name: string): IConfiguration;
    property Name: string read GetName;
    property Parent: IConfiguration read GetParent;
    property Properties: IDictionary<string, TConfigurationProperty> read GetProperties;
    //property Properties: IList<TConfigurationProperty> read GetProperties;
    function AddChild: IConfiguration;
    property Children: IConfigurations read GetChildren;
  end;

  IConfigurationsList = interface(IConfigurations)
  ['{F2D9B523-18DA-49CA-A5B3-26A539E9022C}']
    procedure Add(const item: IConfiguration); overload;
  end;

  TConfigurations = class(TList<IConfiguration>, IList<IConfiguration>, IConfigurations, IConfigurationsList)
  public
    procedure Remove(const predicate: TPredicate<IConfiguration>); overload;
  end;

  EConfigurationException = class(Exception);

implementation

uses
  Spring.Configuration.ResourceStrings;

{$REGION 'TConfiguration'}

function TConfiguration.AddChild: IConfiguration;
begin
  Result := TConfiguration.Create(Self);
  GetChildrenList.Add(Result);
end;

constructor TConfiguration.Create(const parent: IConfiguration);
begin
  inherited Create;
  fParent := parent;
end;

function TConfiguration.TryGetProperty(const name: string;
  out value: TConfigurationProperty): Boolean;
begin
  Result := Properties.TryGetValue(name, value);
end;

function TConfiguration.GetProperty(const name: string): TConfigurationProperty;
begin
  if not TryGetProperty(name, Result) then
    raise EConfigurationException.CreateResFmt(@SConfigurationAttributeNotFound, [name]);
end;

function TConfiguration.TryGetChild(const name: string;
  out section: IConfiguration): Boolean;
var
  sections: IEnumerable<IConfiguration>;
begin
  sections := Children.Where(
    function(const configuration: IConfiguration): Boolean
    begin
      Result := SameText(configuration.Name, name);
    end
  );
  Result := not sections.IsEmpty;
  if Result then
    section := sections.First;
end;

function TConfiguration.GetChild(const name: string): IConfiguration;
begin
  if not TryGetChild(name, Result) then
    raise EConfigurationException.CreateResFmt(@SConfigurationChildrenNotFound, [name]);
end;

procedure TConfiguration.SetName(const value: string);
begin
  if value <> fName then
    fName := value;
end;

function TConfiguration.GetName: string;
begin
  Result := fName;
end;

function TConfiguration.GetParent: IConfiguration;
begin
  Result := fParent;
end;

function TConfiguration.GetProperties: IDictionary<string, TConfigurationProperty>;
var
  col: IConfigurationPropertiesCollection;
begin
  if fProperties = nil then
  begin
    col := TConfigurationPropertiesCollection.Create;
    if Assigned(Parent) then
      col.ParentNodeCollection := Parent.Properties;
    fProperties := col;
  end;
  Result := fProperties;
end;

function TConfiguration.GetChildren: IConfigurations;
begin
  Result := GetChildrenList;
end;

function TConfiguration.GetChildrenList: IConfigurationsList;
begin
  if fChildren = nil then
  begin
    fChildren := TConfigurations.Create;
  end;
  Result := fChildren;
end;

{$ENDREGION}

{ TConfigurations }

procedure TConfigurations.Remove(const predicate: TPredicate<IConfiguration>);
var
  item: IConfiguration;
  collection: IList<IConfiguration>;
begin
  collection := TCollections.CreateList<IConfiguration>;
  for item in Self do
    collection.Add(item);

  for item in collection do
    if predicate(item) then
      Self.Remove(item);
end;

end.

