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
  Spring.Configuration.ConfigurationProperty;

type
  ///	<summary>Base implementation of IConfiguration interface.</summary>
  TConfiguration = class(TInterfacedObject, IConfiguration)
  strict private
    fName: string;
    fChildren: IList<IConfiguration>;
    fAttributes: IDictionary<string, TConfigurationProperty>;
    function GetName: string;
    function GetChildren: IList<IConfiguration>;
    function GetAttributes: IDictionary<string, TConfigurationProperty>;
  protected
    procedure SetName(const value: string); virtual;
  public
    function TryGetAttribute(const name: string; out value: TConfigurationProperty): Boolean;
    function GetAttribute(const name: string): TConfigurationProperty;
    function TryGetSection(const name: string;
      out section: IConfiguration): Boolean;
    function GetSection(const name: string): IConfiguration;
    property Name: string read GetName;
    property Attributes: IDictionary<string, TConfigurationProperty> read GetAttributes;
    property Children: IList<IConfiguration> read GetChildren;
  end;

  EConfigurationException = class(Exception);

implementation

uses
  Spring.Configuration.ResourceStrings;

{$REGION 'TConfiguration'}

function TConfiguration.TryGetAttribute(const name: string;
  out value: TConfigurationProperty): Boolean;
begin
  Result := Attributes.TryGetValue(name, value);
end;

function TConfiguration.GetAttribute(const name: string): TConfigurationProperty;
begin
  if not TryGetAttribute(name, Result) then
    raise EConfigurationException.CreateResFmt(@SConfigurationAttributeNotFound, [name]);
end;

function TConfiguration.TryGetSection(const name: string;
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

function TConfiguration.GetSection(const name: string): IConfiguration;
begin
  if not TryGetSection(name, Result) then
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

function TConfiguration.GetAttributes: IDictionary<string, TConfigurationProperty>;
begin
  if fAttributes = nil then
  begin
    fAttributes := TCollections.CreateDictionary<string, TConfigurationProperty>;
  end;
  Result := fAttributes;
end;

function TConfiguration.GetChildren: IList<IConfiguration>;
begin
  if fChildren = nil then
  begin
    fChildren := TCollections.CreateList<IConfiguration>;
  end;
  Result := fChildren;
end;

{$ENDREGION}

end.

