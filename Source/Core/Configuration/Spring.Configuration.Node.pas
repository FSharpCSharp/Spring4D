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
    //fParent: IConfiguration;
    fChildren: IList<IConfiguration>;
    fProperties: IDictionary<string, TConfigurationProperty>;
  {$REGION 'Property Accessors'}
    function GetName: string;
    //function GetParent: IConfiguration;
    function GetChildren: IList<IConfiguration>;
    function GetProperties: IDictionary<string, TConfigurationProperty>;
    //function GetProperties: IList<TConfigurationProperty>;
  {$ENDREGION}
  protected
    procedure SetName(const value: string); virtual;
  public
    //constructor Create(const parent: IConfiguration);
    function TryGetProperty(const name: string; out value: TConfigurationProperty): Boolean;
    function GetProperty(const name: string): TConfigurationProperty;
    function TryGetChild(const name: string;
      out section: IConfiguration): Boolean;
    function GetChild(const name: string): IConfiguration;
    property Name: string read GetName;
    //property Parent: IConfiguration read GetParent;
    property Properties: IDictionary<string, TConfigurationProperty> read GetProperties;
    //property Properties: IList<TConfigurationProperty> read GetProperties;
    property Children: IList<IConfiguration> read GetChildren;
  end;

  EConfigurationException = class(Exception);

implementation

uses
  Spring.Configuration.ResourceStrings;

{$REGION 'TConfiguration'}

{constructor TConfiguration.Create(const parent: IConfiguration);
begin
  inherited Create;
  fParent := parent;
end;}

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

{function TConfiguration.GetParent: IConfiguration;
begin
  Result := fParent;
end;}

function TConfiguration.GetProperties: IDictionary<string, TConfigurationProperty>;
begin
  if fProperties = nil then
  begin
    fProperties := TCollections.CreateDictionary<string, TConfigurationProperty>;
  end;
  Result := fProperties;
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

