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
unit Spring.Configuration.Sources experimental;

interface

uses
  XMLIntf,
  IniFiles,
  Spring.Configuration;

type
  TXmlConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    fXml: IXmlDocument;
    function Root: IXmlNode;
  public
    constructor Create(const fileName: string);
    function GetConfiguration: IConfiguration;
    function TryGetConfiguration(out configuration: IConfiguration): Boolean;
  end;

  TIniConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    fIni: TIniFile;
  public
    constructor Create(const fileName: string);
    function GetConfiguration: IConfiguration;
    function TryGetConfiguration(out configuration: IConfiguration): Boolean;
  end;

implementation

uses
  IOUtils,
  XMLDoc,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Configuration.Node,
  Spring.Configuration.ResourceStrings;

{$REGION 'TXmlConfigurationSource'}

constructor TXmlConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
  fXml := LoadXMLDocument(TPath.GetFullPath(fileName));
end;

function TXmlConfigurationSource.Root: IXmlNode;
begin
  Result := fXml.DocumentElement;
end;

function TXmlConfigurationSource.TryGetConfiguration(out configuration: IConfiguration): Boolean;
  {$REGION 'TryGetNode recursion function'}
    function TryGetNode(const section: IXmlNode;
      out configuration: IConfiguration): Boolean;
    var
      i: Integer;
      node: IXmlNode;
      key: string;
      value: TValue;
      children: IConfiguration;
    begin
      Result := Assigned(section);
      if not Result then
        Exit;

      with section do
      begin
        configuration := TConfiguration.Create;
        configuration.Name := NodeName;

        for i := 0 to AttributeNodes.Count - 1 do
        begin
          key := AttributeNodes[i].NodeName;
          value := TValue.From<string>(AttributeNodes[i].Text);
          configuration.Attributes.Add(key, value);
        end;
      end;

      node := section.ChildNodes.First;
      while node <> nil do
      begin
        TryGetNode(node, children);
        configuration.Children.Add(children);
        node := node.NextSibling;
      end;
    end;
  {$ENDREGION}
begin
  try
    fXml.Active := True;
    Result := TryGetNode(Root, configuration);
  finally
    fXml.Active := False;
  end;
end;

function TXmlConfigurationSource.GetConfiguration: IConfiguration;
begin
  if not TryGetConfiguration(Result) then
    raise EConfigurationException.CreateResFmt(@SConfigurationNotFound, ['']);
end;

{$ENDREGION}


{$REGION 'TIniConfigurationSource'}

constructor TIniConfigurationSource.Create(const fileName: string);
begin
  inherited Create;
  fIni := TIniFile.Create(TPath.GetFullPath(fileName));
end;

function TIniConfigurationSource.GetConfiguration: IConfiguration;
begin
  if TryGetConfiguration(Result) then
    raise ENotImplementedException.CreateResFmt(@SConfigurationNotFound, ['']);
end;

function TIniConfigurationSource.TryGetConfiguration(out configuration: IConfiguration): Boolean;
begin
  Result := False;
end;

{$ENDREGION}

end.
