{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Extensions.Logging.Configuration;

interface

uses
  TypInfo,
  Spring,
  Spring.Collections;

type
  TLoggingConfiguration = class
  private
    fTypes: IDictionary<PTypeInfo, string>;
  public
    constructor Create;

    procedure RegisterLogger<T>(const name: string);

    function HasLogger(typeInfo: PTypeInfo): Boolean;
    function GetLogger(typeInfo: PTypeInfo): string;
  end;

implementation

{ TLoggingConfiguration }

constructor TLoggingConfiguration.Create;
begin
  inherited;
  fTypes := TCollections.CreateDictionary<PTypeInfo, string>;
end;

function TLoggingConfiguration.GetLogger(typeInfo: PTypeInfo): string;
begin
  Result := fTypes[typeInfo];
end;

function TLoggingConfiguration.HasLogger(typeInfo: PTypeInfo): Boolean;
begin
  Result := fTypes.ContainsKey(typeInfo);
end;

procedure TLoggingConfiguration.RegisterLogger<T>(const name: string);
begin
  Guard.CheckNotNull(name <> '', 'name');
  Guard.CheckTypeKind(TypeInfo(T), [tkClass, tkRecord], 'T');
  fTypes.Add(TypeInfo(T), name);
end;

end.
