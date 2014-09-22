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

unit Spring.Persistence.SQL.Register;

{$I Spring.inc}

interface

uses
  Spring.Collections,
  Spring.Persistence.SQL.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent factory for <c>ISQLGenerators.</c> Each custom SQL generator
  ///	  must register itself with this factory class.
  ///	</summary>
  {$ENDREGION}
  TSQLGeneratorRegister = class
  strict private
    class var FGenerators: IDictionary<TQueryLanguage,ISQLGenerator>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterGenerator(const AGenerator: ISQLGenerator);
    class function GetGenerator(const AQueryLanguage: TQueryLanguage): ISQLGenerator;
  end;

implementation

uses
  Spring.Persistence.SQL.Generators.Ansi;

{ TSQLGeneratorRegister }

class constructor TSQLGeneratorRegister.Create;
begin
  FGenerators := TCollections.CreateDictionary<TQueryLanguage,ISQLGenerator>;
  RegisterGenerator(TAnsiSQLGenerator.Create as ISQLGenerator);
end;

class destructor TSQLGeneratorRegister.Destroy;
begin
  FGenerators := nil;
end;

class function TSQLGeneratorRegister.GetGenerator(const AQueryLanguage: TQueryLanguage): ISQLGenerator;
begin
  Result := FGenerators[AQueryLanguage];
end;

class procedure TSQLGeneratorRegister.RegisterGenerator(const AGenerator: ISQLGenerator);
begin
  FGenerators.AddOrSetValue(AGenerator.GetQueryLanguage, AGenerator);
end;


end.
