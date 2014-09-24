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

unit Spring.Persistence.SQL.Commands.Page;

interface

uses
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing paged statements.
  ///	</summary>
  {$ENDREGION}
  TPageExecutor = class(TAbstractCommandExecutor)
  private
    FPage: Integer;
    FItemsPerPage: Integer;
    FTotalItems: Int64;
    function GetLimit: Integer;
    function GetOffset: Integer;
  protected
    function GetCommand: TDMLCommand; override;
  public
    procedure Build(AClass: TClass); override;
    function BuildSQL(const ASql: string): string;
    procedure Execute(AEntity: TObject); override;

    property Page: Integer read FPage write FPage;
    property ItemsPerPage: Integer read FItemsPerPage write FItemsPerPage;
    property TotalItems: Int64 read FTotalItems write FTotalItems;
    property Limit: Integer read GetLimit;
    property Offset: Integer read GetOffset;
  end;

implementation

uses
  Math;

{ TPageExecutor }

function TPageExecutor.BuildSQL(const ASql: string): string;
begin
  Result := Generator.GeneratePagedQuery(ASql, Limit, Offset);
end;

procedure TPageExecutor.Build(AClass: TClass);
begin
  //do nothing
end;

procedure TPageExecutor.Execute(AEntity: TObject);
begin
  //do nothing
end;

function TPageExecutor.GetCommand: TDMLCommand;
begin
  Result := nil;
end;

function TPageExecutor.GetLimit: Integer;
begin
  Result := ItemsPerPage;
end;

function TPageExecutor.GetOffset: Integer;
begin
  if Page <= 1 then
    Result := 0
  else
    Result := (Page * ItemsPerPage) - ItemsPerPage;
end;

end.
