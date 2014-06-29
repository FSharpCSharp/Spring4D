(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.Commands.Page;

interface

uses
  SQL.AbstractCommandExecutor, SQL.Types, SQL.Commands, SQL.Params, Generics.Collections
  , Mapping.Attributes, Core.Interfaces;

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
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;
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
  Math
  ;

{ TPageExecutor }

function TPageExecutor.BuildSQL(const ASql: string): string;
begin
  Result := Generator.GeneratePagedQuery(ASql, Limit, Offset);
end;

procedure TPageExecutor.Build(AClass: TClass);
begin
  //do nothing
end;

procedure TPageExecutor.BuildParams(AEntity: TObject);
begin
  inherited;

end;

constructor TPageExecutor.Create();
begin
  inherited Create();
end;

destructor TPageExecutor.Destroy;
begin
  inherited Destroy;
end;

procedure TPageExecutor.Execute(AEntity: TObject);
begin
  inherited Execute(AEntity);
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
  if (Page <= 1) then
    Result := 0
  else
    Result := (Page * ItemsPerPage) - ItemsPerPage;
end;

end.
