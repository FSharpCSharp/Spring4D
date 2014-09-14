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
unit Spring.Persistence.SQL.Commands.Factory;

interface

uses
  Spring.Persistence.SQL.Commands, Rtti, Spring.Persistence.SQL.AbstractCommandExecutor
  , Spring.Persistence.Core.Interfaces;

type
{
  TCommandKey = record
    EntityClass: TClass;
    TypeInfo: Pointer;
  end;
 }

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents factory which creates custom statement executors.
  ///	</summary>
  {$ENDREGION}
  TCommandFactory = class
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function GetCommand<T: TAbstractCommandExecutor, constructor>(AClass: TClass; const AConnection: IDBConnection): T;
  end;

var
  CommandFactory: TCommandFactory = nil;

implementation

uses
  Spring.Persistence.Core.Exceptions;

{ TCommandFactory }


constructor TCommandFactory.Create;
begin
  inherited Create;
end;

destructor TCommandFactory.Destroy;
begin
  inherited Destroy;
end;

function TCommandFactory.GetCommand<T>(AClass: TClass; const AConnection: IDBConnection): T;
begin
  Result := T.Create;
  Result.Connection := AConnection;
  Result.EntityClass := AClass;
  Result.Build(AClass);
end;

initialization
  CommandFactory := TCommandFactory.Create;

finalization
  CommandFactory.Free;

end.
