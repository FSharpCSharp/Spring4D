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
unit SQL.Commands.Factory;

interface

uses
  Generics.Collections, SQL.Commands, Rtti, SQL.AbstractCommandExecutor, Core.Interfaces;

type
  TCommandKey = record
    EntityClass: TClass;
    TypeInfo: Pointer;
  end;

  TCommandFactory = class
  private
    FCommands: TDictionary<TCommandKey,TValue>;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear();

    function GetCommand<T: TAbstractCommandExecutor, constructor>(AClass: TClass; AConnection: IDBConnection): T;


  end;

var
  CommandFactory: TCommandFactory = nil;

implementation

uses
  Core.Exceptions;

{ TCommandFactory }

procedure TCommandFactory.Clear;
var
  LCommand: TValue;
  LObj: TObject;
begin
  for LCommand in FCommands.Values do
  begin
    LObj := LCommand.AsObject;
    if Assigned(LObj) then
    begin
      LObj.Free;
    end;
  end;
  FCommands.Clear;
end;

constructor TCommandFactory.Create;
begin
  inherited Create;
  FCommands := TDictionary<TCommandKey, TValue>.Create();
end;

destructor TCommandFactory.Destroy;
begin
  Clear;
  FCommands.Free;
  inherited Destroy;
end;

function TCommandFactory.GetCommand<T>(AClass: TClass; AConnection: IDBConnection): T;
var
  LCommand: TValue;
  LKey: TCommandKey;
begin
  LKey.EntityClass := AClass;
  LKey.TypeInfo := TypeInfo(T);

  if not FCommands.TryGetValue(LKey, LCommand) then
  begin
    Result := T.Create;
    Result.Connection := AConnection;
    Result.Build(AClass);
    FCommands.Add(LKey, Result);
  end
  else
  begin
    Result := LCommand.AsType<T>();
    Result.Connection := AConnection;
  end;
end;

initialization
  CommandFactory := TCommandFactory.Create;

finalization
  CommandFactory.Free;

end.
