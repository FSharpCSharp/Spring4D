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
unit Core.ConnectionFactory;

interface

uses
  Core.Interfaces
  ,Generics.Collections
  ;

type
  TConnectionFactory = class
  private
    class var FRegistered: TDictionary<TDBDriverType, TClass>;
  protected
    class function ConcreteCreate(AClass: TClass; AConcreteConnection: TObject): IDBConnection;
  public
    class constructor Create;
    class destructor Destroy;

    class function GetInstance(AKey: TDBDriverType; AConcreteConnection: TObject): IDBConnection;
    class procedure RegisterConnection<T: class>(AKey: TDBDriverType);
    class function IsRegistered(AKey: TDBDriverType): Boolean;

  end;


implementation

uses
  Core.Exceptions
  ,Rtti
  ,TypInfo
  ;

{ TConnectionFactory }

class constructor TConnectionFactory.Create;
begin
  FRegistered := TDictionary<TDBDriverType, TClass>.Create(100);
end;

class destructor TConnectionFactory.Destroy;
begin
  FRegistered.Free;
end;

class function TConnectionFactory.ConcreteCreate(AClass: TClass; AConcreteConnection: TObject): IDBConnection;
var
  LParams: TArray<TRttiParameter>;
  LMethod: TRttiMethod;
begin
  for LMethod in TRttiContext.Create.GetType(AClass).GetMethods() do
  begin
    if LMethod.IsConstructor then
    begin
      LParams := LMethod.GetParameters;
      if (Length(LParams) = 1) then
      begin
        Result := LMethod.Invoke(AClass, [AConcreteConnection]).AsType<IDBConnection>;
        Exit();
      end;
    end;
  end;
  Result := nil;
end;

class function TConnectionFactory.GetInstance(AKey: TDBDriverType;
  AConcreteConnection: TObject): IDBConnection;
var
  LClass: TClass;
begin
  if not IsRegistered(AKey) then
    raise EORMConnectionNotRegistered.Create('Connection not registered');

  LClass := FRegistered[AKey];

  Result := ConcreteCreate(LClass, AConcreteConnection);
  if not Assigned(Result) then
    raise EORMUnsupportedType.Create('Connection type not supported');
end;

class function TConnectionFactory.IsRegistered(AKey: TDBDriverType): Boolean;
begin
  Result := FRegistered.ContainsKey(AKey);
end;

class procedure TConnectionFactory.RegisterConnection<T>(AKey: TDBDriverType);
var
  LClass: TClass;
begin
  LClass := T;
  if IsRegistered(AKey) then
    raise EORMConnectionAlreadyRegistered.Create('Connection already registered');

  FRegistered.Add(AKey, LClass);
end;


end.
