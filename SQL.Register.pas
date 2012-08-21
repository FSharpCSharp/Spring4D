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
unit SQL.Register;

interface

uses
  SQL.Interfaces, SysUtils, Generics.Collections;

type
  TSQLGeneratorRegister = class
  strict private
    class var FGenerators: TDictionary<TQueryLanguage,ISQLGenerator>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterGenerator(const AGenerator: ISQLGenerator);
    class function GetGenerator(const AQueryLanguage: TQueryLanguage): ISQLGenerator;
  end;

implementation

uses
  SQL.Generator.Ansi;

{ TSQLGeneratorRegister }

class constructor TSQLGeneratorRegister.Create;
var
  LGenerator: ISQLGenerator;
begin
  FGenerators := TDictionary<TQueryLanguage,ISQLGenerator>.Create();
  LGenerator := TAnsiSQLGenerator.Create;
  RegisterGenerator(LGenerator);
end;

class destructor TSQLGeneratorRegister.Destroy;
begin
  FGenerators.Free;
end;

class function TSQLGeneratorRegister.GetGenerator(const AQueryLanguage: TQueryLanguage): ISQLGenerator;
begin
  Result := FGenerators[AQueryLanguage];
end;

class procedure TSQLGeneratorRegister.RegisterGenerator(const AGenerator: ISQLGenerator);
begin
  FGenerators.Add(AGenerator.GetQueryLanguage, AGenerator);
end;


end.
