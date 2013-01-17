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
  ,SysUtils
  ,DBXJSON
  ,Rtti
  ;

type
  EORMTypeNotFoundException = Exception;
  EORMConstructorNotFound = Exception;
  EORMConnectionFactoryException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Static class which acts as factory for <c>IDBConnection</c>s.
  ///	</summary>
  {$ENDREGION}
  TConnectionFactory = class sealed
  private
    class var FRegistered: TDictionary<TDBDriverType, TClass>;
  protected
    class function ConcreteCreate(AClass: TClass; AConcreteConnection: TObject): IDBConnection; overload;
    class function ConcreteCreate(AClass: TClass): TObject; overload;

    class function GetConnectionType(const AQualifiedName: string): TRttiType;
    class procedure SetConnectionProperties(AConcreteConnection: TObject; AJsonObj: TJSONObject);
    class procedure SetConnectionConnected(const AQualifiedName: string; AConcreteConnection: TObject);
  public
    class constructor Create;
    class destructor Destroy;

    class function GetInstance(AKey: TDBDriverType; AConcreteConnection: TObject): IDBConnection; overload;
    class function GetInstance(AKey: TDBDriverType; const AJsonString: string): IDBConnection; overload;
    class function GetInstanceFromFilename(AKey: TDBDriverType; const AJsonFilename: string): IDBConnection;
    class procedure RegisterConnection<T: class>(AKey: TDBDriverType);
    class function IsRegistered(AKey: TDBDriverType): Boolean;

  end;


implementation

uses
  Core.Exceptions
  ,Mapping.RttiExplorer
  ,Core.Reflection
  ,TypInfo
  ,Classes
  ;

{ TConnectionFactory }

class function TConnectionFactory.ConcreteCreate(AClass: TClass): TObject;
var
  LType: TRttiType;
  LConstructors: TList<TRttiMethod>;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
  LArgs: array of TValue;
  i: Integer;
begin
  Result := nil;
  LType := TRttiContext.Create.GetType(AClass);
  LConstructors := TList<TRttiMethod>.Create;
  try
    TRttiExplorer.GetDeclaredConstructors(AClass, LConstructors);

    if LConstructors.Count < 0 then
      raise EORMConstructorNotFound.Create('Constructor not found');

    LMethod := TRttiExplorer.GetMethodWithLessParameters(LConstructors);
    LParams := LMethod.GetParameters;
    SetLength(LArgs, Length(LParams));
    for i := Low(LArgs) to High(LArgs) do
    begin
      LArgs[i] := TValue.Empty;
    end;

    Result := LMethod.Invoke(LType.AsInstance.MetaclassType, LArgs).AsObject;

    if not Assigned(Result) then
      raise EORMConnectionFactoryException.Create('Could not create connection');

  finally
    LConstructors.Free;
  end;
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

class constructor TConnectionFactory.Create;
begin
  FRegistered := TDictionary<TDBDriverType, TClass>.Create(100);
end;

class destructor TConnectionFactory.Destroy;
begin
  FRegistered.Free;
end;

class function TConnectionFactory.GetInstance(AKey: TDBDriverType; const AJsonString: string): IDBConnection;
var
  LConcreteConnection: TObject;
  LJsonObj: TJSONObject;
  LType: TRttiType;
  sQualifiedName: string;
begin
  //resolve connection from file
  LConcreteConnection := nil;
  LJsonObj := TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
  if Assigned(LJsonObj) then
  begin
    try
      sQualifiedName := LJsonObj.Get(0).JsonString.Value;
      LType := GetConnectionType(sQualifiedName);
      //try to create instance
      LConcreteConnection := ConcreteCreate(LType.AsInstance.MetaclassType);
      SetConnectionProperties(LConcreteConnection, LJsonObj.Get(0).JsonValue as TJSONObject);
      SetConnectionConnected(sQualifiedName, LConcreteConnection);
    finally
      LJsonObj.Free;
    end;
  end;
  Result := GetInstance(AKey, LConcreteConnection);
  Result.AutoFreeConnection := True;
end;

class function TConnectionFactory.GetInstanceFromFilename(AKey: TDBDriverType;
  const AJsonFilename: string): IDBConnection;
var
  LFileStream: TStringStream;
begin
  LFileStream := TStringStream.Create();
  try
    LFileStream.LoadFromFile(AJsonFilename);
    LFileStream.Position := 0;
    Result := GetInstance(AKey, LFileStream.DataString);
  finally
    LFileStream.Free;
  end;
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

class function TConnectionFactory.GetConnectionType(const AQualifiedName: string): TRttiType;
begin
  Result := TRttiContext.Create.FindType(AQualifiedName);
  if not Assigned(Result) and not Result.IsInstance then
    raise EORMTypeNotFoundException.CreateFmt('Type %S not found or is not an instance', [AQualifiedName]);
end;

class procedure TConnectionFactory.SetConnectionProperties(AConcreteConnection: TObject; AJsonObj: TJSONObject);
var
  LPair: TJSONPair;
  LValue: TValue;
  bFree: Boolean;
  i: Integer;
  LConverted: TValue;
begin
  //set properties from json config
  for i := 0 to AJsonObj.Size - 1 do
  begin
    LPair := AJsonObj.Get(i);
    LValue := LPair.JsonValue.Value;
    if LValue.TryConvert(TRttiExplorer.GetMemberTypeInfo(AConcreteConnection.ClassType, LPair.JsonString.Value), LConverted, bFree) then
      LValue := LConverted;
    TRttiExplorer.SetMemberValueSimple(AConcreteConnection, LPair.JsonString.Value, LValue);
  end;
end;

class procedure TConnectionFactory.SetConnectionConnected(const AQualifiedName: string; AConcreteConnection: TObject);
var
  LProp: TRttiProperty;
begin
  //set connected property to true
  LProp := TRttiContext.Create.FindType(AQualifiedName).GetProperty('Connected');
  if Assigned(LProp) then
  begin
    LProp.SetValue(AConcreteConnection, True);
  end;
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
