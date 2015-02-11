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

{$I Spring.inc}

unit Spring.Persistence.Core.ConnectionFactory;

interface

uses
  {$IFDEF DELPHIXE6_UP}
  JSON,
  {$ELSE}
  DBXJSON,
  {$ENDIF}
  Rtti,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  EORMTypeNotFoundException = Exception;
  EORMConstructorNotFound = Exception;
  EORMConnectionFactoryException = Exception;

  /// <summary>
  ///   Static class which acts as factory for <c>IDBConnection</c>s.
  /// </summary>
  TConnectionFactory = class sealed
  private
    class var fRegistered: IDictionary<TDBDriverType,TClass>;

    class function GetJsonPair(const jsonObject: TJSONObject; index: Integer): TJSONPair;
    class function GetJsonObjectCount(const jsonObject: TJSONObject): Integer;

    class function CreateConnection(connectionClass: TClass): TObject; overload;
    class function CreateConnection(connectionClass: TClass;
      const externalConnection: TObject): IDBConnection; overload;

    class function GetConnectionType(const qualifiedName: string): TRttiType;
    class procedure SetConnectionProperties(
      const externalConnection: TObject; const jsonObject: TJSONObject);
    class procedure SetConnectionConnected(
      const qualifiedName: string; const externalConnection: TObject);
  public
    class constructor Create;
    class destructor Destroy;

    class function GetInstance(key: TDBDriverType;
      const externalConnection: TObject): IDBConnection; overload;
    class function GetInstance(key: TDBDriverType;
      const jsonString: string): IDBConnection; overload;
    class function GetInstanceFromFilename(key: TDBDriverType;
      const jsonFilename: string): IDBConnection;
    class procedure RegisterConnection<T: class>(key: TDBDriverType);
    class function IsRegistered(key: TDBDriverType): Boolean;
  end;

 implementation

uses
  Classes,
  TypInfo,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TConnectionFactory'}

class constructor TConnectionFactory.Create;
begin
  fRegistered := TCollections.CreateDictionary<TDBDriverType,TClass>;
end;

class destructor TConnectionFactory.Destroy;
begin
  fRegistered := nil;
end;

class function TConnectionFactory.CreateConnection(connectionClass: TClass): TObject;
begin
  Result := TActivator.CreateInstance(connectionClass, [nil]);

  if not Assigned(Result) then
    raise EORMConnectionFactoryException.Create('Could not create connection');
end;

class function TConnectionFactory.CreateConnection(connectionClass: TClass;
  const externalConnection: TObject): IDBConnection;
var
  method: TRttiMethod;
  params: TArray<TRttiParameter>;
begin
  for method in TRttiContext.Create.GetType(connectionClass).GetMethods do
    if method.IsConstructor then
    begin
      params := method.GetParameters;
      if Length(params) = 1 then
        Exit(method.Invoke(connectionClass, [externalConnection]).AsType<IDBConnection>);
    end;
  Result := nil;
end;

class function TConnectionFactory.GetInstance(key: TDBDriverType;
  const externalConnection: TObject): IDBConnection;
var
  connectionClass: TClass;
begin
  if not IsRegistered(key) then
    raise EORMConnectionNotRegistered.Create('Connection not registered');

  connectionClass := fRegistered[key];

  Result := CreateConnection(connectionClass, externalConnection);
  if not Assigned(Result) then
    raise EORMUnsupportedType.Create('Connection type not supported');
end;

class function TConnectionFactory.GetInstance(key: TDBDriverType;
  const jsonString: string): IDBConnection;
var
  externalConnection: TObject;
  jsonObject: TJSONObject;
  rttiType: TRttiType;
  qualifiedName: string;
begin
  // resolve connection from file
  externalConnection := nil;
  jsonObject := TJSONObject.ParseJSONValue(jsonString) as TJSONObject;
  if Assigned(jsonObject) then
  try
    qualifiedName := GetJsonPair(jsonObject, 0).JsonString.Value;
    rttiType := GetConnectionType(qualifiedName);
    // try to create instance
    externalConnection := CreateConnection(rttiType.AsInstance.MetaclassType);
    SetConnectionProperties(externalConnection, GetJsonPair(jsonObject, 0).JsonValue as TJSONObject);
    SetConnectionConnected(qualifiedName, externalConnection);
  finally
    jsonObject.Free;
  end;
  Result := GetInstance(key, externalConnection);
  Result.AutoFreeConnection := True;
end;

class function TConnectionFactory.GetInstanceFromFilename(key: TDBDriverType;
  const jsonFilename: string): IDBConnection;
var
  fileStream: TStringStream;
begin
  fileStream := TStringStream.Create;
  try
    fileStream.LoadFromFile(jsonFilename);
    fileStream.Position := 0;
    Result := GetInstance(key, fileStream.DataString);
  finally
    fileStream.Free;
  end;
end;

class function TConnectionFactory.GetJsonObjectCount(
  const jsonObject: TJSONObject): Integer;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := jsonObject.Count;
  {$ELSE}
  Result := jsonObject.Size;
  {$ENDIF}
end;

class function TConnectionFactory.GetJsonPair(const jsonObject: TJSONObject;
  index: Integer): TJSONPair;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := jsonObject.Pairs[index];
  {$ELSE}
  Result := jsonObject.Get(index);
  {$ENDIF}
end;

class function TConnectionFactory.IsRegistered(key: TDBDriverType): Boolean;
begin
  Result := fRegistered.ContainsKey(key);
end;

class function TConnectionFactory.GetConnectionType(
  const qualifiedName: string): TRttiType;
begin
  Result := TRttiContext.Create.FindType(qualifiedName);
  if not Assigned(Result) and not Result.IsInstance then
    raise EORMTypeNotFoundException.CreateFmt('Type %S not found or is not an instance', [qualifiedName]);
end;

class procedure TConnectionFactory.RegisterConnection<T>(key: TDBDriverType);
begin
  if IsRegistered(key) then
    raise EORMConnectionAlreadyRegistered.Create('Connection already registered');

  fRegistered.Add(key, TClass(T));
end;

class procedure TConnectionFactory.SetConnectionConnected(
  const qualifiedName: string; const externalConnection: TObject);
begin
  TType.SetPropertyValue(externalConnection, 'Connected', True);
end;

class procedure TConnectionFactory.SetConnectionProperties(
  const externalConnection: TObject; const jsonObject: TJSONObject);
var
  i: Integer;
  jsonPair: TJSONPair;
  value: TValue;
  propType: TRttiType;
begin
  for i := 0 to GetJsonObjectCount(jsonObject) - 1 do
  begin
    jsonPair := GetJsonPair(jsonObject, i);
    propType := TType.GetType(externalConnection).GetProperty(jsonPair.JsonString.Value).PropertyType;
    value := jsonPair.JsonValue.Value;
    //do only simplest conversions
    case propType.TypeKind of
      tkString, tkWString, tkUString, tkLString: value := jsonPair.JsonValue.Value;
      tkEnumeration: begin
                       if (propType.Handle = TypeInfo(Boolean)) then
                         value := StrToBool(jsonPair.JsonValue.Value);
                     end;
      tkInteger: value := StrToInt(jsonPair.JsonValue.Value);
      tkInt64: value := StrToInt64(jsonPair.JsonValue.Value);
    end;
    TType.SetMemberValue(externalConnection, jsonPair.JsonString.Value, value);
  end;
end;

{$ENDREGION}


end.
