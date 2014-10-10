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

    class function GetJsonPair(const AJsonObject: TJSONObject; const AIndex: Integer): TJSONPair;
    class function GetJsonObjectCount(const AJsonObject: TJSONObject): Integer;
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
  Classes,
  TypInfo,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TConnectionFactory'}

class function TConnectionFactory.ConcreteCreate(AClass: TClass): TObject;
var
  LType: TRttiType;
  LConstructors: IList<TRttiMethod>;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
  LArgs: array of TValue;
  i: Integer;
begin
  LType := TRttiContext.Create.GetType(AClass);
  LConstructors := TCollections.CreateList<TRttiMethod>;
  TRttiExplorer.GetDeclaredConstructors(AClass, LConstructors);

  if LConstructors.IsEmpty then
    raise EORMConstructorNotFound.CreateFmt('Constructor for class %S not found', [AClass.ClassName]);

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
end;

class function TConnectionFactory.ConcreteCreate(AClass: TClass; AConcreteConnection: TObject): IDBConnection;
var
  LParams: TArray<TRttiParameter>;
  LMethod: TRttiMethod;
begin
  for LMethod in TRttiContext.Create.GetType(AClass).GetMethods do
  begin
    if LMethod.IsConstructor then
    begin
      LParams := LMethod.GetParameters;
      if (Length(LParams) = 1) then
      begin
        Result := LMethod.Invoke(AClass, [AConcreteConnection]).AsType<IDBConnection>;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

class constructor TConnectionFactory.Create;
begin
  fRegistered := TCollections.CreateDictionary<TDBDriverType,TClass>(100);
end;

class destructor TConnectionFactory.Destroy;
begin
  fRegistered := nil;
end;

class function TConnectionFactory.GetInstance(AKey: TDBDriverType; const AJsonString: string): IDBConnection;
var
  LConcreteConnection: TObject;
  LJsonObj: TJSONObject;
  LType: TRttiType;
  sQualifiedName: string;
begin
  // resolve connection from file
  LConcreteConnection := nil;
  LJsonObj := TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
  if Assigned(LJsonObj) then
  begin
    try
      sQualifiedName := GetJsonPair(LJsonObj, 0).JsonString.Value;
      LType := GetConnectionType(sQualifiedName);
      // try to create instance
      LConcreteConnection := ConcreteCreate(LType.AsInstance.MetaclassType);
      SetConnectionProperties(LConcreteConnection, GetJsonPair(LJsonObj, 0).JsonValue as TJSONObject);
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
  LFileStream := TStringStream.Create;
  try
    LFileStream.LoadFromFile(AJsonFilename);
    LFileStream.Position := 0;
    Result := GetInstance(AKey, LFileStream.DataString);
  finally
    LFileStream.Free;
  end;
end;

class function TConnectionFactory.GetJsonObjectCount(
  const AJsonObject: TJSONObject): Integer;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := AJsonObject.Count;
  {$ELSE}
  Result := AJsonObject.Size;
  {$ENDIF}
end;

class function TConnectionFactory.GetJsonPair(const AJsonObject: TJSONObject; const AIndex: Integer): TJSONPair;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := AJsonObject.Pairs[0];
  {$ELSE}
  Result := AJsonObject.Get(0);
  {$ENDIF}
end;

class function TConnectionFactory.GetInstance(AKey: TDBDriverType;
  AConcreteConnection: TObject): IDBConnection;
var
  LClass: TClass;
begin
  if not IsRegistered(AKey) then
    raise EORMConnectionNotRegistered.Create('Connection not registered');

  LClass := fRegistered[AKey];

  Result := ConcreteCreate(LClass, AConcreteConnection);
  if not Assigned(Result) then
    raise EORMUnsupportedType.Create('Connection type not supported');
end;

class function TConnectionFactory.IsRegistered(AKey: TDBDriverType): Boolean;
begin
  Result := fRegistered.ContainsKey(AKey);
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
  // set properties from json config
  for i := 0 to GetJsonObjectCount(AJsonObj) - 1 do
  begin
    LPair := GetJsonPair(AJsonObj, i);
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
  // set connected property to true
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

  fRegistered.Add(AKey, LClass);
end;

{$ENDREGION}


end.
