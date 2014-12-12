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

unit Spring.Reflection.Activator;

interface

uses
  Generics.Collections,
  Rtti,
  TypInfo;

type

  {$REGION 'Activator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TValue;
  end;

  TActivator = record
  private
    type TConstructor = function(InstanceOrVMT: Pointer; Alloc: ShortInt): TObject;
    class var Context: TRttiContext;
    class var ConstructorCache: TDictionary<TClass,TConstructor>;
    class function FindConstructor(const classType: TRttiInstanceType;
      const arguments: array of TValue): TRttiMethod; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure ClearCache; static;

    class function CreateInstance(const classType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const arguments: array of TValue): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;

    class function CreateInstance(typeInfo: PTypeInfo): TObject; overload; static;
    class function CreateInstance(const typeName: string): TObject; overload; static;

    class function CreateInstance(classType: TClass): TObject; overload; static; inline;
    class function CreateInstance(classType: TClass;
      const arguments: array of TValue): TObject; overload; static;

    class function CreateInstance<T: class>: T; overload; static; inline;
    class function CreateInstance<T: class>(
      const arguments: array of TValue): T; overload; static;
  end;

  {$ENDREGION}


implementation

uses
  Spring,
  Spring.ResourceStrings;


{$REGION 'TActivator'}

class constructor TActivator.Create;
begin
  Context := TRttiContext.Create;
  ConstructorCache := TDictionary<TClass,TConstructor>.Create;
end;

class destructor TActivator.Destroy;
begin
  ConstructorCache.Free;
  Context.Free;
end;

class procedure TActivator.ClearCache;
begin
  ConstructorCache.Clear;
  Context.Free;
  Context := TRttiContext.Create;
end;

class function TActivator.CreateInstance(
  const classType: TRttiInstanceType): TValue;
begin
  Result := CreateInstance(classType, []);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const arguments: array of TValue): TValue;
var
  method: TRttiMethod;
begin
  method := FindConstructor(classType, arguments);
  if Assigned(method) then
    Result := CreateInstance(classType, method, arguments)
  else
    raise ENotSupportedException.CreateResFmt(
      @SMissingConstructor, [classType.ClassName]);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue;
begin
  Result := constructorMethod.Invoke(classType.MetaclassType, arguments);
end;

class function TActivator.CreateInstance(typeInfo: PTypeInfo): TObject;
var
  classType: TClass;
  ctor: TConstructor;
  rttiType: TRttiType;
begin
  classType := typeInfo.TypeData.ClassType;
  if ConstructorCache.TryGetValue(classType, ctor) then
    Result := ctor(classType, 1)
  else
  begin
    rttiType := Context.GetType(typeInfo);
    Result := CreateInstance(TRttiInstanceType(rttiType), []).AsObject;
  end;
end;

class function TActivator.CreateInstance(const typeName: string): TObject;
var
  rttiType: TRttiType;
begin
  rttiType := Context.FindType(typeName);
  Result := CreateInstance(TRttiInstanceType(rttiType), []).AsObject;
end;

class function TActivator.CreateInstance(classType: TClass): TObject;
begin
  Result := CreateInstance(classType.ClassInfo);
end;

class function TActivator.CreateInstance(classType: TClass;
  const arguments: array of TValue): TObject;
var
  rttiType: TRttiType;
begin
  rttiType := Context.GetType(classType);
  Result := CreateInstance(TRttiInstanceType(rttiType), arguments).AsObject;
end;

class function TActivator.CreateInstance<T>: T;
begin
  Result := T(CreateInstance(TypeInfo(T)));
end;

class function TActivator.CreateInstance<T>(
  const arguments: array of TValue): T;
begin
  Result := T(CreateInstance(TClass(T), arguments));
end;

class function TActivator.FindConstructor(const classType: TRttiInstanceType;
  const arguments: array of TValue): TRttiMethod;

  function Assignable(const params: TArray<TRttiParameter>;
    const args: array of TValue): Boolean;
  var
    i: Integer;
    v: TValue;
  begin
    Result := Length(params) = Length(args);
    if Result then
      for i := Low(args) to High(args) do
        if not args[i].TryCast(params[i].paramType.Handle, v) then
          Exit(False);
  end;

var
  method: TRttiMethod;
begin
  for method in classType.GetMethods do
  begin
    if method.MethodKind <> mkConstructor then
      Continue;

    if Assignable(method.GetParameters, arguments) then
    begin
      if Length(arguments) = 0 then
        ConstructorCache.AddOrSetValue(classType.MetaclassType, method.CodeAddress);
      Exit(method);
    end;
  end;
  Result := nil;
end;

{$ENDREGION}


end.
