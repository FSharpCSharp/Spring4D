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
    class var Context: TRttiContext;
    class function FindConstructor(const classType: TRttiInstanceType;
      const arguments: array of TValue): TRttiMethod; static;
  public
    class function CreateInstance(const classType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const arguments: array of TValue): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;

    class function CreateInstance(const typeInfo: PTypeInfo): TValue; overload; static;
    class function CreateInstance(const typeName: string): TValue; overload; static;

    class function CreateInstance(classType: TClass): TObject; overload; static;
    class function CreateInstance(classType: TClass;
      const arguments: array of TValue): TObject; overload; static;

    class function CreateInstance<T: class>: T; overload; static;
    class function CreateInstance<T: class>(
      const arguments: array of TValue): T; overload; static;
  end;

  {$ENDREGION}


implementation

uses
  Spring,
  Spring.ResourceStrings;


{$REGION 'TActivator'}

class function TActivator.CreateInstance(
  const classType: TRttiInstanceType): TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

  Result := CreateInstance(classType, []);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const arguments: array of TValue): TValue;
var
  method: TRttiMethod;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
  Guard.CheckNotNull(constructorMethod, 'constructorMethod');
{$ENDIF}

  Result := constructorMethod.Invoke(classType.MetaclassType, arguments);
end;

class function TActivator.CreateInstance(const typeInfo: PTypeInfo): TValue;
var
  rttiType: TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  rttiType := Context.GetType(typeInfo);
  Result := TActivator.CreateInstance(rttiType as TRttiInstanceType)
end;

class function TActivator.CreateInstance(const typeName: string): TValue;
var
  rttiType: TRttiType;
begin
  rttiType := Context.FindType(typeName);
  Result := TActivator.CreateInstance(rttiType as TRttiInstanceType)
end;

class function TActivator.CreateInstance(classType: TClass): TObject;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'typeInfo');
{$ENDIF}

  Result := CreateInstance(classType.ClassInfo).AsObject;
end;

class function TActivator.CreateInstance(classType: TClass;
  const arguments: array of TValue): TObject;
var
  instanceType: TRttiInstanceType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'typeInfo');
{$ENDIF}

  instanceType := Context.GetType(classType.ClassInfo) as TRttiInstanceType;
  Result := CreateInstance(instanceType, arguments).AsObject;
end;

class function TActivator.CreateInstance<T>: T;
begin
  Result := CreateInstance(TypeInfo(T)).AsType<T>;
end;

class function TActivator.CreateInstance<T>(
  const arguments: array of TValue): T;
var
  instanceType: TRttiInstanceType;
begin
  instanceType := Context.GetType(TypeInfo(T)) as TRttiInstanceType;
  Result := CreateInstance(instanceType, arguments).AsType<T>;
end;

class function TActivator.FindConstructor(const classType: TRttiInstanceType;
  const arguments: array of TValue): TRttiMethod;
var
  argumentTypes: TArray<PTypeInfo>;
  i: Integer;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
  parameterTypes: TArray<PTypeInfo>;
begin
  SetLength(argumentTypes, Length(arguments));
  for i := Low(arguments) to High(arguments) do
    argumentTypes[i] := arguments[i].TypeInfo;

  for method in classType.GetMethods do
  begin
    if method.MethodKind <> mkConstructor then
      Continue;

    parameters := method.GetParameters;
    if Length(parameters) = Length(arguments) then
    begin
      SetLength(parameterTypes, Length(parameters));
      for i := Low(parameters) to High(parameters) do
        parameterTypes[i] := parameters[i].ParamType.Handle;
      if IsAssignableFrom(parameterTypes, argumentTypes) then
        Exit(method);
    end;
  end;
  Result := nil;
end;

{$ENDREGION}


end.
