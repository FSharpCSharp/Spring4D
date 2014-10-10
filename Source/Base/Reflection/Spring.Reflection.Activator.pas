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
  Spring;

type

  {$REGION 'Activator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TValue;
  end;

  TActivator = record
  public
    class function CreateInstance(const classType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;
    class function CreateInstance(const typeInfo: PTypeInfo): TValue; overload; static;
    class function CreateInstance(const typeName: string): TValue; overload; static;
  end;

  {$ENDREGION}


implementation

uses
  Spring.Helpers,
  Spring.Reflection;


{$REGION 'TActivator'}

class function TActivator.CreateInstance(
  const classType: TRttiInstanceType): TValue;
var
  method: TRttiMethod;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

  for method in classType.GetMethods do
    if method.IsConstructor and (Length(method.GetParameters) = 0) then
      Exit(method.Invoke(classType.MetaclassType, []));
  Result := nil;
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

  rttiType := TType.GetType(typeInfo);
  if rttiType is TRttiInstanceType then
    Result := TActivator.CreateInstance(TRttiInstanceType(rttiType))
  else
    Result := nil;
end;

class function TActivator.CreateInstance(const typeName: string): TValue;
var
  rttiType: TRttiType;
begin
  rttiType := TType.FindType(typeName);
  if rttiType is TRttiInstanceType then
    Result := TActivator.CreateInstance(TRttiInstanceType(rttiType))
  else
    Result := nil;
end;

{$ENDREGION}


end.
