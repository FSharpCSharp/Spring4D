{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.IoC;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.IoC.Core;

type
  TContainer = class;

//  IContainer = TFunc<TContainer>;

  /// <summary>
  /// Represents an IoC (Inversion of Control) container.
  /// </summary>
  /// <remarks>
  /// </remarks>
  TContainer = class
  private
    fManager: TComponentModelManager;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterComponent<TServiceType; TComponentType: TServiceType>: TContainer; overload;
    function RegisterComponent<TServiceType; TComponentType: TServiceType>(lifetime: TLifetimeType): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string; lifetime: TLifetimeType): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string; lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload;
//    function RegisterComponent(serviceType, componentType: PTypeInfo): TContainer; overload;
    function RegisterComponent(const name: string; serviceType, componentType: PTypeInfo;
      lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterDependencies(const name: string; componentType: PTypeInfo;
//      const dependencies: array of IDependency): TContainer; overload; virtual; abstract;
    procedure Release(instance: TObject);
    function Resolve<T>: T; overload;
//    function Resolve<T>(const name: string; const dependencies: array of IDependency): T; overload;
//    function Resolve<T>(const name: string): T; overload;
    function Resolve(typeInfo: PTypeInfo): Pointer; overload;
//    function ResolveAll<TServiceType>: IEnumerable<TServiceType>;
  end;

  EContainerException = Spring.IoC.Core.EContainerException;

implementation

uses
  Spring.ResourceStrings;

{$REGION 'TContainer'}

constructor TContainer.Create;
begin
  inherited Create;
  fManager := TComponentModelManager.Create;
end;

destructor TContainer.Destroy;
begin
  fManager.Free;
  inherited Destroy;
end;

function TContainer.RegisterComponent<TServiceType, TComponentType>: TContainer;
begin
  Result := RegisterComponent('', TypeInfo(TServiceType), TypeInfo(TComponentType), ltUnknown, []);
end;

function TContainer.RegisterComponent<TServiceType, TComponentType>(lifetime: TLifetimeType): TContainer;
begin
  Result := RegisterComponent('', TypeInfo(TServiceType), TypeInfo(TComponentType), lifetime, []);
end;

procedure TContainer.Release(instance: TObject);
begin
  fManager.Release(instance);
end;

function TContainer.RegisterComponent(const name: string; serviceType,
  componentType: PTypeInfo; lifetime: TLifetimeType;
  const dependencies: array of IDependency): TContainer;
begin
  fManager.AddComponentModel(name, serviceType, componentType, lifetime);
  Result := Self;
end;

function TContainer.Resolve(typeInfo: PTypeInfo): Pointer;
begin
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');
  Result := fManager.Resolve(typeInfo);
end;

function TContainer.Resolve<T>: T;
begin
  TRtti.CheckTypeKind<T>([tkClass, tkInterface]);
  PPointer(@Result)^ := Resolve(TypeInfo(T));
end;

{$ENDREGION}

end.
