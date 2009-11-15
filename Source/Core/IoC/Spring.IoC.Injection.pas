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

unit Spring.IoC.Injection;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.IoC.Core;

type
  TInjectionBase = class abstract(TInterfacedObject, IInjection, IInterface)
  private
    fModel: TComponentModel;
    fMember: TRttiMember;
    fDependencies: TArray<TDependency>;
    function GetName: string;
    function GetDependencyCount: Integer;
    function GetModel: TComponentModel;
    function GetMemberType: TRttiMember;
  protected
//    property IsConstructor: Boolean;
//    property IsProperty: Boolean;
//    property IsMethod: Boolean;
//    property IsField: Boolean;
  public
    constructor Create(model: TComponentModel; member: TRttiMember; const dependencies: TArray<TDependency>);
    destructor Destroy; override;
    procedure Inject(instance: TObject; const parameters: TArray<TValue>); virtual; abstract;
    function GetDependencies: TArray<TDependency>;
    property Name: string read GetName;
    property DependencyCount: Integer read GetDependencyCount;
    property Model: TComponentModel read GetModel;
    property MemberType: TRttiMember read GetMemberType;
  end;

  TConstructorInjection = class(TInjectionBase)
  public
    constructor Create(model: TComponentModel; constructorMethod: TRttiMethod; const dependencies: TArray<TDependency>);
    procedure Inject(instance: TObject; const parameters: TArray<TValue>); override;
  end;

  TPropertyInjection = class(TInjectionBase)
  public
    constructor Create(model: TComponentModel; propertyMember: TRttiProperty; const dependencies: TArray<TDependency>);
    procedure Inject(instance: TObject; const parameters: TArray<TValue>); override;
  end;

  TMethodInjection = class(TInjectionBase)
  public
    constructor Create(model: TComponentModel; method: TRttiMethod; const dependencies: TArray<TDependency>);
    procedure Inject(instance: TObject; const parameters: TArray<TValue>); override;
  end;

  TFieldInjection = class(TInjectionBase)
  public
    constructor Create(model: TComponentModel; field: TRttiField; const dependencies: TArray<TDependency>);
    procedure Inject(instance: TObject; const parameters: TArray<TValue>); override;
  end;

  TInjectionFactory = class(TInterfacedObject, IInjectionFactory, IInterface)
  public
    function CreateConstructor(model: TComponentModel; constructorMethod: TRttiMethod;
      const dependencies: TArray<TDependency>): IInjection; virtual;
    function CreateProperty(model: TComponentModel; propertyMember: TRttiProperty;
      const dependencies: TArray<TDependency>): IInjection; virtual;
    function CreateMethod(model: TComponentModel; method: TRttiMethod;
      const dependencies: TArray<TDependency>): IInjection; virtual;
    function CreateField(model: TComponentModel; field: TRttiField;
      const dependencies: TArray<TDependency>): IInjection; virtual;
  end;

implementation

uses
  Spring.ResourceStrings,
  Spring.Helpers;


{$REGION 'TInjectionBase'}

constructor TInjectionBase.Create(model: TComponentModel;
  member: TRttiMember; const dependencies: TArray<TDependency>);
begin
  TArgument.CheckNotNull(model, 'model');
  TArgument.CheckNotNull(member, 'member');

  inherited Create;
  fModel := model;
  fMember := member;
  fDependencies := dependencies;
end;

destructor TInjectionBase.Destroy;
var
  dependency: TDependency;
begin
  for dependency in fDependencies do
  begin
    dependency.Free;
  end;
  inherited Destroy;
end;

function TInjectionBase.GetModel: TComponentModel;
begin
  Result := fModel;
end;

function TInjectionBase.GetName: string;
begin
  Result := fMember.Name;
end;

function TInjectionBase.GetDependencies: TArray<TDependency>;
begin
  Result := fDependencies;
end;

function TInjectionBase.GetDependencyCount: Integer;
begin
  Result := Length(fDependencies);
end;

function TInjectionBase.GetMemberType: TRttiMember;
begin
  Result := fMember;
end;

{$ENDREGION}


{$REGION 'TConstructorInjection'}

constructor TConstructorInjection.Create(model: TComponentModel;
  constructorMethod: TRttiMethod; const dependencies: TArray<TDependency>);
begin
  TArgument.CheckNotNull(constructorMethod, 'constructorMethod');
  TArgument.CheckTrue(constructorMethod.IsConstructor, 'The constructorMethod should be a constructor method.');

  inherited Create(model, constructorMethod, dependencies);
end;

procedure TConstructorInjection.Inject(instance: TObject;
  const parameters: TArray<TValue>);
begin
  MemberType.AsMethod.Invoke(instance, parameters);
end;

{$ENDREGION}


{$REGION 'TPropertyInjection'}

constructor TPropertyInjection.Create(model: TComponentModel;
  propertyMember: TRttiProperty; const dependencies: TArray<TDependency>);
begin
  inherited Create(model, propertyMember, dependencies);
end;

procedure TPropertyInjection.Inject(instance: TObject;
  const parameters: TArray<TValue>);
begin
  TArgument.CheckTrue(Length(parameters) = 1, SUnexpectedParameterLength);
  MemberType.AsProperty.SetValue(instance, parameters[0]);  // TEMP
end;

{$ENDREGION}


{$REGION 'TMethodInjection'}

constructor TMethodInjection.Create(model: TComponentModel;
  method: TRttiMethod; const dependencies: TArray<TDependency>);
begin
  inherited Create(model, method, dependencies);
end;

procedure TMethodInjection.Inject(instance: TObject;
  const parameters: TArray<TValue>);
begin
  MemberType.AsMethod.Invoke(instance, parameters);
end;

{$ENDREGION}


{$REGION 'TFieldInjection'}

constructor TFieldInjection.Create(model: TComponentModel;
  field: TRttiField; const dependencies: TArray<TDependency>);
begin
  inherited Create(model, field, dependencies);
end;

procedure TFieldInjection.Inject(instance: TObject;
  const parameters: TArray<TValue>);
begin
  TArgument.CheckTrue(Length(parameters) = 1, SUnexpectedParameterLength);
  MemberType.AsField.SetValue(instance, parameters[0]);  // TEMP
end;

{$ENDREGION}


{$REGION 'TDefaultInjectionFactory'}

function TInjectionFactory.CreateConstructor(model: TComponentModel;
  constructorMethod: TRttiMethod; const dependencies: TArray<TDependency>): IInjection;
begin
  Result := TConstructorInjection.Create(model, constructorMethod, dependencies);
end;

function TInjectionFactory.CreateProperty(model: TComponentModel;
  propertyMember: TRttiProperty; const dependencies: TArray<TDependency>): IInjection;
begin
  Result := TPropertyInjection.Create(model, propertyMember, dependencies);
end;

function TInjectionFactory.CreateMethod(model: TComponentModel;
  method: TRttiMethod; const dependencies: TArray<TDependency>): IInjection;
begin
  Result := TMethodInjection.Create(model, method, dependencies);
end;

function TInjectionFactory.CreateField(model: TComponentModel;
  field: TRttiField; const dependencies: TArray<TDependency>): IInjection;
begin
  Result := TFieldInjection.Create(model, field, dependencies);
end;

{$ENDREGION}

end.
