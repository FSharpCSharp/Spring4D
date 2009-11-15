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

{ TODO: Redesign the test cases to be more clear. }

unit Spring.Tests.IoC;

{$I Spring.inc}

interface

uses
  TestFramework,
  Spring.System,
  Spring.IoC,
  Spring.IoC.Core,
  Spring.IoC.LifetimeManager;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestContainer = class(TContainerTestCase)
  published
    procedure TestInterfaceWithGuid;
    procedure TestInterfaceWithoutGuid;
    procedure TestSimpleObject;
    procedure TestBootstrap;
    procedure TestSingleton;
    procedure TestTransient;
  end;

  TTestContainerInjection = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestCircularDependency = class(TContainerTestCase)
  published
    procedure TestDirect;
    procedure TestCycle1;
    procedure TestCycle2;
  end;

implementation

type
  IFoo = interface
    ['{96163ACB-E3FD-412E-A9A6-5084CE1BC25A}']
    function GetName: string;
    property Name: string read GetName;    
  end;

  TFoo = class(TInterfacedObject, IFoo, IInterface)
  private
    fName: string;
    function GetName: string;
  public
    constructor Create;
    property Name: string read GetName;
  end;

  INonGuid = interface
  end;

  TNonGUID = class(TInterfacedObject, INonGuid)
  end;

  TBase = class abstract
  end;

  TDerived = class(TBase)
  end;

  TFoo2Base = class

  end;

  TFoo2 = class(TFoo2Base)
  private
    fIntf: IFoo;
    [Injection]
    fFieldInjection: IFoo;
    fMethodInjection: IFoo;
    fPropertyInjection: IFoo;
  public
    constructor Create(const foo: IFoo);
    [Injection]
    procedure SetMethodInjection(const value: IFoo);
    property ConstructorInjection: IFoo read fIntf;
    [Injection]
    property PropertyInjection: IFoo read fPropertyInjection write fPropertyInjection;
    property FieldInjection: IFoo read fFieldInjection;
    property MethodInjection: IFoo read fMethodInjection;
  end;

{ TContainerTestCase }

procedure TContainerTestCase.SetUp;
begin
  inherited;
  fContainer := TContainer.Create;
end;

procedure TContainerTestCase.TearDown;
begin
  fContainer.Free;
  inherited;
end;

{ TFoo }

constructor TFoo.Create;
begin
  fName := ClassName;
end;

function TFoo.GetName: string;
begin
  Result := fName;
end;

{ TFoo2 }

constructor TFoo2.Create(const foo: IFoo);
begin
  inherited Create;
  fIntf := foo;
end;

procedure TFoo2.SetMethodInjection(const value: IFoo);
begin
  fMethodInjection := value;
end;

{ TTestContainer }

procedure TTestContainer.TestInterfaceWithGuid;
var
  foo: IFoo;
begin
  fContainer.RegisterType<IFoo, TFoo>;
  foo := fContainer.Resolve<IFoo>;
  try
    CheckNotNull(foo, 'foo should not be nil.');
    CheckTrue(foo is TFoo, 'foo should be a TFoo object.');
    CheckEquals(TFoo.ClassName, foo.Name);
  finally
    fContainer.Release(foo);
  end;
end;

procedure TTestContainer.TestInterfaceWithoutGuid;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<INonGuid, TNonGuid>;
end;

procedure TTestContainer.TestSimpleObject;
var
  obj: TBase;
begin
  fContainer.RegisterType<TBase, TDerived>(ltSingleton);
  obj := fContainer.Resolve<TBase>;
  try
    CheckIs(obj, TDerived);
  finally
    fContainer.Release(obj);
  end;
end;

procedure TTestContainer.TestBootstrap;
var
  foo: TFoo2;
begin
  fContainer.RegisterType<IFoo, TFoo>;
  foo := fContainer.Resolve<TFoo2>;
  try
    CheckNotNull(foo);
    CheckEquals(TFoo.ClassName, foo.ConstructorInjection.Name);
  finally
    fContainer.Release(foo);
  end;
end;

procedure TTestContainer.TestSingleton;
var
  obj1, obj2: TBase;
begin
  fContainer.RegisterType<TBase, TDerived>(ltSingleton);
  obj1 := fContainer.Resolve<TBase>;
  obj2 := fContainer.Resolve<TBase>;
  CheckSame(obj1, obj2);
end;

procedure TTestContainer.TestTransient;
var
  obj1, obj2: TBase;
begin
  fContainer.RegisterType<TBase, TDerived>(ltTransient);
  obj1 := fContainer.Resolve<TBase>;
  obj2 := fContainer.Resolve<TBase>;
  try
    CheckNotNull(obj1, 'obj1');
    CheckNotNull(obj2, 'obj2');
    CheckTrue(obj1 <> obj2, 'obj1 should not be the same as obj2.');
  finally
    obj1.Free;
    obj2.Free;
  end;
end;


type
  TCircularDependencyComponent = class(TInterfacedObject, IFoo)
  private
    fFoo: IFoo;
    function GetName: string;
  public
    constructor Create(const foo: IFoo);
    property Name: string read GetName;
  end;

{ TCircularDependencyComponent }

constructor TCircularDependencyComponent.Create(const foo: IFoo);
begin
  inherited Create;
  fFoo := foo;
end;

function TCircularDependencyComponent.GetName: string;
begin
  Result := fFoo.Name;
end;

type
  // [S1] <== C1 --> S2 <== C2 --> [S1]

  IS1 = interface
    ['{88C4F5E9-85B4-43D4-9265-0A9FAD099055}']
  end;

  IS2 = interface
    ['{9BFC513F-635C-42CD-B29D-9E66D47882A6}']
  end;

  TC1 = class(TInterfacedObject, IS1)
  private
    fS2: IS2;
  public
    constructor Create(const s2: IS2);
  end;

  TC2 = class(TInterfacedObject, IS2)
  private
    fS1: IS1;
  public
    constructor Create(const s1: IS1);
  end;

{ TC1 }

constructor TC1.Create(const s2: IS2);
begin
  inherited Create;
  fS2 := s2;
end;

{ TC2 }

constructor TC2.Create(const s1: IS1);
begin
  inherited Create;
  fS1 := s1;
end;

{ TTestCircularDependency }

procedure TTestCircularDependency.TestDirect;
var
  foo: IFoo;
begin
  fContainer.RegisterType<IFoo, TCircularDependencyComponent>;
  ExpectedException := ECircularDependencyException;
  foo := fContainer.Resolve<IFoo>;
end;

procedure TTestCircularDependency.TestCycle1;
var
  service: IS1;
begin
  fContainer.RegisterType<IS1, TC1>;
  fContainer.RegisterType<IS2, TC2>;
  ExpectedException := ECircularDependencyException;
  service := fContainer.Resolve<IS1>;
end;

procedure TTestCircularDependency.TestCycle2;
var
  service: IS2;
begin
  fContainer.RegisterType<IS1, TC1>;
  fContainer.RegisterType<IS2, TC2>;
  ExpectedException := ECircularDependencyException;
  service := fContainer.Resolve<IS2>;
end;

{$REGION 'TTestContainerInjection'}

procedure TTestContainerInjection.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<IFoo, TFoo>;
  fContainer.RegisterType<TFoo2Base, TFoo2>(ltSingleton);
end;

procedure TTestContainerInjection.TestConstructorInjection;
var
  obj: TFoo2Base;
begin
  obj := fContainer.Resolve<TFoo2Base>;
  CheckIs(obj, TFoo2);
  with TFoo2(obj) do
  begin
    CheckTrue(ConstructorInjection is TFoo);
    CheckTrue(MethodInjection is TFoo);
    CheckTrue(PropertyInjection is TFoo);
    CheckTrue(FieldInjection is TFoo);
  end;
end;

procedure TTestContainerInjection.TestFieldInjection;
begin

end;

procedure TTestContainerInjection.TestMethodInjection;
begin

end;

procedure TTestContainerInjection.TestPropertyInjection;
begin

end;

{$ENDREGION}

end.
