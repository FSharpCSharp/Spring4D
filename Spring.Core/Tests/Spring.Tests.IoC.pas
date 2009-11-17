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
  SysUtils,
  TestFramework,
  Spring.System,
  Spring.IoC,
  Spring.IoC.Core,
  Spring.IoC.LifetimeManager,
  Spring.Tests.IoC.Components;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestEmptyContainer = class(TContainerTestCase)
  published
    procedure TestResolveUnknownIntferfaceService;
    // TODO: TestResolveUnknownClassService
//    procedure TestResolveUnknownClassService;
    procedure TestRegisterNonGuidInterfaceService;
    procedure TestRegisterGenericInterfaceService;
  end;

  TTestSimpleContainer = class(TContainerTestCase)
  published
    procedure TestInterfaceService;
    procedure TestAbstractClassService;
    procedure TestServiceSameAsComponent;
    procedure TestBootstrap;
    procedure TestSingleton;
    procedure TestTransient;
  end;

  TTestMemberInjection = class(TContainerTestCase)
  protected
    fComponent: TFoo2;
    procedure SetUp; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestDirectCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolve;
  end;

  TTestCrossedCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolveChicken;
    procedure TestResolveEgg;
  end;

implementation

{$REGION 'TContainerTestCase'}

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

{$ENDREGION}


{$REGION 'TTestEmptyContainer'}

procedure TTestEmptyContainer.TestResolveUnknownIntferfaceService;
begin
  ExpectedException := EResolveException;
  fContainer.Resolve<INameService>;
end;

//procedure TTestEmptyContainer.TestResolveUnknownClassService;
//begin
//  ExpectedException := EResolveException;
//  fContainer.Resolve<TFoo2>;
//end;

procedure TTestEmptyContainer.TestRegisterNonGuidInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<INonGuid, TNonGuid>;
end;

procedure TTestEmptyContainer.TestRegisterGenericInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<INonGuid<TObject>, TNonGuid<TObject>>;
end;

{$ENDREGION}


{$REGION 'TTestSimpleContainer'}

procedure TTestSimpleContainer.TestInterfaceService;
var
  service: INameService;
begin
  fContainer.RegisterType<INameService, TNameService>;
  service := fContainer.Resolve<INameService>;
  try
    CheckNotNull(service, 'service should not be nil.');
    CheckTrue(service is TNameService, 'service should be a TNameService instance.');
    CheckEquals(TNameService.DefaultNameString, service.Name);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestAbstractClassService;
var
  service: TNameServiceBase;
begin
  fContainer.RegisterType<TNameServiceBase, TNameService>;
  service := fContainer.Resolve<TNameServiceBase>;
  try
    CheckIs(service, TNameService, 'service should be a TNameService instance.');
    CheckEquals(TNameService.DefaultNameString, service.Name);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestServiceSameAsComponent;
var
  service: TNameService;
begin
  fContainer.RegisterType<TNameService, TNameService>;
  service := fContainer.Resolve<TNameService>;
  try
    CheckNotNull(service, 'service should not be null.');
    CheckEquals(TNameService.DefaultNameString, service.Name);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestBootstrap;
var
  foo: TFoo2;
begin
  fContainer.RegisterType<INameService, TNameService>;
  foo := fContainer.Resolve<TFoo2>;
  try
    CheckNotNull(foo);
    CheckEquals(TNameService.DefaultNameString, foo.ConstructorInjection.Name);
  finally
    fContainer.Release(foo);
  end;
end;

procedure TTestSimpleContainer.TestSingleton;
var
  obj1, obj2: TBase;
begin
  fContainer.RegisterType<TBase, TDerived>(ltSingleton);
  obj1 := fContainer.Resolve<TBase>;
  obj2 := fContainer.Resolve<TBase>;
  try
    CheckSame(obj1, obj2, 'obj1 should be the same as obj2.');
  finally
    fContainer.Release(obj1);
    fContainer.Release(obj2);
  end;
end;

procedure TTestSimpleContainer.TestTransient;
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
    fContainer.Release(obj1);
    fContainer.Release(obj2);
  end;
end;

{$ENDREGION}


{$REGION 'TTestMemberInjection'}

procedure TTestMemberInjection.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<INameService, TNameService>;
  fContainer.RegisterType<TFoo2Base, TFoo2>(ltSingleton);
  try
    fComponent := fContainer.Resolve<TFoo2Base> as TFoo2;
  except on e: Exception do
    Fail('fService should be an instance of TFoo2. exception: ' + e.Message);
  end;
end;

procedure TTestMemberInjection.TestConstructorInjection;
begin
  CheckTrue(fComponent.ConstructorInjection is TNameService);
end;

procedure TTestMemberInjection.TestPropertyInjection;
begin
  CheckTrue(fComponent.PropertyInjection is TNameService);
end;

procedure TTestMemberInjection.TestMethodInjection;
begin
  CheckTrue(fComponent.MethodInjection is TNameService);
end;

procedure TTestMemberInjection.TestFieldInjection;
begin
  CheckTrue(fComponent.FieldInjection is TNameService);
end;

{$ENDREGION}


{$REGION 'TTestDirectCircularDependency'}

procedure TTestDirectCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<IChicken, TCircularDependencyChicken>;
end;

procedure TTestDirectCircularDependency.TestResolve;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

{$ENDREGION}


{$REGION 'TTestCrossedCircularDependency'}

procedure TTestCrossedCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<IChicken, TChicken>;
  fContainer.RegisterType<IEgg, TEgg>;
end;

procedure TTestCrossedCircularDependency.TestResolveChicken;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

procedure TTestCrossedCircularDependency.TestResolveEgg;
var
  egg: IEgg;
begin
  ExpectedException := ECircularDependencyException;
  egg := fContainer.Resolve<IEgg>;
end;

{$ENDREGION}


end.
