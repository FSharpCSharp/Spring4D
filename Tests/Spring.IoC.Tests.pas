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

unit Spring.IoC.Tests;

interface

uses
  TestFramework,
  Spring.System,
  Spring.IoC;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestContainer = class abstract(TContainerTestCase)
  published
    procedure TestInterfaceWithGuid;
    procedure TestInterfaceWithoutGuid;
    procedure TestSimpleObject;
    procedure TestConstructorInjection;
    procedure TestResolveComponent;
    procedure TestSingleton;
    procedure TestTransient;
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
  public
    constructor Create(const foo: IFoo);
    property Intf: IFoo read fIntf;
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

{ TTestContainer }

procedure TTestContainer.TestInterfaceWithGuid;
var
  foo: IFoo;
begin
  fContainer.RegisterComponent<IFoo, TFoo>;
  foo := fContainer.Resolve<IFoo>;
  CheckNotNull(foo, 'intf should not be nil.');
  CheckTrue(foo is TFoo, 'intf should be a TFoo object.');
  CheckEquals(TFoo.ClassName, foo.Name);
end;

procedure TTestContainer.TestInterfaceWithoutGuid;
begin
  ExpectedException := ENotSupportedException;
  fContainer.RegisterComponent<INonGuid, TNonGuid>;
end;

procedure TTestContainer.TestSimpleObject;
var
  obj: TBase;
begin
  fContainer.RegisterComponent<TBase, TDerived>;
  obj := fContainer.Resolve<TBase>;
  CheckIs(obj, TDerived);
end;

procedure TTestContainer.TestConstructorInjection;
var
  obj: TFoo2Base;
begin
  fContainer.RegisterComponent<IFoo, TFoo>;
  fContainer.RegisterComponent<TFoo2Base, TFoo2>;
  obj := fContainer.Resolve<TFoo2Base>;
  CheckIs(obj, TFoo2);
end;

procedure TTestContainer.TestResolveComponent;
var
  foo: TFoo2;
begin
  fContainer.RegisterComponent<IFoo, TFoo>;
  foo := fContainer.Resolve<TFoo2>;
  CheckNotNull(foo);
end;

procedure TTestContainer.TestSingleton;
var
  obj1, obj2: TBase;
begin
  fContainer.RegisterComponent<TBase, TDerived>;
  obj1 := fContainer.Resolve<TBase>;
  obj2 := fContainer.Resolve<TBase>;
  CheckSame(obj1, obj2);
end;

procedure TTestContainer.TestTransient;
var
  obj1, obj2: TBase;
begin
  fContainer.RegisterComponent<TBase, TDerived>(ltTransient);
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

end.
