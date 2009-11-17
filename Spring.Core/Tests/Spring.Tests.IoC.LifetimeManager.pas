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

unit Spring.Tests.IoC.LifetimeManager;

interface

uses
  TestFramework,
  Rtti,
  Spring.System,
  Spring.IoC.Core,
  Spring.IoC.LifetimeManager;

type
  TLifetimeManagerTestCase = class abstract(TTestCase)
  private
    type
      TMockObjectActivator = class(TInterfaceBase, IComponentActivator, IInterface)
      private
        fModel: TComponentModel;
      public
        function CreateInstance(model: TComponentModel): TObject;
        procedure DestroyInstance(instance: TObject);
        property Model: TComponentModel read fModel;
      end;

      TMockInterfacedObjectActivator = class(TInterfaceBase, IComponentActivator, IInterface)
      private
        fModel: TComponentModel;
      public
        function CreateInstance(model: TComponentModel): TObject;
        procedure DestroyInstance(instance: TObject);
        property Model: TComponentModel read fModel;
      end;

      TMockObject = class
      end;

      TMockInterfacedObject = class(TInterfacedObject)
      end;
  protected
    fContext: TRttiContext;
    fLifetimeManager: ILifetimeManager;
    fModel: TComponentModel;
    fActivator: TMockObjectActivator;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestSingletonLifetimeManager = class(TLifetimeManagerTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReferences;
  end;

  TTestTransientLifetimeManager = class(TLifetimeManagerTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReferences;
  end;

  TTestPerThreadLifetimeManager = class(TLifetimeManagerTestCase)

  end;

implementation

{ TLifetimeManagerTestCase }

procedure TLifetimeManagerTestCase.SetUp;
begin
  inherited;
  fContext := TRttiContext.Create;
  fModel := TComponentModel.Create('',
    fContext.GetType(TMockObject),
    fContext.GetType(TMockObject).AsInstance
  );
  fActivator := TMockObjectActivator.Create;
  fModel.ComponentActivator := fActivator;
end;

procedure TLifetimeManagerTestCase.TearDown;
begin
  fActivator.Free;
  fModel.Free;
  fContext.Free;
  inherited;
end;

{ TSingletonLifetimeTestCase }

procedure TTestSingletonLifetimeManager.SetUp;
begin
  inherited;
  fLifetimeManager := TSingletonLifetimeManager.Create(fModel);
end;

procedure TTestSingletonLifetimeManager.TearDown;
begin
  fLifetimeManager := nil;
  inherited;
end;

procedure TTestSingletonLifetimeManager.TestReferences;
var
  obj1, obj2: TObject;
begin
  obj1 := fLifetimeManager.GetInstance;
  obj2 := fLifetimeManager.GetInstance;
  try
    CheckIs(obj1, TMockObject, 'obj1');
    CheckIs(obj2, TMockObject, 'obj2');
    CheckSame(obj1, obj2);
    CheckSame(fActivator.Model, fModel);
  finally
    fLifetimeManager.Release(obj1);
    fLifetimeManager.Release(obj2);
  end;
end;

{ TTestTransientLifetimeManager }

procedure TTestTransientLifetimeManager.SetUp;
begin
  inherited;
  fLifetimeManager := TTransientLifetimeManager.Create(fModel);
end;

procedure TTestTransientLifetimeManager.TearDown;
begin
  fLifetimeManager := nil;
  inherited;
end;

procedure TTestTransientLifetimeManager.TestReferences;
var
  obj1, obj2: TObject;
begin
  obj1 := fLifetimeManager.GetInstance;
  obj2 := fLifetimeManager.GetInstance;
  try
    CheckIs(obj1, TMockObject, 'obj1');
    CheckIs(obj2, TMockObject, 'obj2');
    CheckTrue(obj1 <> obj2);
    CheckSame(fActivator.Model, fModel);
  finally
    fLifetimeManager.Release(obj1);
    fLifetimeManager.Release(obj2);
  end;
end;

{ TLifetimeManagerTestCase.TMockComponentActivator }

function TLifetimeManagerTestCase.TMockObjectActivator.CreateInstance(
  model: TComponentModel): TObject;
begin
  Result := TMockObject.Create;
  fModel := model;
end;

procedure TLifetimeManagerTestCase.TMockObjectActivator.DestroyInstance(
  instance: TObject);
begin

end;

{ TLifetimeManagerTestCase.TMockInterfacedObjectActivator }

function TLifetimeManagerTestCase.TMockInterfacedObjectActivator.CreateInstance(
  model: TComponentModel): TObject;
begin
  Result := TMockInterfacedObject.Create;
  fModel := model;
end;

procedure TLifetimeManagerTestCase.TMockInterfacedObjectActivator.DestroyInstance(
  instance: TObject);
begin

end;

end.
