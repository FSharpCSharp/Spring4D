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

unit Spring.Tests.IoC.Components;

interface

uses
  TestFramework,
  Spring.System,
  Spring.IoC.Core;

type
  INameService = interface
    ['{96163ACB-E3FD-412E-A9A6-5084CE1BC25A}']
    function GetName: string;
    property Name: string read GetName;
  end;

  TNameServiceBase = class(TInterfacedObject, INameService, IInterface)
  protected
    function GetName: string; virtual; abstract;
  public
    property Name: string read GetName;
  end;

  TNameService = class(TNameServiceBase)
  private
    fName: string;
  protected
    function GetName: string; override;
  public
    constructor Create;
    const DefaultNameString: string = 'Name';
  end;

  TBase = class abstract
  end;

  TDerived = class(TBase)
  end;

  TFoo2Base = class

  end;

  TFoo2 = class(TFoo2Base)
  private
    fIntf: INameService;
    [Injection]
    fFieldInjection: INameService;
    fMethodInjection: INameService;
    fPropertyInjection: INameService;
  public
    constructor Create(const foo: INameService);
    [Injection]
    procedure SetMethodInjection(const value: INameService);
    property ConstructorInjection: INameService read fIntf;
    [Injection]
    property PropertyInjection: INameService read fPropertyInjection write fPropertyInjection;
    property FieldInjection: INameService read fFieldInjection;
    property MethodInjection: INameService read fMethodInjection;
  end;

  {$REGION 'Non-Guid Interfaces'}

  INonGuid = interface
  end;

  TNonGuid = class(TInterfacedObject, INonGuid)
  end;

  INonGuid<T> = interface
  end;

  TNonGuid<T> = class(TInterfacedObject, INonGuid<T>)
  end;

  {$ENDREGION}

  {$REGION 'Circular Dependency Services & Components (Chicken-Egg)'}

  // IChicken <== TChicken --> IEgg <== TEgg --> IChicken

  IChicken = interface
    ['{88C4F5E9-85B4-43D4-9265-0A9FAD099055}']
  end;

  IEgg = interface
    ['{9BFC513F-635C-42CD-B29D-9E66D47882A6}']
  end;

  TChicken = class(TInterfacedObject, IChicken)
  private
    fEgg: IEgg;
  public
    constructor Create(const egg: IEgg);
  end;

  TEgg = class(TInterfacedObject, IEgg)
  private
    fChicken: IChicken;
  public
    constructor Create(const chicken: IChicken);
  end;

  TCircularDependencyChicken = class(TInterfacedObject, IChicken)
  private
    fChicken: IChicken;
  public
    constructor Create(const chicken: IChicken);
  end;

  {$ENDREGION}

implementation

{ TNameComponent }

constructor TNameService.Create;
begin
  fName := DefaultNameString;
end;

function TNameService.GetName: string;
begin
  Result := fName;
end;

{ TFoo2 }

constructor TFoo2.Create(const foo: INameService);
begin
  inherited Create;
  fIntf := foo;
end;

procedure TFoo2.SetMethodInjection(const value: INameService);
begin
  fMethodInjection := value;
end;


{ TCircularDependencyChicken }

constructor TCircularDependencyChicken.Create(const chicken: IChicken);
begin
  inherited Create;
  fChicken := chicken;
end;

{ TChicken }

constructor TChicken.Create(const egg: IEgg);
begin
  inherited Create;
  fEgg := egg;
end;

{ TEgg }

constructor TEgg.Create(const chicken: IChicken);
begin
  inherited Create;
  fChicken := chicken;
end;

end.
