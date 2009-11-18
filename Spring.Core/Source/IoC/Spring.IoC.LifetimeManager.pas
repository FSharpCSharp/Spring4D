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

unit Spring.IoC.LifetimeManager;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Spring.System,
  Spring.IoC.Core;

type
  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager, IInterface)
  private
    fModel: TComponentModel;
    function GetActivator: IComponentActivator;
  protected
    function TryGetInterface(instance: TObject; const IID: TGUID; out intf): Boolean;
    procedure DoAfterConstruction(instance: TObject); virtual;
    procedure DoBeforeDestruction(instance: TObject); virtual;
    property ComponentActivator: IComponentActivator read GetActivator;
    property Model: TComponentModel read fModel;
  public
    constructor Create(model: TComponentModel);
    function GetInstance: TObject; virtual; abstract;
    procedure ReleaseInstance(instance: TObject); virtual; abstract;
  end;

  TSingletonLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstance: TObject;
    fLifetimeWatcher: IInterface;
  public
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

  TTransientLifetimeManager = class(TLifetimeManagerBase)
  public
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

implementation


{$REGION 'TLifetimeManagerBase'}

type
  TInterfacedObjectHack = class(TInterfacedObject);

constructor TLifetimeManagerBase.Create(model: TComponentModel);
begin
  TArgument.CheckNotNull(model, 'model');
  inherited Create;
  fModel := model;
end;

function TLifetimeManagerBase.TryGetInterface(instance: TObject;
  const IID: TGUID; out intf): Boolean;
var
  localIntf: Pointer; // weak-reference
begin
  Assert(instance <> nil, 'instance should not be nil.');
  Result := instance.GetInterface(IInitializable, intf);
  Result := Result or (instance.GetInterface(IInterface, localIntf) and
    (IInterface(localIntf).QueryInterface(IID, intf) = S_OK));
  if instance is TInterfacedObject then
  begin
    Dec(TInterfacedObjectHack(instance).FRefCount);
  end;
end;

procedure TLifetimeManagerBase.DoAfterConstruction(instance: TObject);
var
  intf: Pointer;
begin
  if TryGetInterface(instance, IInitializable, intf) then
  begin
    IInitializable(intf).Initialize;
  end;
end;

procedure TLifetimeManagerBase.DoBeforeDestruction(instance: TObject);
var
  intf: Pointer;
begin
  if TryGetInterface(instance, IDisposable, intf) then
  begin
    IDisposable(intf).Dispose;
  end;
end;

function TLifetimeManagerBase.GetActivator: IComponentActivator;
begin
  Result := fModel.ComponentActivator;
end;

{$ENDREGION}


{$REGION 'TSingletonLifetimeManager'}

function TSingletonLifetimeManager.GetInstance: TObject;
var
  localInstance: TObject;
begin
  if fInstance = nil then
  begin
    localInstance := ComponentActivator.CreateInstance;
    if InterlockedCompareExchangePointer(Pointer(fInstance), localInstance, nil) <> nil then
    begin
      localInstance.Free;
    end
    else
    begin
      DoAfterConstruction(fInstance);
      if fInstance is TInterfacedObject then // Keep another reference
      begin
        fInstance.GetInterface(IInterface, fLifetimeWatcher);
      end
      else
      begin
        fLifetimeWatcher := TLifetimeWatcher.Create(
          procedure
          begin
            DoBeforeDestruction(fInstance);
            fInstance.Free;
          end
        );
      end;
    end;
  end;
  Result := fInstance;
end;

procedure TSingletonLifetimeManager.ReleaseInstance(instance: TObject);
begin
end;

{$ENDREGION}


{$REGION 'TTransientLifetimeManager'}

function TTransientLifetimeManager.GetInstance: TObject;
begin
  Result := ComponentActivator.CreateInstance;
  DoAfterConstruction(Result);
end;

procedure TTransientLifetimeManager.ReleaseInstance(instance: TObject);
begin
  TArgument.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
  instance.Free;
end;

{$ENDREGION}

end.
