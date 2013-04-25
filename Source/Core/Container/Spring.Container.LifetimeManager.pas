{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

unit Spring.Container.LifetimeManager;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Spring,
  Spring.Services,
  Spring.Container.Core,
  Spring.Container.Pool;

type
  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager, IInterface)
  private
    fModel: TComponentModel;
    function GetActivator: IComponentActivator;
  protected
    procedure DoAfterConstruction(instance: TValue); virtual;
    procedure DoBeforeDestruction(instance: TValue); virtual;
  protected
    function TryGetInterfaceWithoutCopy(instance: TValue; const IID: TGUID; out intf): Boolean;
    property ComponentActivator: IComponentActivator read GetActivator;
    property Model: TComponentModel read fModel;
  public
    constructor Create(model: TComponentModel);
    function GetInstance: TValue; overload;
    function GetInstance(resolver: IDependencyResolver): TValue; overload; virtual; abstract;
    procedure ReleaseInstance(instance: TValue); virtual; abstract;
  end;

  TSingletonLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstance: TFunc<TValue>;
  public
    destructor Destroy; override;
    function GetInstance(resolver: IDependencyResolver): TValue; override;
    procedure ReleaseInstance(instance: TValue); override;
  end;

  TTransientLifetimeManager = class(TLifetimeManagerBase)
  public
    function GetInstance(resolver: IDependencyResolver): TValue; override;
    procedure ReleaseInstance(instance: TValue); override;
  end;

  TSingletonPerThreadLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstances: TDictionary<TThreadID, TFunc<TValue>>;
  protected
    procedure HandleValueNotify(sender: TObject; const item: TFunc<TValue>; action: TCollectionNotification);
    function CreateHolder(instance: TValue): TFunc<TValue>; virtual;
  public
    constructor Create(model: TComponentModel);
    destructor Destroy; override;
    function GetInstance(resolver: IDependencyResolver): TValue; override;
    procedure ReleaseInstance(instance: TValue); override;
  end;

  TPooledLifetimeManager = class(TLifetimeManagerBase)
  private
    fPool: IObjectPool;
  public
    constructor Create(model: TComponentModel);
    function GetInstance(resolver: IDependencyResolver): TValue; override;
    procedure ReleaseInstance(instance: TValue); override;
  end;

implementation

uses
  Rtti,
  TypInfo;

{$REGION 'TLifetimeManagerBase'}

constructor TLifetimeManagerBase.Create(model: TComponentModel);
begin
  TArgument.CheckNotNull(model, 'model');
  inherited Create;
  fModel := model;
end;

function TLifetimeManagerBase.TryGetInterfaceWithoutCopy(instance: TValue;
  const IID: TGUID; out intf): Boolean;

  function GetInterface(Self: TObject; const IID: TGUID; out Obj): Boolean;
  var
    interfaceEntry: PInterfaceEntry;
  begin
    interfaceEntry := Self.GetInterfaceEntry(IID);
    if Assigned(interfaceEntry) and (interfaceEntry.IOffset <> 0) then
    begin
      Pointer(Obj) := Pointer(NativeInt(Self) + InterfaceEntry.IOffset);
      Result := Pointer(Obj) <> nil;
    end
    else
      Result := Self.GetInterface(IID, Obj);
  end;

var
  localIntf: Pointer; // weak-reference
begin
  Assert(not instance.IsEmpty, 'instance should not be empty.');
  case instance.Kind of
    tkClass:
    begin
      Result := GetInterface(instance.AsObject, IInitializable, intf);
      Result := Result or (GetInterface(instance.AsObject, IInterface, localIntf)
        and (IInterface(localIntf).QueryInterface(IID, intf) = S_OK));
    end;
    tkInterface:
    begin
      Result := instance.AsInterface.QueryInterface(IID, intf) = S_OK;
    end;
  else
    Result := False;
  end
end;

procedure TLifetimeManagerBase.DoAfterConstruction(instance: TValue);
var
  intf: Pointer;
begin
  if TryGetInterfaceWithoutCopy(instance, IInitializable, intf) then
  begin
    IInitializable(intf).Initialize;
  end;
end;

procedure TLifetimeManagerBase.DoBeforeDestruction(instance: TValue);
var
  intf: Pointer;
begin
  if TryGetInterfaceWithoutCopy(instance, IDisposable, intf) then
  begin
    IDisposable(intf).Dispose;
  end;
end;

function TLifetimeManagerBase.GetActivator: IComponentActivator;
begin
  Result := fModel.ComponentActivator;
end;

function TLifetimeManagerBase.GetInstance: TValue;
begin
  Result := GetInstance(nil);
end;

{$ENDREGION}


{$REGION 'TSingletonLifetimeManager'}

destructor TSingletonLifetimeManager.Destroy;
begin
  if Assigned(fInstance) then
  begin
    DoBeforeDestruction(fInstance);
  end;
  inherited Destroy;
end;

function TSingletonLifetimeManager.GetInstance(resolver: IDependencyResolver): TValue;
var
  newInstance: TValue;
begin
  if not Assigned(fInstance) then
  begin
    newInstance := ComponentActivator.CreateInstance(resolver);
    fInstance := TValueHolder.Create(newInstance, Model.RefCounting);
    DoAfterConstruction(fInstance);
  end;
  Result := fInstance;
end;

procedure TSingletonLifetimeManager.ReleaseInstance(instance: TValue);
begin
end;

{$ENDREGION}


{$REGION 'TTransientLifetimeManager'}

function TTransientLifetimeManager.GetInstance(resolver: IDependencyResolver): TValue;
begin
  Result := ComponentActivator.CreateInstance(resolver);
  DoAfterConstruction(Result);
end;

procedure TTransientLifetimeManager.ReleaseInstance(instance: TValue);
begin
  TArgument.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
  if instance.IsObject then
  begin
    instance.AsObject.Free;
  end;
end;

{$ENDREGION}


{$REGION 'TSingletonPerThreadLifetimeManager'}

constructor TSingletonPerThreadLifetimeManager.Create(model: TComponentModel);
begin
  inherited Create(model);
  fInstances := TDictionary<TThreadID, TFunc<TValue>>.Create;
  fInstances.OnValueNotify := HandleValueNotify;
end;

destructor TSingletonPerThreadLifetimeManager.Destroy;
begin
  fInstances.Free;
  inherited Destroy;
end;

function TSingletonPerThreadLifetimeManager.CreateHolder(instance: TValue): TFunc<TValue>;
begin
  Result := TValueHolder.Create(instance, Model.RefCounting);
end;

procedure TSingletonPerThreadLifetimeManager.HandleValueNotify(sender: TObject;
  const item: TFunc<TValue>; action: TCollectionNotification);
begin
  if action = cnRemoved then
  begin
    DoBeforeDestruction(item);
  end;
end;

function TSingletonPerThreadLifetimeManager.GetInstance(
  resolver: IDependencyResolver): TValue;
var
  threadID: THandle;
  instance: TValue;
  holder: TFunc<TValue>;
begin
  threadID := TThread.CurrentThread.ThreadID;
  MonitorEnter(fInstances);
  try
    if not fInstances.TryGetValue(threadID, holder) then
    begin
      instance := ComponentActivator.CreateInstance(resolver);
      holder := CreateHolder(instance);
      fInstances.AddOrSetValue(threadID, holder);
      DoAfterConstruction(holder);
    end;
  finally
    MonitorExit(fInstances);
  end;
  Result := holder;
end;

procedure TSingletonPerThreadLifetimeManager.ReleaseInstance(instance: TValue);
begin
end;

{$ENDREGION}


{$REGION 'TPooledLifetimeManager'}

constructor TPooledLifetimeManager.Create(model: TComponentModel);
begin
  inherited Create(model);
  fPool := TSimpleObjectPool.Create(model.ComponentActivator, model.MinPoolsize, model.MaxPoolsize);
end;

function TPooledLifetimeManager.GetInstance(resolver: IDependencyResolver): TValue;
begin
  Result := fPool.GetInstance;
  DoAfterConstruction(Result);
end;

procedure TPooledLifetimeManager.ReleaseInstance(instance: TValue);
begin
  TArgument.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
  fPool.ReleaseInstance(instance.AsObject);
end;

{$ENDREGION}

end.
