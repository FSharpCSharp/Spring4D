{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

unit Spring.DI.LifetimeManager;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Generics.Collections,
  Spring,
  Spring.Utils,
  Spring.DI.Core,
  Spring.Pool;

type
  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager, IInterface)
  private
    fModel: TComponentModel;
    function GetActivator: IComponentActivator;
  protected
    procedure DoAfterConstruction(instance: TObject); virtual;
    procedure DoBeforeDestruction(instance: TObject); virtual;
  protected
    function TryGetInterfaceWithoutCopy(instance: TObject; const IID: TGUID; out intf): Boolean;
    property ComponentActivator: IComponentActivator read GetActivator;
    property Model: TComponentModel read fModel;
  public
    constructor Create(model: TComponentModel);
    function GetInstance: TObject; virtual; abstract;
    procedure ReleaseInstance(instance: TObject); virtual; abstract;
  end;

  TSingletonLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstance: TFunc<TObject>;
  public
    destructor Destroy; override;
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

  TTransientLifetimeManager = class(TLifetimeManagerBase)
  public
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

  TSingletonPerThreadLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstances: TDictionary<TThreadID, TFunc<TObject>>;
  protected
    procedure HandleValueNotify(sender: TObject; const item: TFunc<TObject>; action: TCollectionNotification);
    function CreateHolder(instance: TObject): TFunc<TObject>; virtual;
  public
    constructor Create(model: TComponentModel);
    destructor Destroy; override;
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

  TPooledLifetimeManager = class(TLifetimeManagerBase)
  private
    fPool: IObjectPool;
  public
    constructor Create(model: TComponentModel);
    function GetInstance: TObject; override;
    procedure ReleaseInstance(instance: TObject); override;
  end;

implementation


{$REGION 'TLifetimeManagerBase'}

constructor TLifetimeManagerBase.Create(model: TComponentModel);
begin
  TArgument.CheckNotNull(model, 'model');
  inherited Create;
  fModel := model;
end;

type
  /// <summary>
  /// Provides access to the protected fRefCount field of TInterfacedObject.
  /// </summary>
  TInterfacedObjectHack = class(TInterfacedObject);

/// <remarks>
/// The IInterface._AddRef method will be automatically called by the compiler
/// when TObject.GetInterface method was invoked. So we should decrease the protected
/// fRefCount of an TInterfacedObject instance.
/// </remarks>
function TLifetimeManagerBase.TryGetInterfaceWithoutCopy(instance: TObject;
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
  if TryGetInterfaceWithoutCopy(instance, IInitializable, intf) then
  begin
    IInitializable(intf).Initialize;
  end;
end;

procedure TLifetimeManagerBase.DoBeforeDestruction(instance: TObject);
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

function TSingletonLifetimeManager.GetInstance: TObject;
var
  newInstance: TObject;
  localInstance: TFunc<TObject>;
begin
  if not Assigned(fInstance) then
  begin
    newInstance := ComponentActivator.CreateInstance;
    localInstance := TObjectHolder<TObject>.Create(newInstance);
    if InterlockedCompareExchangePointer(PPointer(@fInstance)^, PPointer(@localInstance)^, nil) = nil then
    begin
      IInterface(PPointer(@fInstance)^)._AddRef;
      DoAfterConstruction(fInstance);
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


{$REGION 'TSingletonPerThreadLifetimeManager'}

constructor TSingletonPerThreadLifetimeManager.Create(model: TComponentModel);
begin
  inherited Create(model);
  fInstances := TDictionary<THandle, TFunc<TObject>>.Create;
  fInstances.OnValueNotify := HandleValueNotify;
end;

destructor TSingletonPerThreadLifetimeManager.Destroy;
begin
  fInstances.Free;
  inherited Destroy;
end;

function TSingletonPerThreadLifetimeManager.CreateHolder(instance: TObject): TFunc<TObject>;
begin
  Result := TObjectHolder<TObject>.Create(instance);
end;

procedure TSingletonPerThreadLifetimeManager.HandleValueNotify(sender: TObject;
  const item: TFunc<TObject>; action: TCollectionNotification);
begin
  if action = cnRemoved then
  begin
    DoBeforeDestruction(item);
  end;
end;

function TSingletonPerThreadLifetimeManager.GetInstance: TObject;
var
  threadID: THandle;
  instance: TObject;
  holder: TFunc<TObject>;
begin
  threadID := GetCurrentThreadId;
  Lock(fInstances,
    procedure
    begin
      if not fInstances.TryGetValue(threadID, holder) then
      begin
        instance := ComponentActivator.CreateInstance;
        holder := CreateHolder(instance);
        fInstances.AddOrSetValue(threadID, holder);
        DoAfterConstruction(holder);
      end;
    end
  );
  Result := holder;
end;

procedure TSingletonPerThreadLifetimeManager.ReleaseInstance(instance: TObject);
begin
end;

{$ENDREGION}


{$REGION 'TPooledLifetimeManager'}

constructor TPooledLifetimeManager.Create(model: TComponentModel);
begin
  inherited Create(model);
  fPool := TSimpleObjectPool.Create(model.ComponentActivator, model.MinPoolsize, model.MaxPoolsize);
end;

function TPooledLifetimeManager.GetInstance: TObject;
begin
  Result := fPool.GetInstance;
  DoAfterConstruction(Result);
end;

procedure TPooledLifetimeManager.ReleaseInstance(instance: TObject);
begin
  TArgument.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
  fPool.ReleaseInstance(instance);
end;

{$ENDREGION}

end.
