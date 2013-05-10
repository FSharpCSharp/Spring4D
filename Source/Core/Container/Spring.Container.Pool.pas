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

///	<preliminary />
unit Spring.Container.Pool;  // experimental;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  IObjectPool = interface
    ['{E5842280-3750-46C0-8C91-0888EFFB0ED5}']
    procedure Initialize(const resolver: IDependencyResolver);
    function GetInstance(const resolver: IDependencyResolver): TObject;
    procedure ReleaseInstance(instance: TObject);
  end;

  IObjectPool<T> = interface(IObjectPool)
    function GetInstance(const resolver: IDependencyResolver): T;
    procedure ReleaseInstance(instance: T);
  end;

  IPoolableObjectFactory = interface(IComponentActivator)
    ['{56F9E805-A115-4E3A-8583-8D0B5462D98A}']
    function Validate(instance: TObject): Boolean;
    procedure Activate(instance: TObject);
    procedure Passivate(instance: TObject);
  end;

  IPoolableObjectFactory<T> = interface(IPoolableObjectFactory)
    function Validate(instance: T): Boolean;
    procedure Activate(instance: T);
    procedure Passivate(instance: T);
  end;

  TSimpleObjectPool = class(TInterfacedObject, IObjectPool)
  private
    fActivator: IComponentActivator;
    fMinPoolsize: Nullable<Integer>;
    fMaxPoolsize: Nullable<Integer>;
    fAvailableList: IQueue<TObject>;
    fActiveList: IList<TObject>;
    fInstances: IList<TObject>;
    fInitialized: Boolean;
  protected
    function AddNewInstance(const resolver: IDependencyResolver): TObject;
    function GetAvailableObject: TObject;
    property MinPoolsize: Nullable<Integer> read fMinPoolsize;
    property MaxPoolsize: Nullable<Integer> read fMaxPoolsize;
  public
    constructor Create(const activator: IComponentActivator; minPoolSize, maxPoolSize: Integer);
    procedure Initialize(const resolver: IDependencyResolver); virtual;
    function GetInstance(const resolver: IDependencyResolver): TObject; virtual;
    procedure ReleaseInstance(instance: TObject); virtual;
  end;

implementation

{ TSimpleObjectPool }

constructor TSimpleObjectPool.Create(const activator: IComponentActivator;
  minPoolSize, maxPoolSize: Integer);
begin
  inherited Create;
  fActivator := activator;
  if minPoolSize > 0 then
  begin
    fMinPoolsize := minPoolSize;
  end;
  if maxPoolSize > 0 then
  begin
    fMaxPoolsize := maxPoolSize;
  end;
  fInstances := TCollections.CreateObjectList<TObject>;
  fAvailableList := TCollections.CreateQueue<TObject>;
  fActiveList := TCollections.CreateList<TObject>;
end;

function TSimpleObjectPool.AddNewInstance(const resolver: IDependencyResolver): TObject;
begin
  Result := fActivator.CreateInstance(resolver).AsObject;
  MonitorEnter(TObject(fInstances));
  try
    fInstances.Add(Result);
  finally
    MonitorExit(TObject(fInstances));
  end;
end;

procedure TSimpleObjectPool.Initialize(const resolver: IDependencyResolver);
var
  instance: TObject;
  i: Integer;
begin
  if not fMinPoolsize.HasValue then
  begin
    Exit;
  end;
  for i := 0 to fMinPoolsize.Value - 1 do
  begin
    instance := AddNewInstance(resolver);
    fAvailableList.Enqueue(instance);
  end;
  fInitialized := True;
end;

function TSimpleObjectPool.GetAvailableObject: TObject;
begin
  Result := fAvailableList.Dequeue;
end;

function TSimpleObjectPool.GetInstance(const resolver: IDependencyResolver): TObject;
var
  instance: TObject;
begin
  if not fInitialized then
    Initialize(resolver);

  MonitorEnter(TObject(fAvailableList));
  try
    if fAvailableList.Count > 0 then
    begin
      instance := GetAvailableObject;
    end
    else
    begin
      instance := AddNewInstance(resolver);
    end;
  finally
    MonitorExit(TObject(fAvailableList));
  end;

  MonitorEnter(TObject(fActiveList));
  try
    fActiveList.Add(instance);
  finally
    MonitorExit(TObject(fActiveList));
  end;

  Result := instance;
end;

procedure TSimpleObjectPool.ReleaseInstance(instance: TObject);
begin
  TArgument.CheckNotNull(instance, 'instance');

  MonitorEnter(TObject(fActiveList));
  try
    fActiveList.Remove(instance);
  finally
    MonitorExit(TObject(fActiveList));
  end;

  MonitorEnter(TObject(fAvailableList));
  try
    if not fMaxPoolsize.HasValue or (fAvailableList.Count < fMaxPoolsize.Value) then
    begin
      fAvailableList.Enqueue(instance);
    end;
  finally
    MonitorExit(TObject(fAvailableList));
  end;
end;

end.
