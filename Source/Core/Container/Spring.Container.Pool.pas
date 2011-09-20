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

/// <preliminary />
unit Spring.Container.Pool;  // experimental;

{$I Spring.inc}

interface

uses
//  Classes,
  SysUtils,
  Types,
  TypInfo,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Reflection;

type
  IObjectPool = interface
    ['{E5842280-3750-46C0-8C91-0888EFFB0ED5}']
    function GetInstance: TObject;
    procedure ReleaseInstance(instance: TObject);
  end;

  IObjectPool<T> = interface(IObjectPool)
    function GetInstance: T;
    procedure ReleaseInstance(instance: T);
  end;

  IPoolableObjectFactory = interface(IObjectActivator)
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
    fActivator: IObjectActivator;
    fMinPoolsize: TNullable<Integer>;
    fMaxPoolsize: TNullable<Integer>;
    fAvailableList: IQueue<TObject>;
    fActiveList: IList<TObject>;
    fInstances: IList<TObject>;
  protected
    procedure InitializePool; virtual;
    function AddNewInstance: TObject;
    function GetAvailableObject: TObject;
    property MinPoolsize: TNullable<Integer> read fMinPoolsize;
    property MaxPoolsize: TNullable<Integer> read fMaxPoolsize;
  public
    constructor Create(const activator: IObjectActivator; minPoolSize, maxPoolSize: Integer);
    destructor Destroy; override;
    function GetInstance: TObject; virtual;
    procedure ReleaseInstance(instance: TObject); virtual;
  end;

implementation

uses
  Spring.Utils;

{ TSimpleObjectPool }

constructor TSimpleObjectPool.Create(const activator: IObjectActivator;
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
  fInstances := TObjectList<TObject>.Create;
  fAvailableList := TQueue<TObject>.Create;
  fActiveList := TList<TObject>.Create;
  InitializePool;
end;

destructor TSimpleObjectPool.Destroy;
begin
//  fAvailableList.Free;
//  fInstances.Free;
  inherited Destroy;
end;

function TSimpleObjectPool.AddNewInstance: TObject;
begin
  Result := fActivator.CreateInstance;
  MonitorEnter(TObject(fInstances));
  try
    fInstances.Add(Result);
  finally
    MonitorExit(TObject(fInstances));
  end;
end;

procedure TSimpleObjectPool.InitializePool;
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
    instance := AddNewInstance;
    fAvailableList.Enqueue(instance);
  end;
end;

function TSimpleObjectPool.GetAvailableObject: TObject;
begin
  Result := fAvailableList.Dequeue;
end;

function TSimpleObjectPool.GetInstance: TObject;
var
  instance: TObject;
begin
  Lock(fAvailableList,
    procedure
    begin
      if fAvailableList.Count > 0 then
      begin
        instance := GetAvailableObject;
      end
      else
      begin
        instance := AddNewInstance;
      end;
    end
  );
  Lock(fActiveList,
    procedure
    begin
      fActiveList.Add(instance);
    end
  );
  Result := instance;
end;

procedure TSimpleObjectPool.ReleaseInstance(instance: TObject);
begin
  TArgument.CheckNotNull(instance, 'instance');

  Lock(fActiveList,
    procedure
    begin
      fActiveList.Remove(instance);
    end
  );
  Lock(fAvailableList,
    procedure
    begin
      if not fMaxPoolsize.HasValue or (fAvailableList.Count < fMaxPoolsize.Value) then
      begin
        fAvailableList.Enqueue(instance);
      end;
    end
  );
end;

end.
