{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Adapters;

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  Spring,
  Spring.DesignPatterns,
  Spring.Helpers,
  Spring.ResourceStrings;

type
  IAdaptable      = interface;
  IAdapterFactory = interface;
//  IAdapterManager = interface;

  TTypeInfoDynArray = array of PTypeInfo;

  IAdaptable = interface
    ['{1F27EA6E-168B-4CD6-BFB8-4D6D0DFF4B09}']
    function GetAdapter(adapterType: PTypeInfo): TValue;
  end;

  IAdapterFactory = interface
    ['{0182AD00-15E7-47C2-91B6-3173317F3F92}']
    function GetAdapter(const instance: TValue; adapterType: PTypeInfo): TValue;
    function GetAdapterTypes: TArray<PTypeInfo>;
  end;

  IAdapterManager<T> = interface
//    function HasAdapter(const instance: TValue): Boolean;
//    function GetAdapter(const instance: TValue): T;
//    procedure RegisterAdapters(const factory: IAdapterFactory; adaptableType: PTypeInfo);
//    procedure UnregisterAdapters(const factory: IAdapterFactory);
  end;

  TAdapterManager = class
  private
    type
      TLookupKey = record
        AdaptableType: PTypeInfo;
        AdapterType: PTypeInfo;
      end;
  private
    fDictionary: TDictionary<PTypeInfo, TList<IAdapterFactory>>;
    fLookup: TDictionary<TLookupKey, IAdapterFactory>;
    procedure FlushLookup;
    procedure BuildLookupTable(const adaptableTypes: TTypeInfoDynArray;
      lookupTable: TDictionary<TLookupKey, IAdapterFactory>);
    function GetFactory(adaptableType, adapterType: PTypeInfo): IAdapterFactory;
  public
    constructor Create;
    destructor Destroy; override;
    function HasAdapter<TAdapter: IInterface>(const adaptableObject: TObject): Boolean; overload;
    function HasAdapter<TAdapter: IInterface>(const adaptableObject; adaptableType: PTypeInfo): Boolean; overload;
//    function TryGetAdapter<TAdapter>(const instance: TValue; out adapter: TAdapter): Boolean; overload;
//    function TryGetAdapter(const instance: TValue; adapterType: PTypeInfo; out adapter: TValue): Boolean; overload;
    function GetAdapter<TAdapter: IInterface>(const adaptableObject: TObject): TAdapter; overload;
    function GetAdapter<TAdapter: IInterface>(const adaptableObject; adaptableType: PTypeInfo): TAdapter; overload;
    function ComputerAdapterTypes(typeInfo: PTypeInfo): TTypeInfoDynArray;
    procedure RegisterAdapters(const factory: IAdapterFactory; adaptableType: PTypeInfo);
    procedure UnregisterAdapters(const factory: IAdapterFactory); overload;
    procedure UnregisterAdapters(const factory: IAdapterFactory; adaptableType: PTypeInfo); overload;
    procedure UnregisterAll;
  end;

  EAdapterException = class(Exception);

function TryGetAdapter(const instance: TValue; adapterType: PTypeInfo; out adapter: TValue): Boolean;

implementation

function TryGetAdapter(const instance: TValue; adapterType: PTypeInfo; out adapter: TValue): Boolean;
begin
  Result := False;
end;

{ TAdapterManager }

constructor TAdapterManager.Create;
begin
  inherited;
  fDictionary := TObjectDictionary<PTypeInfo, TList<IAdapterFactory>>.Create([doOwnsValues]);
end;

destructor TAdapterManager.Destroy;
begin
  fDictionary.Free;
  inherited;
end;

function TAdapterManager.GetAdapter<TAdapter>(
  const adaptableObject: TObject): TAdapter;
begin
  Result := GetAdapter<TAdapter>(adaptableObject, adaptableObject.ClassInfo);
end;

function TAdapterManager.HasAdapter<TAdapter>(
  const adaptableObject: TObject): Boolean;
begin
  Result := HasAdapter<TAdapter>(adaptableObject, adaptableObject.ClassInfo);
end;

function TAdapterManager.HasAdapter<TAdapter>(const adaptableObject;
  adaptableType: PTypeInfo): Boolean;
var
  adapter: TAdapter;
begin
  adapter := GetAdapter<TAdapter>(adaptableObject, adaptableType);
  Result := adapter <> nil;
end;

function TAdapterManager.GetAdapter<TAdapter>(const adaptableObject;
  adaptableType: PTypeInfo): TAdapter;
var
  factory: IAdapterFactory;
  adapterType: PTypeInfo;
  intf: IInterface;
  guid: TGuid;
begin
  TArgument.CheckNotNull(adaptableType, 'adaptableType');
  adapterType := TypeInfo(TAdapter);
  factory := GetFactory(adaptableType, adapterType);
  if factory = nil then
  begin
    Exit(Default(TAdapter));
  end;
  intf := factory.GetAdapter(adaptableObject, adapterType);
  guid := GetTypeData(TypeInfo(TAdapter)).Guid;
  if guid.IsEmpty then
  begin
    IInterface(Result) := intf; // *DANGER* { TODO: NOT Supported Exception? }
  end
  else if intf.QueryInterface(guid, Result) <> S_OK then
  begin
    Exit(Default(TAdapter));
  end;
end;

procedure TAdapterManager.BuildLookupTable(const adaptableTypes: TTypeInfoDynArray;
  lookupTable: TDictionary<TLookupKey, IAdapterFactory>);
var
  factories: TList<IAdapterFactory>;
  adaptableTye: PTypeInfo;
  key: TLookupKey;
begin
  Assert(lookupTable <> nil, 'lookupTable must not be nil.');
  for adaptableTye in adaptableTypes do
  begin
    Lock(fDictionary,
      procedure
      begin
        if not fDictionary.TryGetValue(adaptableTye, factories) then
          Exit;
        Lock(factories,
          procedure
          var
            factory: IAdapterFactory;
            adapterTypes: TTypeInfoDynArray;
            adapterType: PTypeInfo;
          begin
            for factory in factories do
            begin
              adapterTypes := factory.GetAdapterTypes;
              for adapterType in adapterTypes do
              begin
                key.AdaptableType := adaptableTye;
                key.AdapterType := adapterType;
                Lock(lookupTable,
                  procedure
                  begin
                    lookupTable.AddOrSetValue(key, factory);
                  end
                );
              end;
            end;
          end
        );
      end
    );
  end;
end;

function TAdapterManager.GetFactory(adaptableType,
  adapterType: PTypeInfo): IAdapterFactory;
var
  factory: IAdapterFactory;
  key: TLookupKey;
  adaptableTypes: TTypeInfoDynArray;

begin
  key.AdaptableType := adaptableType;
  key.AdapterType := adapterType;
  if fLookup <> nil then
  begin
    Lock(fLookup,
      procedure
      begin
        if fLookup.TryGetValue(key, factory) then
          Exit;
      end
    );
    if factory <> nil then Exit;
  end;
  adaptableTypes := ComputerAdapterTypes(adaptableType);
  if fLookup = nil then
  begin
    fLookup := TDictionary<TLookupKey, IAdapterFactory>.Create(16);
  end;
  BuildLookupTable(adaptableTypes, fLookup);
  key.AdapterType := adapterType;
  Lock(fLookup,
    procedure
    var
      aType: PTypeInfo;
    begin
      for aType in adaptableTypes do
      begin
        key.AdaptableType := aType;
        if fLookup.TryGetValue(key, factory) then
          Exit;
      end;
    end
  );
  Result := factory;
end;

function TAdapterManager.ComputerAdapterTypes(
  typeInfo: PTypeInfo): TTypeInfoDynArray;
var
  list: TList<PTypeInfo>;
  typeData: PTypeData;
  i: Integer;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  list := TList<PTypeInfo>.Create;
  try
    while typeInfo <> nil do
    begin
      list.Add(typeInfo);
      case typeInfo.Kind of
        tkClass:
        begin
          typeData := GetTypeData(typeInfo);
          if typeData.ParentInfo <> nil then
          begin
            typeInfo := typeData.ParentInfo^;
          end
          else
          begin
            typeInfo := nil;
          end;
        end;
        tkInterface:
        begin
          typeData := GetTypeData(typeInfo);
          if typeData.IntfParent <> nil then
          begin
            typeInfo := typeData.IntfParent^;
          end
          else
          begin
            typeInfo := nil;
          end;
        end
        else
        begin
          typeInfo := nil;
        end;
      end;
    end;
    SetLength(Result, list.Count);
    for i := 0 to list.Count - 1 do
    begin
      Result[i] := list[i];
    end;
  finally
    list.Free;
  end;
end;

procedure TAdapterManager.RegisterAdapters(const factory: IAdapterFactory;
  adaptableType: PTypeInfo);
var
  list: TList<IAdapterFactory>;
begin
  TArgument.CheckNotNull(factory, 'factory');
  TArgument.CheckNotNull(adaptableType, 'adaptableType');
  Assert(fDictionary <> nil, 'fDictionary should not be nil.');
  Lock(fDictionary,
    procedure
    begin
      if not fDictionary.TryGetValue(adaptableType, list) then
      begin
        list := TList<IAdapterFactory>.Create;
        fDictionary.Add(adaptableType, list);
      end;
    end
  );
  Lock(list,
    procedure
    begin
      if not list.Contains(factory) then
        list.Add(factory);
    end
  );
  FlushLookup;
end;

procedure TAdapterManager.UnregisterAdapters(const factory: IAdapterFactory);
begin
  TArgument.CheckNotNull(factory <> nil, 'factory');
  Lock(fDictionary,
    procedure
    var
      factories: TList<IAdapterFactory>;
    begin
      for factories in fDictionary.Values do
      begin
        Lock(factories,
          procedure
          begin
            factories.Remove(factory);
          end
        );
      end;
    end
  );
  FlushLookup;
end;

procedure TAdapterManager.UnregisterAdapters(const factory: IAdapterFactory;
  adaptableType: PTypeInfo);
var
  factories: TList<IAdapterFactory>;
begin
  TArgument.CheckNotNull(factory, 'factory');
  TArgument.CheckNotNull(adaptableType, 'adaptableType');
  Lock(fDictionary,
    procedure
    begin
      if not fDictionary.TryGetValue(adaptableType, factories) then
      begin
        factories := nil;
      end;
    end
  );
  if factories <> nil then
  begin
    Lock(factories,
      procedure
      begin
        factories.Remove(factory);
      end
    );
  end;
  FlushLookup;
end;

procedure TAdapterManager.UnregisterAll;
begin
  fDictionary.Clear;
  FlushLookup;
end;

procedure TAdapterManager.FlushLookup;
begin
  FreeAndNil(fLookup);
end;

end.
