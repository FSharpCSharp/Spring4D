{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Core.Types;

{$I Spring.inc}

interface

uses
  SysUtils,
  Spring,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Mapping.Attributes;

type
  /// <summary>
  /// Generic enumeration type
  /// <remarks>
  /// Should be used only for enumerations!
  /// </remarks>
  /// </summary>
  TEnum<T> = record
  private
    FValue: NativeInt;
  private
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    class operator Implicit(const AEnum: TEnum<T>): T; inline;
    class operator Implicit(const AEnum: T): TEnum<T>; inline;
    class operator Implicit(const AEnum: TEnum<T>): Integer; inline;
    class operator Implicit(const AEnum: Integer): TEnum<T>; inline;

    function ToString: string;

    property Value: T read GetValue write SetValue;
  end;

  ISvLazy<T> = interface(IInvokable)
    ['{433C34BB-E5F3-4594-814C-70F02C415CB8}']
    function OwnsObjects: Boolean;
    function ValueCreated: Boolean;
    procedure SetValue(const AValue: T);
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property Value: T read GetValue;
    function GetManager: TSession;
    procedure SetManager(const Value: TSession);
    property Manager: TSession read GetManager write SetManager;
    function GetID: TValue;
    procedure SetID(const Value: TValue);
    property ID: TValue read GetID write SetID;
    function GetEntity: TObject;
    procedure SetEntity(const Value: TObject);
    property Entity: TObject read GetEntity write SetEntity;
    function GetColumn: ColumnAttribute;
    procedure SetColumn(const Value: ColumnAttribute);
    property EntityColumn: ColumnAttribute read GetColumn write SetColumn;
    function GetRttiMember: string;
    procedure SetRttiMember(const Value: string);
    property RttiMember: string read GetRttiMember write SetRttiMember;
  end;

  ISvLazyObject<T: class, constructor> = interface(ISvLazy<T>)
    ['{A5749874-ABD7-45F3-A9E9-4EB9826C0329}']
  end;

  TLazyVarData = record
    ID: TValue;
    Manager: TSession;
    Entity: TObject;
    RttiMemberName: string;
    Column: ColumnAttribute;
  end;

  TSvLazy<T> = class(TInterfacedObject, ISvLazy<T>)
  private
    FValueFactory: TFunc<T>;
    FValueCreated: Boolean;
    FOwnsObjects: Boolean;
    FValue: T;
    FDisableCache: Boolean;
    FVarData: TLazyVarData;
    function GetValue: T;
    function GetManager: TSession;
    procedure SetManager(const Value: TSession);
    function GetID: TValue;
    procedure SetID(const Value: TValue);
    function GetEntity: TObject;
    procedure SetEntity(const Value: TObject);
    function GetColumn: ColumnAttribute;
    procedure SetColumn(const Value: ColumnAttribute);
    function GetRttiMember: string;
    procedure SetRttiMember(const Value: string);
  protected
    procedure SetValue(const AValue: T);
    function OwnsObjects: Boolean;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    function DoGetValue: T; virtual;
    procedure CheckInitialized;
  public
    constructor Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean = False); overload;
    constructor Create(AOwnsObjects: Boolean = False); overload;
    constructor Create(AValue: T; AOwnsObjects: Boolean = False); overload;
    destructor Destroy; override;

    function ValueCreated: Boolean;

    property EntityColumn: ColumnAttribute read GetColumn write SetColumn;
    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property ID: TValue read GetID write SetID;
    property Entity: TObject read GetEntity write SetEntity;
    property Manager: TSession read GetManager write SetManager;
    property RttiMember: string read GetRttiMember write SetRttiMember;
    property Value: T read GetValue;
  end;

  TSvLazyObject<T: class, constructor> = class(TSvLazy<T>, ISvLazyObject<T>)
  protected
    function DoGetValue: T; override;
  end;

  /// <summary>
  /// Represents lazy variable.
  /// </summary>
  Lazy<T> = record
  private
    FLazy: ISvLazy<T>;
    FVarData: TLazyVarData;
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    procedure SetManager(const Value: TSession);
    function GetManager: TSession;
    function GetID: TValue;
    procedure SetID(const Value: TValue);
  public
    /// <summary>
    /// Initializes lazy value with anonymous getValue factory
    /// </summary>
    /// <param name="AValueFactory">Anonymous method which gets value</param>
    /// <param name="AOwnsObjects">Boolean property to specify if lazy value should free it's value when it goes out of scope</param>
    class function Create(const AValue: T): Lazy<T>; static;

    procedure Assign(const AValue: T);
    function IsValueCreated: Boolean;

    class operator Implicit(const ALazy: Lazy<T>): T; inline;
    class operator Implicit(const AValue: T): Lazy<T>; inline;

    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property ID: TValue read GetID write SetID;
    property Manager: TSession read GetManager write SetManager;
    property Value: T read GetValue;
  end;

  LazyObject<T: class, constructor> = record
  private
    FLazy: ISvLazyObject<T>;
    FVarData: TLazyVarData;
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
  public
    /// <summary>
    /// Initializes lazy value with anonymous getValue factory
    /// </summary>
    /// <param name="AValueFactory">Anonymous method which gets value</param>
    /// <param name="AOwnsObjects">Boolean property to specify if lazy value should free it's value when it goes out of scope</param>
    constructor Create(const AValue: T);

    procedure Assign(const AValue: T);
    function IsValueCreated: Boolean;

    class operator Implicit(const ALazy: LazyObject<T>): T; inline;
    class operator Implicit(const AValue: T): LazyObject<T>; inline;

    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property Value: T read GetValue;
  end;

implementation

uses
  TypInfo,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Mapping.RttiExplorer;

{ TEnum<T> }

function TEnum<T>.GetValue: T;
begin
  Result := TValue.FromOrdinal(TypeInfo(T), FValue).AsType<T>;
end;

class operator TEnum<T>.Implicit(const AEnum: T): TEnum<T>;
begin
  Result.Value := AEnum;
end;

class operator TEnum<T>.Implicit(const AEnum: TEnum<T>): T;
begin
  Result := AEnum.Value;
end;

class operator TEnum<T>.Implicit(const AEnum: Integer): TEnum<T>;
begin
  Result.FValue := AEnum;
end;

class operator TEnum<T>.Implicit(const AEnum: TEnum<T>): Integer;
begin
  Result := AEnum.FValue;
end;

procedure TEnum<T>.SetValue(const Value: T);
var
  AValue: TValue;
begin
  AValue := TValue.From<T>(Value);
  if AValue.Kind = tkEnumeration then
    FValue := AValue.AsOrdinal
  else
    raise EORMEnumException.Create('Incorrect value type. Can set only enumerations.');
end;

function TEnum<T>.ToString: string;
begin
  Result := GetEnumName(TypeInfo(T), FValue);
end;

{ TSvLazy<T> }

constructor TSvLazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean);
begin
  inherited Create;
  FValueFactory := AValueFactory;
  FValueCreated := False;
  FOwnsObjects := AOwnsObjects;
  FVarData.Manager := nil;
end;

constructor TSvLazy<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FValueCreated := False;
  FOwnsObjects := AOwnsObjects;
  FVarData.Manager := nil;
  FVarData.Entity := nil;
  FVarData.Column := nil;
  FVarData.RttiMemberName := '';
end;

procedure TSvLazy<T>.CheckInitialized;
var
  LValue: TValue;
  LObj: TObject;
begin
  LValue := TValue.From<T>(FValue);
  if LValue.IsObject then
  begin
    LObj := LValue.AsObject;
    if not Assigned(LObj) then
    begin
      FValue := TRttiExplorer.CreateNewClass<T>;
    end;
  end;
end;

constructor TSvLazy<T>.Create(AValue: T; AOwnsObjects: Boolean);
begin
  Create(AOwnsObjects);
  FValue := AValue;
end;

destructor TSvLazy<T>.Destroy;
begin
  if FOwnsObjects and FValueCreated then
  begin
    //free value
    TRttiExplorer.DestroyClass<T>(FValue);
  end;
  inherited Destroy;
end;

function TSvLazy<T>.DoGetValue: T;
begin
  //check if FValue needs initialization
  CheckInitialized;
  FVarData.Manager.SetLazyValue<T>(FValue, FVarData.ID, FVarData.Entity, GetColumn);
  Result := FValue;
end;

function TSvLazy<T>.GetColumn: ColumnAttribute;
begin
  if (FVarData.Column = nil) and (FVarData.Entity <> nil) then
  begin
    FVarData.Column := TEntityCache.Get(FVarData.Entity.ClassType).ColumnByMemberName(FVarData.RttiMemberName);
  end;
  Result := FVarData.Column;
end;

function TSvLazy<T>.GetDisableCache: Boolean;
begin
  Result := FDisableCache;
end;

function TSvLazy<T>.GetEntity: TObject;
begin
  Result := FVarData.Entity;
end;

function TSvLazy<T>.GetID: TValue;
begin
  Result := FVarData.ID;
end;

function TSvLazy<T>.GetManager: TSession;
begin
  Result := FVarData.Manager;
end;

function TSvLazy<T>.GetRttiMember: string;
begin
  Result := FVarData.RttiMemberName;
end;

function TSvLazy<T>.GetValue: T;
begin
  if (not FValueCreated) or FDisableCache then
  begin
    FValue := DoGetValue;
    CheckInitialized;
    FValueCreated := True;
  end;

  FDisableCache := False;
  Result := FValue;
end;

function TSvLazy<T>.OwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TSvLazy<T>.SetColumn(const Value: ColumnAttribute);
begin
  FVarData.Column := Value;
end;

procedure TSvLazy<T>.SetDisableCache(const Value: Boolean);
begin
  FDisableCache := Value;
end;

procedure TSvLazy<T>.SetEntity(const Value: TObject);
begin
  FVarData.Entity := Value;
end;

procedure TSvLazy<T>.SetID(const Value: TValue);
begin
  FVarData.ID := Value;
end;

procedure TSvLazy<T>.SetManager(const Value: TSession);
begin
  FVarData.Manager := Value;
end;

procedure TSvLazy<T>.SetRttiMember(const Value: string);
begin
  FVarData.RttiMemberName := Value;
end;

procedure TSvLazy<T>.SetValue(const AValue: T);
begin
  FValueCreated := True;
  FValue := AValue;
end;

function TSvLazy<T>.ValueCreated: Boolean;
begin
  Result := FValueCreated;
end;

{ SvLazy<T> }

procedure Lazy<T>.Assign(const AValue: T);
begin
  if Assigned(FLazy) then
  begin
    FLazy.SetValue(AValue);
  end
  else
  begin
    Create(AValue);
  end;
end;

class function Lazy<T>.Create(const AValue: T): Lazy<T>;
begin
  Result.FLazy := TSvLazy<T>.Create(AValue, False);
end;

function Lazy<T>.GetDisableCache: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
  begin
    Result := FLazy.DisableCacheForNextGet;
  end;
end;

function Lazy<T>.GetID: TValue;
begin
  Result := TValue.Empty;
  if Assigned(FLazy) then
    Result := FLazy.ID;
end;

function Lazy<T>.GetManager: TSession;
begin
  Result := nil;
  if Assigned(FLazy) then
    Result := FLazy.Manager;
end;

function Lazy<T>.GetValue: T;
begin
  if not Assigned(FLazy) then
  begin
    FLazy := TSvLazy<T>.Create(False);
  end;
  FLazy.Manager := FVarData.Manager;
  FLazy.ID := FVarData.ID;
  FLazy.Entity := FVarData.Entity;
  FLazy.EntityColumn := FVarData.Column;
  FLazy.RttiMember := FVarData.RttiMemberName;
  Result := FLazy.Value;
end;

class operator Lazy<T>.Implicit(const AValue: T): Lazy<T>;
begin
  Result := Create(AValue);
end;

function Lazy<T>.IsValueCreated: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
    Result := FLazy.ValueCreated;
end;

procedure Lazy<T>.SetDisableCache(const Value: Boolean);
begin
  Assert(Assigned(FLazy));
  FLazy.DisableCacheForNextGet := Value;
end;

procedure Lazy<T>.SetID(const Value: TValue);
begin
  if Assigned(FLazy) then
    FLazy.ID := Value;
end;

procedure Lazy<T>.SetManager(const Value: TSession);
begin
  if Assigned(FLazy) then
    FLazy.Manager := Value;
end;

class operator Lazy<T>.Implicit(const ALazy: Lazy<T>): T;
begin
  Result := ALazy.Value;
end;

{ LazyObject<T> }

procedure LazyObject<T>.Assign(const AValue: T);
var
  oldValue: T;
begin
  if Assigned(FLazy) then
  begin
    if FLazy.ValueCreated then
    begin
      oldValue := FLazy.Value;
      TRttiExplorer.DestroyClass<T>(oldValue);
    end;

    FLazy.SetValue(AValue);
  end
  else
  begin
    FLazy := TSvLazyObject<T>.Create(AValue);
  end;
end;

constructor LazyObject<T>.Create(const AValue: T);
begin
  FLazy := TSvLazyObject<T>.Create(AValue, True);
end;

function LazyObject<T>.GetDisableCache: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
  begin
    Result := FLazy.DisableCacheForNextGet;
  end;
end;

function LazyObject<T>.GetValue: T;
begin
  if not Assigned(FLazy) then
  begin
    FLazy := TSvLazyObject<T>.Create(True);
  end;
  FLazy.Manager := FVarData.Manager;
  FLazy.ID := FVarData.ID;
  FLazy.Entity := FVarData.Entity;
  FLazy.RttiMember := FVarData.RttiMemberName;
  FLazy.EntityColumn := FVarData.Column;
  Result := FLazy.Value;
end;

class operator LazyObject<T>.Implicit(const ALazy: LazyObject<T>): T;
begin
  Result := ALazy.Value;
end;

class operator LazyObject<T>.Implicit(const AValue: T): LazyObject<T>;
begin
  Result := LazyObject<T>.Create(AValue);
end;

function LazyObject<T>.IsValueCreated: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
    Result := FLazy.ValueCreated;
end;

procedure LazyObject<T>.SetDisableCache(const Value: Boolean);
begin
  Assert(Assigned(FLazy));
  FLazy.DisableCacheForNextGet := Value;
end;

{ TSvLazyObject<T> }

function TSvLazyObject<T>.DoGetValue: T;
begin
  Result := FVarData.Manager.GetLazyValueClass<T>(FVarData.ID, FVarData.Entity, GetColumn);
end;

end.
