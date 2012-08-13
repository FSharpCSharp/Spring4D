(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Core.Types;

interface

uses
  Rtti, Core.EntityManager, SysUtils, Mapping.Attributes;

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

    function ToString(): string;

    property Value: T read GetValue write SetValue;
  end;

  /// <summary>
  /// Nullable type
  /// </summary>
 // {$TYPEINFO ON}
  Nullable<T> = record
  private
    FValue: T;
    FHasValue: Boolean;
    function GetValue: T;
    procedure SetValue(const Value: T);
    function GetIsNull: Boolean;
    function GetValueOrDef: T;
  public
    constructor Create(const AValue: T);

    function ToString(): string;

    property HasValue: Boolean read FHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    property ValueOrDef: T read GetValueOrDef;

    class operator Implicit(const Value: T): Nullable<T>;
    class operator Implicit(const Value: Nullable<T>): T;

    class operator Equal(const Left, Right: Nullable<T>): Boolean;
    class operator NotEqual(const Left, Right: Nullable<T>): Boolean;

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
    function GetManager: TEntityManager;
    procedure SetManager(const Value: TEntityManager);
    property Manager: TEntityManager read GetManager write SetManager;
    function GetID: Variant;
    procedure SetID(const Value: Variant);
    property ID: Variant read GetID write SetID;
    function GetEntity: TObject;
    procedure SetEntity(const Value: TObject);
    property Entity: TObject read GetEntity write SetEntity;
    function GetColumn: Column;
    procedure SetColumn(const Value: Column);
    property EntityColumn: Column read GetColumn write SetColumn;
  end;

  ISvLazyObject<T: class, constructor> = interface(ISvLazy<T>)
    ['{A5749874-ABD7-45F3-A9E9-4EB9826C0329}']
  end;

  TSvLazy<T> = class(TInterfacedObject, ISvLazy<T>)
  private
    FValueFactory: TFunc<T>;
    FValueCreated: Boolean;
    FOwnsObjects: Boolean;
    FValue: T;
    FDisableCache: Boolean;
    FID: Variant;
    FManager: TEntityManager;
    FEntity: TObject;
    FColumn: Column;
    function GetValue: T;
    function GetManager: TEntityManager;
    procedure SetManager(const Value: TEntityManager);
    function GetID: Variant;
    procedure SetID(const Value: Variant);
    function GetEntity: TObject;
    procedure SetEntity(const Value: TObject);
    function GetColumn: Column;
    procedure SetColumn(const Value: Column);
  protected
    procedure SetValue(const AValue: T);
    function OwnsObjects: Boolean;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    function DoGetValue(): T; virtual;
    procedure CheckInitialized();
  public
    constructor Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean = False); overload;
    constructor Create(AOwnsObjects: Boolean = False); overload;
    constructor Create(AValue: T; AOwnsObjects: Boolean = False); overload;
    destructor Destroy; override;

    function ValueCreated: Boolean;

    property EntityColumn: Column read GetColumn write SetColumn;
    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property ID: Variant read GetID write SetID;
    property Entity: TObject read GetEntity write SetEntity;
    property Manager: TEntityManager read GetManager write SetManager;
    property Value: T read GetValue;
  end;

  TSvLazyObject<T: class, constructor> = class(TSvLazy<T>, ISvLazyObject<T>)
  protected
    function DoGetValue(): T; override;
  end;
  /// <summary>
  /// Represents lazy variable.
  /// </summary>
  {DONE -oLinas -cGeneral : Make work without class type constraint}
  Lazy<T> = record
  private
    FLazy: ISvLazy<T>;
    FManager: TEntityManager;
    FID: Variant;
    FEntity: TObject;
    FColumn: Column;
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    procedure SetManager(const Value: TEntityManager);
    function GetManager: TEntityManager;
    function GetID: Variant;
    procedure SetID(const Value: Variant);
  public
    /// <summary>
    /// Initializes lazy value with anonymous getValue factory
    /// </summary>
    /// <param name="AValueFactory">Anonymous method which gets value</param>
    /// <param name="AOwnsObjects">Boolean property to specify if lazy value should free it's value when it goes out of scope</param>
    constructor Create(AValue: T);

    procedure Assign(const AValue: T);
    function IsValueCreated: Boolean;

    class operator Implicit(const ALazy: Lazy<T>): T; inline;
    class operator Implicit(const AValue: T): Lazy<T>; inline;

    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property ID: Variant read GetID write SetID;
    property Manager: TEntityManager read GetManager write SetManager;
    property Value: T read GetValue;
  end;

  LazyObject<T: class, constructor> = record
  private
    FLazy: ISvLazyObject<T>;
    FManager: TEntityManager;
    FID: Variant;
    FEntity: TObject;
    FColumn: Column;
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

 // {$TYPEINFO OFF}

implementation

uses
  TypInfo,
  Core.Exceptions,
  Mapping.RttiExplorer
  ,Core.Reflection
  ,Core.Utils
  ,Variants
  ;

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

{ Nullable<T> }

constructor Nullable<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

class operator Nullable<T>.Equal(const Left, Right: Nullable<T>): Boolean;
var
  LLeft, LRight: TValue;
begin
  LLeft := TValue.From<T>(Left);
  LRight := TValue.From<T>(Right);

  Result := SameValue(LLeft, LRight);
//  Result := LLeft.IsSameAs(LRight);
end;

function Nullable<T>.GetIsNull: Boolean;
begin
  Result := not (FHasValue) ;
end;

function Nullable<T>.GetValue: T;
begin
  Result := FValue;
end;

function Nullable<T>.GetValueOrDef: T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := System.Default(T);
end;

class operator Nullable<T>.Implicit(const Value: T): Nullable<T>;
begin
  Result.Value := Value;
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.NotEqual(const Left, Right: Nullable<T>): Boolean;
var
  LLeft, LRight: TValue;
begin
  LLeft := TValue.From<T>(Left);
  LRight := TValue.From<T>(Right);

  Result := not SameValue(LLeft, LRight); // LLeft.IsSameAs(LRight);
end;

procedure Nullable<T>.SetValue(const Value: T);
begin
  FValue := Value;
  FHasValue := True;
end;

function Nullable<T>.ToString: string;
var
  LValue: TValue;
begin
  Result := '';
  if FHasValue then
  begin
    LValue := TValue.From<T>(FValue);
    Result := TValue.ToString(LValue);
  end;
end;

{ TSvLazy<T> }

constructor TSvLazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean);
begin
  inherited Create;
  FValueFactory := AValueFactory;
  FValueCreated := False;
  FOwnsObjects := AOwnsObjects;
  FManager := nil;
end;

constructor TSvLazy<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FValueCreated := False;
  FOwnsObjects := AOwnsObjects;
  FManager := nil;
  FEntity := nil;
  FColumn := nil;
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
      FValue := TRttiExplorer.CreateNewClass<T>();
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
  CheckInitialized();
  FManager.SetLazyValue<T>(FValue, TUtils.FromVariant(FID), FEntity, FColumn);
  Result := FValue;
end;

function TSvLazy<T>.GetColumn: Column;
begin
  Result := FColumn;
end;

function TSvLazy<T>.GetDisableCache: Boolean;
begin
  Result := FDisableCache;
end;

function TSvLazy<T>.GetEntity: TObject;
begin
  Result := FEntity;
end;

function TSvLazy<T>.GetID: Variant;
begin
  Result := FID;
end;

function TSvLazy<T>.GetManager: TEntityManager;
begin
  Result := FManager;
end;

function TSvLazy<T>.GetValue: T;
begin
  if (not FValueCreated) or FDisableCache then
  begin
    FValue := DoGetValue;
    CheckInitialized();
    FValueCreated := True;
  end;

  FDisableCache := False;
  Result := FValue;
end;

function TSvLazy<T>.OwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TSvLazy<T>.SetColumn(const Value: Column);
begin
  FColumn := Value;
end;

procedure TSvLazy<T>.SetDisableCache(const Value: Boolean);
begin
  FDisableCache := Value;
end;

procedure TSvLazy<T>.SetEntity(const Value: TObject);
begin
  FEntity := Value;
end;

procedure TSvLazy<T>.SetID(const Value: Variant);
begin
  FID := Value;
end;

procedure TSvLazy<T>.SetManager(const Value: TEntityManager);
begin
  FManager := Value;
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

constructor Lazy<T>.Create(AValue: T);
begin
  FLazy := TSvLazy<T>.Create(AValue, False);
end;

function Lazy<T>.GetDisableCache: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
  begin
    Result := FLazy.DisableCacheForNextGet;
  end;
end;

function Lazy<T>.GetID: Variant;
begin
  Result := Unassigned;
  if Assigned(FLazy) then
    Result := FLazy.ID;
end;

function Lazy<T>.GetManager: TEntityManager;
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
  FLazy.Manager := FManager;
  FLazy.ID := FID;
  FLazy.Entity := FEntity;
  FLazy.EntityColumn := FColumn;
  Result := FLazy.Value;
end;

class operator Lazy<T>.Implicit(const AValue: T): Lazy<T>;
begin
  Result := Lazy<T>.Create(AValue);
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

procedure Lazy<T>.SetID(const Value: Variant);
begin
  if Assigned(FLazy) then
    FLazy.ID := Value;
end;

procedure Lazy<T>.SetManager(const Value: TEntityManager);
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
  FLazy.Manager := FManager;
  FLazy.ID := FID;
  FLazy.Entity := FEntity;
  FLazy.EntityColumn := FColumn;
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
  Result := FManager.GetLazyValueClass<T>(TUtils.FromVariant(FID), FEntity, FColumn);
end;

end.
