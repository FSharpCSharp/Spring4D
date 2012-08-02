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
  Rtti, Core.EntityManager;

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
  /// Variable with lazy loading
  /// </summary>
  Lazy<T> = record
  private
    FLoaded: Boolean;
    FValue: T;
    FId: TValue;
    FManager: TEntityManager;
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    property Value: T read GetValue write SetValue;

    constructor Create(const AManager: TEntityManager; const AId: TValue);

    class operator Implicit(const ALazy: Lazy<T>): T; inline;
    class operator Implicit(const AValue: T): Lazy<T>; inline;
  end;
  /// <summary>
  /// Nullable type
  /// </summary>
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


    property HasValue: Boolean read FHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    property ValueOrDef: T read GetValueOrDef;

    class operator Implicit(const Value: T): Nullable<T>;
    class operator Implicit(const Value: Nullable<T>): T;

    class operator Equal(const Left, Right: Nullable<T>): Boolean;
    class operator NotEqual(const Left, Right: Nullable<T>): Boolean;

  end;

implementation

uses
  TypInfo,
  Core.Exceptions,
  Mapping.RttiExplorer
  ,Core.Reflection
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

{ Lazy<T> }

constructor Lazy<T>.Create(const AManager: TEntityManager; const AId: TValue);
begin
  FManager := AManager;
  FId := AId;
end;

function Lazy<T>.GetValue: T;
begin
  if not FLoaded then
  begin
    if FId.IsEmpty then
      FValue := System.Default(T)
    else
    begin
      if not Assigned(FManager) then
        raise EEntityManagerNotSet.Create('Entity Manager for lazy value not set');

      FValue := FManager.Find<T>(FId);
    end;
    FLoaded := True;
  end;
  Result := FValue;
end;

class operator Lazy<T>.Implicit(const AValue: T): Lazy<T>;
begin
  Result.Value := AValue;
end;

class operator Lazy<T>.Implicit(const ALazy: Lazy<T>): T;
begin
  Result := ALazy.Value;
end;

procedure Lazy<T>.SetValue(const Value: T);
begin
  FValue := Value;
  FLoaded := True;
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

end.
