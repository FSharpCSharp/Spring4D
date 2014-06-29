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
unit SvContainers;

interface

uses
  Generics.Collections
  ,Rtti
  ;


type
  /// <remarks>
  /// Object list with no argument constructor for easier serialization
  /// </remarks>
  TSvObjectList<T: class> = class(TObjectList<T>)
  public
    constructor Create(); reintroduce; overload;
  end;

  /// <summary>
  /// Generic list which contains needed methods for kernel
  /// </summary>
  TSvList<T> = class(TList<T>)
  private
    FSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
    function Convert(const Value: TValue): TValue;
  public
    /// <summary>
    /// Checks if list contains all the given values
    /// </summary>
    /// <param name="AValues">Values to check</param>
    /// <returns>true: all the values are in the list, otherwise - false</returns>
    function Includes(const AValues: array of T): Boolean;
    function ToString(): string; reintroduce;
    function IsEmpty(): Boolean;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  TSvPair<TKey,TVal> = record
  public
    Key: TKey;
    Value: TVal;

    procedure SetKey(const AKey: TKey);
    procedure SetValue(const AValue: TVal);

    class function Init(const AKey: TKey; const AValue: TVal): TSvPair<TKey, TVal>; static;
  end;

  TSvListMap<TKey, TVal> = class(TSvList<TSvPair<TKey,TVal>>)
  public
    function AddEx(const AKey: TKey; const AValue: TVal): Integer;
    procedure EditKey(AIndex: Integer; const AKey: TKey);
    procedure EditValue(AIndex: Integer; const AValue: TVal);
    function TryGetValue(const AKey: TKey; out AValue: TVal): Boolean;
    function TryGetValueDef(const AKey: TKey; out AValue: TVal; const ADef: TVal): Boolean;
    function GetValue(const AKey: TKey): TVal;
    function GetValueDef(const AKey: TKey; const ADef: TVal): TVal;
  end;

  TSvIntegerIntegerList = class(TSvListMap<Integer, Integer>)
  public
    constructor Create(); reintroduce;
  end;

  TSvIntegerDoubleList = class(TSvListMap<Integer, Double>)
  public
    constructor Create(); reintroduce;
  end;

  TSvIntegerDynamicList = class(TSvListMap<Integer, TArray<Variant>>)
  public
    constructor Create(); reintroduce;
  end;

  TSvStringStringList = class(TSvListMap<string, string>)
  public
    constructor Create(); reintroduce;
  end;


implementation

uses
  TypInfo
  ,Generics.Defaults
  ,SysUtils
  ;

{ TSvObjectList<T> }

constructor TSvObjectList<T>.Create;
begin
  inherited Create(True);
end;

{ TSvList<T> }

function TSvList<T>.Convert(const Value: TValue): TValue;
var
  mtdEnum, mtdMoveNext: TRttiMethod;
  ctx: TRttiContext;
  rType, enumType: TRttiType;
  lEnumerator: TValue;
  propCurrent: TRttiProperty;
  sResult: string;
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := Value.TypeInfo;
  sResult := '';
  //from source to target
  //source (Value) TList<> , target (Result): string
  if Value.Kind = tkClass then
  begin
    ctx := TRttiContext.Create;
    rType := ctx.GetType(ATypeInfo);
    mtdEnum := rType.GetMethod('GetEnumerator');
    if Assigned(mtdEnum) then
    begin
      lEnumerator := mtdEnum.Invoke(Value, []);
      enumType := ctx.GetType(lEnumerator.TypeInfo);
      mtdMoveNext := enumType.GetMethod('MoveNext');
      propCurrent := enumType.GetProperty('Current');
      Assert(Assigned(mtdMoveNext), 'MoveNext method not found');
      Assert(Assigned(propCurrent), 'Current property not found');
      while mtdMoveNext.Invoke(lEnumerator.AsObject,[]).asBoolean do
      begin
        sResult := sResult + propCurrent.GetValue(lEnumerator.AsObject).ToString + ',';
      end;

      if sResult <> '' then
      begin
        SetLength(sResult, Length(sResult)-1); //cut last comma
      end;

      if lEnumerator.IsObject then
      begin
        lEnumerator.AsObject.Free;
      end;

    end;
    Result := sResult;
  end
  else
  begin
    Result := '';
  end;
end;

function TSvList<T>.Includes(const AValues: array of T): Boolean;
var
  i, ix: Integer;
begin
  Result := False;

  Sorted := True;

  for i := Low(AValues) to High(AValues) do
  begin
    Result := BinarySearch(AValues[i], ix);
    if not Result then
      Exit;
  end;
end;

function TSvList<T>.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

procedure TSvList<T>.SetSorted(const Value: Boolean);
begin
  if Value <> FSorted then
  begin
    Sort();
    FSorted := Value;
  end;
end;

function TSvList<T>.ToString: string;
var
  ASource, ATarget: TValue;
begin
  Result := '';
  ASource := Self;
  ATarget := Convert(ASource);

  Result := ATarget.AsType<string>;
end;

{ TSvPair<TKey, TValue> }

class function TSvPair<TKey, TVal>.Init(const AKey: TKey;
  const AValue: TVal): TSvPair<TKey, TVal>;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

{ TSvListMap<TKey, TValue> }

function TSvListMap<TKey, TVal>.AddEx(const AKey: TKey; const AValue: TVal): Integer;
begin
  Result := Add(TSvPair<TKey, TVal>.Init(AKey, AValue));
end;

procedure TSvListMap<TKey, TVal>.EditKey(AIndex: Integer; const AKey: TKey);
var
  APair: TSvPair<TKey, TVal>;
begin
  APair := Items[AIndex];

  APair.Key := AKey;

  Items[AIndex] := APair;
end;

procedure TSvListMap<TKey, TVal>.EditValue(AIndex: Integer; const AValue: TVal);
var
  APair: TSvPair<TKey, TVal>;
begin
  APair := Items[AIndex];

  APair.Value := AValue;

  Items[AIndex] := APair;
end;

function TSvListMap<TKey, TVal>.GetValue(const AKey: TKey): TVal;
begin
  TryGetValue(AKey, Result);
end;

function TSvListMap<TKey, TVal>.GetValueDef(const AKey: TKey; const ADef: TVal): TVal;
begin
  TryGetValueDef(AKey, Result, ADef);
end;

function TSvListMap<TKey, TVal>.TryGetValueDef(const AKey: TKey; out AValue: TVal;
  const ADef: TVal): Boolean;
begin
  Result := TryGetValue(AKey, AValue);
  if not Result then
    AValue := ADef;
end;

function TSvListMap<TKey, TVal>.TryGetValue(const AKey: TKey; out AValue: TVal): Boolean;
var
  ix: Integer;
begin
  Result := False;
  Sorted := True;

  Result := BinarySearch(TSvPair<TKey, TVal>.Init(AKey, AValue), ix);
  if Result then
  begin
    AValue := Items[ix].Value;
  end
  else
  begin
    AValue := System.Default(TVal);
  end;
end;

procedure TSvPair<TKey, TVal>.SetKey(const AKey: TKey);
begin
  Key := AKey;
end;

procedure TSvPair<TKey, TVal>.SetValue(const AValue: TVal);
begin
  Value := AValue;
end;

{ TSvIntegerDoubleList }

constructor TSvIntegerDoubleList.Create;
var
  FComparer: IComparer<TsvPair<Integer, Double>>;
begin
  FComparer := TComparer<TsvPair<Integer, Double>>.Construct(
    function(const Left, Right: TsvPair<Integer, Double>): Integer
    begin
      Result := Left.Key - Right.Key;
    end );

  inherited Create(FComparer);

end;

{ TSvIntegerIntegerList }

constructor TSvIntegerIntegerList.Create;
var
  FComparer: IComparer<TsvPair<Integer, Integer>>;
begin
  FComparer := TComparer<TsvPair<Integer, Integer>>.Construct(
    function(const Left, Right: TsvPair<Integer, Integer>): Integer
    begin
      Result := Left.Key - Right.Key;
    end );

  inherited Create(FComparer);

end;

{ TSvIntegerDynamicList }

constructor TSvIntegerDynamicList.Create;
var
  FComparer: IComparer<TsvPair<Integer, TArray<Variant>>>;
begin
  FComparer := TComparer<TsvPair<Integer, TArray<Variant>>>.Construct(
    function(const Left, Right: TsvPair<Integer, TArray<Variant>>): Integer
    begin
      Result := Left.Key - Right.Key;
    end );

  inherited Create(FComparer);

end;

{ TSvStringStringList }

constructor TSvStringStringList.Create;
var
  FComparer: IComparer<TsvPair<string, string>>;
begin
  FComparer := TComparer<TsvPair<string, string>>.Construct(
    function(const Left, Right: TsvPair<string, string>): Integer
    begin
      Result := CompareStr(Left.Key, Right.Key);
    end );

  inherited Create(FComparer);
end;

end.
