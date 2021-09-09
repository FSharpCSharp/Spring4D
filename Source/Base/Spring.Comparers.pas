{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2021 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Comparers;

interface

{$O+,W-,Q-,R-}

uses
  Generics.Defaults,
  TypInfo,
  Spring.Hash;

type
  TDefaultGenericInterface = Generics.Defaults.TDefaultGenericInterface;

  TComparer<T> = record
    class function Default: IComparer<T>; static;
  end;

  TEqualityComparer<T> = record
    class function Default: IEqualityComparer<T>; static;
  end;

  THashFunction = function(const key; len: Cardinal; seed: Integer = 0): Integer;

function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;
procedure RegisterComparer(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; const comparer: IInterface);
function SameGuid(const left, right: TGUID): Boolean;

var
  DefaultHashFunction: THashFunction = MurmurHash3;

implementation

uses
  Math,
  SyncObjs,
  SysUtils,
  Variants,
  Spring,
  Spring.HashTable;

function SameGuid(const left, right: TGUID): Boolean;
{$IFDEF ASSEMBLER}
asm
  movdqu   xmm0, [left]
  movdqu   xmm1, [right]
  pcmpeqd  xmm0, xmm1
  pmovmskb eax, xmm0
  cmp      ax, 65535
  sete     al
end;
{$ELSE}
begin
{$IFDEF CPU32BITS}
  Result := (left.D1 = right.D1)
    and (PInteger(@left.D2)^ = PInteger(@right.D2)^)
    and (PInteger(@left.D4[0])^ = PInteger(@right.D4[0])^)
    and (PInteger(@left.D4[4])^ = PInteger(@right.D4[4])^);
{$ELSE}
  Result := (PInt64(@left)^ = PInt64(@right)^)
    and (PInt64(@left.D4[0])^ = PInt64(@right.D4[0])^);
{$ENDIF}
end;
{$ENDIF}

class function TComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
end;

type
  TMethodPointer = procedure of object;
  Int24 = packed record
  case Integer of
    0: (Low: Word; High: Byte);
    1: (Bytes: array[0..2] of Byte);
  end;

  IComparer = record
    VTable,
    QueryInterface,
    AddRef,
    Release,
    Compare: Pointer;
  end;

  IEqualityComparer = record
    VTable,
    QueryInterface,
    AddRef,
    Release,
    Equals,
    GetHashCode: Pointer;
  end;

  TComparerSelector = function(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
  TComparerInfo = record
    Instance: array[TDefaultGenericInterface] of Pointer;
    Selector: Pointer;
  end;

  PComparerInstance = ^TComparerInstance;
  TComparerInstance = record
    VTable: Pointer;
    RefCount: Integer;
    TypeInfo: PTypeInfo;
    Size: Integer;

    function AddRef: Integer; stdcall;
    function Release: Integer; stdcall;

    function Compare_Binary(const left, right): Integer;
    function Equals_Binary(const left, right): Boolean;
    function GetHashCode_Binary(const value): Integer;

    function Compare_DynArray(const left, right: Pointer): Integer;
    function Equals_DynArray(const left, right: Pointer): Boolean;
    function GetHashCode_DynArray(const value: Pointer): Integer;
  end;

  TRegistryItem = packed record
  public
    HashCode: Integer;
    TypeInfo: PTypeInfo;
    Instance: array[TDefaultGenericInterface] of IInterface;
  end;

var
  comparerRegistry: THashTable;
  lock: TCriticalSection;

procedure RegisterComparer(intf: TDefaultGenericInterface; typeInfo: PTypeInfo;
  const comparer: IInterface);
var
  item: ^TRegistryItem;
begin
  item := THashTable<Pointer>(comparerRegistry).FindWithComparer(typeInfo, OverwriteExisting or InsertNonExisting);
  item.TypeInfo := typeInfo;
  item.Instance[intf] := comparer;
end;

function FindComparer(intf: TDefaultGenericInterface; typeInfo: PTypeInfo): Pointer;
var
  item: ^TRegistryItem;
begin
  item := THashTable<Pointer>(comparerRegistry).FindWithComparer(typeInfo, 0);
  if Assigned(item) then
    Result := Pointer(item.Instance[intf])
  else
    Result := nil;
end;

function MakeInstance(VTable: Pointer; typeInfo: PTypeInfo; size: Integer): PComparerInstance;
begin
  GetMem(Result, SizeOf(TComparerInstance));
  Result.VTable := vtable;
  Result.RefCount := 0;
  Result.TypeInfo := typeInfo;
  Result.Size := size;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function NopRef(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function TComparerInstance.AddRef: Integer; stdcall;
begin
  Result := AtomicIncrement(RefCount);
end;

function TComparerInstance.Release: Integer; stdcall;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
    FreeMem(@Self);
end;

function GetTypeData(typeInfo: PTypeInfo): PTypeData; inline;
begin
  Result := Pointer(NativeUInt(typeInfo) + NativeUInt(PByte(@PByte(typeInfo)[1])^) + 2);
end;

function Compare_Int8(const inst: Pointer; const left, right: ShortInt): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  setg    al
  cmovl   eax, ecx
end;
{$ELSE}
begin
  Result := Integer(left) - Integer(right);
end;
{$ENDIF}

function Equals_Int8(const inst: Pointer; const left, right: ShortInt): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_Int8(const inst: Pointer; const value: ShortInt): Integer;
begin
  Result := value;
end;

function Compare_UInt8(const inst: Pointer; const left, right: Byte): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  seta    al
  cmovb   eax, ecx
end;
{$ELSE}
begin
  Result := Integer(left) - Integer(right);
end;
{$ENDIF}

function Compare_Int16(const inst: Pointer; const left, right: SmallInt): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  setg    al
  cmovl   eax, ecx
end;
{$ELSE}
begin
  Result := Integer(left) - Integer(right);
end;
{$ENDIF}

function Equals_Int16(const inst: Pointer; const left, right: SmallInt): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_Int16(const inst: Pointer; const value: SmallInt): Integer;
begin
  Result := value;
end;

function Compare_UInt16(const inst: Pointer; const left, right: Word): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  seta    al
  cmovb   eax, ecx
end;
{$ELSE}
begin
  Result := Integer(left) - Integer(right);
end;
{$ENDIF}

function Compare_Bin16(const inst: Pointer; const left, right: Word): Integer;
var
  leftVal, rightVal: NativeUInt;
begin
  leftVal := left;
  rightVal := right;
  leftVal := Swap(leftVal);
  rightVal := Swap(rightVal);
  Result := Integer(leftVal) - Integer(rightVal);
end;

function Compare_Bin24(const inst: Pointer; const left, right: Int24): Integer;
var
  leftVal, rightVal: NativeUInt;
begin
  leftVal := left.Low;
  rightVal := right.Low;
  leftVal := (Swap(leftVal) shl 8) + left.High;
  rightVal := (Swap(rightVal) shl 8) + right.High;
  Result := Integer(leftVal) - Integer(rightVal);
end;

function Compare_Bin32(const inst: Pointer; const left, right: Cardinal): Integer;
var
  leftVal, rightVal: NativeUInt;
begin
  {$IFDEF CPU64BITS}
  leftVal := left;
  rightVal := right;
  leftVal := (Swap(leftVal) shl 16) + Swap(leftVal shr 16);
  rightVal := (Swap(rightVal) shl 16) + Swap(rightVal shr 16);
  {$ELSE}
  leftVal := (Swap(left) shl 16) + Swap(left shr 16);
  rightVal := (Swap(right) shl 16) + Swap(right shr 16);
  {$ENDIF}
  Result := ShortInt(Byte(leftVal >= rightVal) - Byte(leftVal <= rightVal));
end;

function Compare_Bin64(const inst: Pointer; const left, right: Int64): Integer;
var
  leftVal, rightVal: NativeUInt;
begin
  if Integer(left) = Integer(right) then
  begin
    leftVal := left shr 32;
    rightVal := right shr 32;
  end else
  begin
    leftVal := Cardinal(left);
    rightVal := Cardinal(right);
  end;
  leftVal := (Swap(leftVal) shl 16) + Swap(leftVal shr 16);
  rightVal := (Swap(rightVal) shl 16) + Swap(rightVal shr 16);

  Result := ShortInt(Byte(leftVal >= rightVal) - Byte(leftVal <= rightVal));
end;

function Equals_Bin24(const inst: Pointer; const left, right: Int24): Boolean;
begin
  Result := (left.Low + left.High shl 16) = (right.Low + right.High shl 16);
end;

function GetHashCode_Bin24(const inst: Pointer; const value: Int24): Integer;
begin
  Result := value.Low + value.High shl 16;
end;

function Compare_Int32(const inst: Pointer; const left, right: Integer): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  setg    al
  cmovl   eax, ecx
end;
{$ELSE}
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}

function Equals_Int32(const inst: Pointer; const left, right: Integer): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_Int32(const inst: Pointer; const value: Integer): Integer;
begin
  Result := value;
end;

function Compare_UInt32(const inst: Pointer; const left, right: Cardinal): Integer;
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
{$IFDEF CPUX86}
  lea     ecx, [eax-1]
{$ELSE}
  lea     ecx, [rax-1]
{$ENDIF}
  seta    al
  cmovb   eax, ecx
end;
{$ELSE}
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}

function Compare_Int64(const inst: Pointer; const left, right: Int64): Integer;
{$IFDEF CPUX64}
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
  lea     ecx, [rax-1]
  setg    al
  cmovl   eax, ecx
end;
{$ELSE}
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}
{$ELSE}
type
  Int64Rec = record
    Lo: Cardinal;
    Hi: Integer;
  end;
var
  loLeft, loRight: Cardinal;
  hiLeft, hiRight: Integer;
begin
  hiLeft := Int64Rec(left).Hi;
  hiRight := Int64Rec(right).Hi;
  if hiLeft <> hiRight then
    Result := ShortInt(Byte(hiLeft >= hiRight) - Byte(hiLeft <= hiRight))
  else
  begin
    loLeft := Int64Rec(left).Lo;
    loRight := Int64Rec(right).Lo;
    Result := ShortInt(Byte(loLeft >= loRight) - Byte(loLeft <= loRight));
  end;
end;
{$ENDIF}

function Equals_Int64(const inst: Pointer; const left, right: Int64): Boolean;
begin
  Result := left = right;
end;

{$IFDEF CPUX86}
function GetHashCode_Int64: Integer;  // no parameters to avoid stackframe
asm
  mov     eax, [esp+8]
  xor     eax, [esp+4]
  ret     8
end;
{$ELSE}
function GetHashCode_Int64(const inst: Pointer; const value: Int64): Integer;
type
  Int64Rec = record
    Lo, Hi: Integer;
  end;
begin
  Result := Int64Rec(value).Lo xor Int64Rec(value).Hi;
end;
{$ENDIF}

function Compare_UInt64(const inst: Pointer; const left, right: UInt64): Integer;
{$IFDEF CPUX64}
{$IFDEF ASSEMBLER}
asm
  xor     eax, eax
  cmp     left, right
  lea     ecx, [rax-1]
  seta    al
  cmovb   eax, ecx
end;
{$ELSE}
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}
{$ELSE}
type
  UInt64Rec = array[0..1] of Cardinal;
var
  index: Integer;
  leftVal, rightVal: Cardinal;
begin
  index := Byte(UInt64Rec(left)[1] <> UInt64Rec(right)[1]);
  leftVal := UInt64Rec(left)[index];
  rightVal := UInt64Rec(right)[index];
  Result := Shortint(Byte(leftVal >= rightVal) - Byte(leftVal <= rightVal));
end;
{$ENDIF}

{$IFDEF ASSEMBLER}
function Compare_Single: Integer;
asm
  {$IFDEF CPUX86}
  movss   xmm1, dword ptr [esp+8]
  movss   xmm2, dword ptr [esp+4]
  {$ENDIF}
  xor     eax, eax
  comiss  xmm1, xmm2
  {$IFDEF CPUX86}
  lea     ecx, [eax-1]
  {$ELSE}
  lea     ecx, [rax-1]
  {$ENDIF}
  seta    al
  cmovb   eax, ecx
  {$IFDEF CPUX86}
  ret     8
  {$ENDIF}
end;
{$ELSE}
function Compare_Single(const inst: Pointer; const left, right: Single): Integer;
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}

function Equals_Single(const inst: Pointer; const left, right: Single): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_Single(const inst: Pointer; const value: Single): Integer;
var
  bits: Int32;
begin
  bits := PInt32(@value)^;

  if ((bits - 1) and $7FFFFFFF) >= $7F800000 then
    // Ensure that all NaNs and both zeros have the same hash code
    bits := bits and $7F800000;

  Result := DefaultHashFunction(bits, SizeOf(Int32));
end;

{$IFDEF ASSEMBLER}
function Compare_Double: Integer; // no parameters to avoid stackframe
asm
  {$IFDEF CPUX86}
  movsd   xmm1, qword ptr [esp+12]
  movsd   xmm2, qword ptr [esp+4]
  {$ENDIF}
  xor     eax, eax
  comisd  xmm1, xmm2
  {$IFDEF CPUX86}
  lea     ecx, [eax-1]
  {$ELSE}
  lea     ecx, [rax-1]
  {$ENDIF}
  seta    al
  cmovb   eax, ecx
  {$IFDEF CPUX86}
  ret     16
  {$ENDIF}
end;
{$ELSE}
function Compare_Double(const inst: Pointer; const left, right: Double): Integer;
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;
{$ENDIF}

{$IFDEF ASSEMBLER}
function Equals_Double: Boolean; // no parameters to avoid stackframe
asm
  {$IFDEF CPUX86}
  movsd   xmm1, qword ptr [esp+12]
  movsd   xmm2, qword ptr [esp+4]
  {$ENDIF}
  mov     al, 1
  ucomisd xmm1, xmm2
  jne     @@not_equal
  jp      @@not_equal
  {$IFDEF CPUX86}
  ret     $10
  {$ELSE}
  ret
  {$ENDIF}
@@not_equal:
  ucomisd xmm1, xmm1
  setp    cl
  ucomisd xmm2, xmm2
  setp    al
  and     al, cl
  {$IFDEF CPUX86}
  ret 16
  {$ENDIF}
end;
{$ELSE}
function Equals_Double(const inst: Pointer; const left, right: Double): Boolean;
begin
  Result := left = right;
end;
{$ENDIF}

function GetHashCode_Double(const inst: Pointer; const value: Double): Integer;
var
  bits: Int64;
begin
  bits := PInt64(@value)^;

  if ((bits - 1) and $7FFFFFFFFFFFFFFF) >= $7FF0000000000000 then
    // Ensure that all NaNs and both zeros have the same hash code
    bits := bits and $7FF0000000000000;

  Result := DefaultHashFunction(bits, SizeOf(Int64));
end;

function Compare_Extended(const inst: Pointer; const left, right: Extended): Integer;
begin
  Result := ShortInt(Byte(left >= right) - Byte(left <= right));
end;

function Equals_Extended(const inst: Pointer; const left, right: Extended): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_Extended(const inst: Pointer; const value: Extended): Integer;
const
{$IF (SizeOf(Extended) > 10) and (Defined(CPUX86) or Defined(CPUX64))}
  BYTESIZEOFEXTENDED = 10;
{$ELSE}
  BYTESIZEOFEXTENDED = SizeOf(Extended);
{$IFEND}
var
  m: Extended;
  e: Integer;
begin
  Frexp(Value, m, e);
  if m = 0 then
    m := Abs(m);
  Result := DefaultHashFunction(m, BYTESIZEOFEXTENDED, 0);
  Result := DefaultHashFunction(e, SizeOf(e), Result);
end;

function Compare_String(const inst: Pointer; const left, right: OpenString): Integer;
begin
  if left < right then
    Result := -1
  else if left > right then
    Result := 1
  else
    Result := 0;
end;

function Equals_String(const inst: Pointer; const left, right: OpenString): Boolean;
begin
  Result := left = right;
end;

function GetHashCode_String(const inst: Pointer; const value: OpenString): Integer;
begin
  Result := DefaultHashFunction(value[1], Length(value));
end;

function Equals_Class(const inst: Pointer; const left, right: TObject): Boolean;
begin
  if left <> nil then
    Result := left.Equals(right)
  else if right <> nil then
    Result := right.Equals(left)
  else
    Result := True;
end;

function Compare_Class(const inst: Pointer; const left, right: TObject): Integer;
begin
  if Equals_Class(inst, left, right) then
    Result := 0
  else if NativeInt(left) < NativeInt(right) then
    Result := -1
  else if NativeInt(left) > NativeInt(right) then
    Result := 1
  else
    Result := 0;
end;

function GetHashCode_Class(const inst: Pointer; const value: TObject): Integer;
begin
  if Value = nil then
    Result := 0
  else
    Result := value.GetHashCode;
end;

function Compare_Method(const inst: Pointer; const left, right: TMethodPointer): Integer;
var
  leftVal, rightVal: NativeUInt;
begin
  leftVal := NativeUInt(TMethod(left).Data);
  rightVal := NativeUInt(TMethod(right).Data);
  if leftVal = rightVal then
  begin
    leftVal := NativeUInt(TMethod(left).Code);
    rightVal := NativeUInt(TMethod(right).Code);
  end;

  Result := ShortInt(Byte(leftVal >= rightVal) - Byte(leftVal <= rightVal));
end;

function Equals_Method(const inst: Pointer; const left, right: TMethodPointer): Boolean;
begin
  Result := (NativeInt(TMethod(left).Data) - NativeInt(TMethod(right).Data)) or
    (NativeInt(TMethod(left).Code) - NativeInt(TMethod(right).Code)) = 0;
end;

function GetHashCode_Method(const inst: Pointer; const value: TMethodPointer): Integer;
begin
  Result := DefaultHashFunction(value, SizeOf(TMethodPointer));
end;

function Compare_LString(const inst: Pointer; const left, right: RawByteString): Integer;
var
  leftPtr, rightPtr: PAnsiChar;
  leftLen, rightLen, i: integer;
begin
  leftLen:= Length(left);
  leftPtr:= PAnsiChar(left);

  rightLen:= Length(right);
  rightPtr:= PAnsiChar(right);

  i := 0;
  while (i < leftLen) and (i < rightLen) do
  begin
    if leftPtr^ <> rightPtr^ then
      Exit(Ord(leftPtr^) - Ord(rightPtr^));

    Inc(leftPtr);
    Inc(rightPtr);
    Inc(i);
  end;

  Result:= leftLen - rightLen;
end;

function Equals_LString(const inst: Pointer; const left, right: RawByteString): Boolean;
begin
  Result := Compare_LString(inst, left, right) = 0;
end;

function GetHashCode_LString(const inst: Pointer; const value: RawByteString): Integer;
begin
  Result := DefaultHashFunction(value[1], Length(value) * SizeOf(value[1]));
end;

function Compare_WString(const inst: Pointer; const left, right: WideString): Integer;
var
  leftChar, rightChar: Char;
  i: NativeInt;
begin
  if Pointer(left) = Pointer(right) then Exit(0);
  if Pointer(left) = nil then Exit(-1);
  if Pointer(right) = nil then Exit(1);
  i := 1;
  while True do
  begin
    leftChar:= left[i];
    rightChar:= right[i];
    Result := Integer(leftChar > rightChar) - Integer(leftChar < rightChar);
    if (Integer(Result = 0) and Integer(leftChar <> #0) and Integer(rightChar <> #0)) = 0 then Exit;
    Inc(i);
  end;
end;

function Equals_WString(const inst: Pointer; const left, right: WideString): Boolean;
begin
  Result := Compare_WString(inst, left, right) = 0;
end;

function GetHashCode_WString(const inst: Pointer; const value: RawByteString): Integer;
begin
  Result := DefaultHashFunction(value[1], Length(value) * SizeOf(value[1]));
end;

function Compare_UString(const inst: Pointer; const left, right: string): Integer;
begin
  Result := CompareStr(left, right);
end;

function Equals_UString(const inst: Pointer; const left, right: string): Boolean;
{$IFDEF CPUX86}
asm
  mov  eax, ecx
  call System.@UStrEqual
  setz al
end;
{$ELSE}
begin
  Result := left = right;
end;
{$ENDIF}

function GetHashCode_UString(const inst: Pointer; const value: string): Integer;
var
  hashCode: NativeInt;
begin
  hashCode := NativeInt(value);
  if hashCode <> 0 then
    hashCode := DefaultHashFunction(PPointer(hashCode)^, PInteger(@PByte(value)[-4])^ * SizeOf(Char));
  Result := hashCode;
end;

function Compare_Variant_Complex(checkEquality: Boolean; const left, right: PVariant): Integer;
var
  leftAsString, rightAsString: string;
begin
  try
    if left^ = right^ then
      Result := 0
    else if not checkEquality then
      Result := Ord(left^ > right^) * 2 - 1
    else
      Result := 1;
  except
    try
      leftAsString := left^;
      rightAsString := right^;
      if checkEquality and (Length(leftAsString) <> Length(rightAsString)) then
        Exit(1);
      Result := CompareStr(leftAsString, rightAsString);
    except
      Result := BinaryCompare(Pointer(left), Pointer(right), SizeOf(Variant));
    end;
  end;
end;

function Compare_Variant(const inst: Pointer; left, right: PVarData): Integer;
var
  varTypeLeft, varTypeRight: Integer;
{$IFNDEF CPU64BITS}
  valueLeft, valueRight: Integer;
{$ENDIF}
begin
  varTypeLeft := left.VType;
  if varTypeLeft <> varByRef or varVariant then
  begin
    varTypeRight := right.VType;
    if varTypeRight <> varByRef or varVariant then
    begin
      if (varTypeLeft > varNull) and (varTypeRight > varNull) then
      begin
        if varTypeLeft = varTypeRight then
          case varTypeLeft of
            varShortInt:
              Exit(Integer(left.VShortInt) - Integer(right.VShortInt));
            varBoolean, varByte:
              Exit(Integer(left.VByte) - Integer(right.VByte));
            varSmallint:
              Exit(Integer(left.VSmallInt) - Integer(right.VSmallInt));
            varWord:
              Exit(Integer(left.VWord) - Integer(right.VWord));
            varInteger:
              Exit(ShortInt(Byte(left.VInteger >= right.VInteger) - Byte(left.VInteger <= right.VInteger)));
            varLongWord:
              Exit(ShortInt(Byte(left.VLongWord >= right.VLongWord) - Byte(left.VLongWord <= right.VLongWord)));
            varInt64, varCurrency:
            begin
              {$IFDEF CPU64BITS}
              Result := ShortInt(Byte(Left.VInt64 >= right.VInt64) - Byte(left.VInt64 <= right.VInt64));
              {$ELSE}
              valueLeft := left.VLongs[2];
              valueRight := right.VLongs[2];
              if valueLeft <> valueRight then
                Result := ShortInt(Byte(valueLeft >= valueRight) - Byte(valueLeft <= valueRight))
              else
              begin
                valueLeft := Left.VInteger;
                valueRight := Right.VInteger;
                Result := ShortInt(Byte(Cardinal(valueLeft) >= Cardinal(valueRight)) - Byte(Cardinal(valueLeft) <= Cardinal(valueRight)));
              end;
              {$ENDIF}
              Exit;
            end;
            varUInt64:
            begin
              {$IFDEF CPU64BITS}
              Result := ShortInt(Byte(left.VUInt64 >= right.VUInt64) - Byte(left.VUInt64 <= right.VUInt64));
              {$ELSE}
              valueLeft := left.VLongs[2];
              valueRight := right.VLongs[2];
              if valueLeft <> valueRight then
              begin
                valueLeft := Left.VInteger;
                valueRight := Right.VInteger;
              end;
              Result := Shortint(Byte(Cardinal(valueLeft) >= Cardinal(valueRight)) - Byte(Cardinal(valueLeft) <= Cardinal(valueRight)));
              {$ENDIF}
              Exit;
            end;
            varSingle:
              Exit(ShortInt(Byte(left.VSingle >= right.VSingle) - Byte(left.VSingle <= right.VSingle)));
            varDouble, varDate:
              Exit(ShortInt(Byte(left.VDouble >= right.VDouble) - Byte(left.VDouble <= right.VDouble)));
            varString:
              Exit(Compare_LString(nil, RawByteString(left.VPointer), RawByteString(right.VPointer)));
            varUString:
              Exit(Compare_UString(nil, string(left.VPointer), string(right.VPointer)));
            varOleStr:
              Exit(Compare_WString(nil, WideString(left.VPointer), WideString(right.VPointer)));
          end;
        Result := Compare_Variant_Complex(False, PVariant(left), PVariant(right));
      end
      else
        Result := 0;
    end
    else
    begin
      repeat
        right := right.VPointer;
        varTypeRight := right.VType;
      until varTypeRight <> varByRef or varVariant;
      Result := Compare_Variant(nil, left, right);
    end;
  end
  else
  begin
    repeat
      left := left.VPointer;
      varTypeLeft := left.VType;
    until varTypeLeft <> varByRef or varVariant;
    Result := Compare_Variant(nil, left, right);
  end;
end;

function Equals_Variant(const inst: Pointer; left, right: PVarData): Boolean;
var
  varTypeLeft, varTypeRight: Integer;
begin
  varTypeLeft := left.VType;
  while varTypeLeft = varByRef or varVariant do
  begin
    left := left.VPointer;
    varTypeLeft := left.VType;
  end;

  varTypeRight := right.VType;
  while varTypeRight = varByRef or varVariant do
  begin
    right := right.VPointer;
    varTypeRight := right.VType;
  end;

  if (varTypeLeft > varNull) and (varTypeRight > varNull) then
  begin
    if varTypeLeft = varTypeRight then
      case varTypeLeft of
        varShortInt, varBoolean, varByte:
          Exit(left.VByte = right.VByte);
        varSmallint, varWord:
          Exit(left.VWord = right.VWord);
        varInteger, varLongWord:
          Exit(left.VInteger = right.VInteger);
        varInt64, varCurrency, varUInt64:
          {$IFDEF CPU64BITS}
          Exit(left.VInt64 = right.VInt64);
          {$ELSE}
          Exit((left.VLongs[1] - right.VLongs[1]) or (left.VLongs[2] - right.VLongs[2]) = 0);
          {$ENDIF}
        varSingle:
          Exit((left.VSingle >= right.VSingle) = (left.VSingle <= right.VSingle));
        varDouble, varDate:
          Exit((left.VDouble >= right.VDouble) = (left.VDouble <= right.VDouble));
        varString:
          Exit(Compare_LString(nil, RawByteString(left.VPointer), RawByteString(right.VPointer)) = 0);
        varUString:
          Exit(string(left.VPointer) = string(right.VPointer));
        varOleStr:
          Exit(Compare_WString(nil, WideString(left.VPointer), WideString(right.VPointer)) = 0);
      end;
    Result := Compare_Variant_Complex(True, PVariant(Left), PVariant(Right)) = 0;
  end
  else
    Result := True;
end;

function GetHashCode_Variant_Complex(const inst: Pointer; value: PVariant): Integer;
var
  valueAsString: string;
begin
  try
    valueAsString := Value^;
    Result := GetHashCode_UString(nil, valueAsString);
  except
    Result := DefaultHashFunction(value^, SizeOf(Variant));
  end;
end;

function GetHashCode_Variant(const inst: Pointer; value: PVarData): Integer;
begin
  case value.VType of
    varEmpty, varNull:
      Exit(0);
    varShortInt, varBoolean, varByte:
      Exit(DefaultHashFunction(value.VByte, 1));
    varSmallint, varWord:
      Exit(DefaultHashFunction(value.VWord, 2));
    varInteger, varLongWord:
      Exit(value.VInteger);
    varInt64, varCurrency, varUInt64:
      Exit(DefaultHashFunction(value.VInt64, 8));
    varSingle:
      Exit(GetHashCode_Single(nil, value.VSingle));
    varDouble, varDate:
      Exit(GetHashCode_Double(nil, value.VDouble));
    varString:
      Exit(DefaultHashFunction(PByte(value.VPointer)[1], PByte(value.VPointer)^));
    varUString:
      Exit(GetHashCode_UString(nil, string(value.VPointer)));
    varOleStr:
      Exit(GetHashCode_WString(nil, RawByteString(value.VPointer)));
  else
    Result := GetHashCode_Variant_Complex(nil, PVariant(value));
  end;
end;

function Equals_GUID(const inst: Pointer; const left, right: TGUID): Boolean;
{$IFDEF ASSEMBLER}
asm
  movdqu   xmm0, [left]
  movdqu   xmm1, [right]
  pcmpeqd  xmm0, xmm1
  pmovmskb eax, xmm0
  cmp      ax, 65535
  sete     al
end;
{$ELSE}
begin
{$IFDEF CPU32BITS}
  Result := (left.D1 = right.D1)
    and (PInteger(@left.D2)^ = PInteger(@right.D2)^)
    and (PInteger(@left.D4[0])^ = PInteger(@right.D4[0])^)
    and (PInteger(@left.D4[4])^ = PInteger(@right.D4[4])^);
{$ELSE}
  Result := (PInt64(@left)^ = PInt64(@right)^)
    and (PInt64(@left.D4[0])^ = PInt64(@right.D4[0])^);
{$ENDIF}
end;
{$ENDIF}

function GetHashCode_GUID(const inst: Pointer; const value: TGUID): Integer;
begin
  Result := DefaultHashFunction(value, SizeOf(TGUID));
end;

function TComparerInstance.Compare_Binary(const left, right): Integer;
begin
  Result := BinaryCompare(@left, @right, Size); // TODO: faster implementation
end;

function TComparerInstance.Equals_Binary(const left, right): Boolean;
begin
  Result := CompareMem(@left, @right, Size);
end;

function TComparerInstance.GetHashCode_Binary(const value): Integer;
begin
  Result := DefaultHashFunction(value, Size);
end;

function TComparerInstance.Compare_DynArray(const left, right: Pointer): Integer;
var
  len, lenDiff: NativeInt;
begin
  len := DynArrayLength(left);
  lenDiff := len - DynArrayLength(right);
  if lenDiff > 0 then
    Dec(len, lenDiff);
  Result := BinaryCompare(left, right, Size * len);
  if Result = 0 then
    Result := lenDiff;
end;

function TComparerInstance.Equals_DynArray(const left, right: Pointer): Boolean;
var
  leftLen, rightLen: NativeInt;
begin
  leftLen := DynArrayLength(left);
  rightLen := DynArrayLength(right);
  if leftLen <> rightLen then
    Exit(False);
  Result := CompareMem(left, right, Size * leftLen);
end;

function TComparerInstance.GetHashCode_DynArray(const value: Pointer): Integer;
begin
  Result := DefaultHashFunction(value^, Size * DynArrayLength(value));
end;

const
  Comparer_Int8: IComparer = (
    VTable: @Comparer_Int8.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Int8
  );

  EqualityComparer_Int8: IEqualityComparer = (
    VTable: @EqualityComparer_Int8.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Int8;
    GetHashCode: @GetHashCode_Int8
  );

  Comparer_UInt8: IComparer = (
    VTable: @Comparer_UInt8.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_UInt8
  );

  Comparer_Int16: IComparer = (
    VTable: @Comparer_Int16.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Int16
  );

  EqualityComparer_Int16: IEqualityComparer = (
    VTable: @EqualityComparer_Int16.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Int16;
    GetHashCode: @GetHashCode_Int16
  );

  Comparer_UInt16: IComparer = (
    VTable: @Comparer_UInt16.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_UInt16
  );

  Comparer_Bin16: IComparer = (
    VTable: @Comparer_Bin16.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Bin16
  );

  Comparer_Bin24: IComparer = (
    VTable: @Comparer_Bin24.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Bin24
  );

  Comparer_Int32: IComparer = (
    VTable: @Comparer_Int32.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Int32
  );

  EqualityComparer_Int32: IEqualityComparer = (
    VTable: @EqualityComparer_Int32.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Int32;
    GetHashCode: @GetHashCode_Int32
  );

  Comparer_UInt32: IComparer = (
    VTable: @Comparer_UInt32.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_UInt32
  );

  EqualityComparer_Bin24: IEqualityComparer = (
    VTable: @EqualityComparer_Bin24.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Bin24;
    GetHashCode: @GetHashCode_Bin24
  );

  Comparer_Bin32: IComparer = (
    VTable: @Comparer_Bin32.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Bin32
  );

  Comparer_Int64: IComparer = (
    VTable: @Comparer_Int64.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Int64
  );

  EqualityComparer_Int64: IEqualityComparer = (
    VTable: @EqualityComparer_Int64.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Int64;
    GetHashCode: @GetHashCode_Int64
  );

  Comparer_UInt64: IComparer = (
    VTable: @Comparer_UInt64.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_UInt64
  );

  Comparer_Bin64: IComparer = (
    VTable: @Comparer_Bin64.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Bin64
  );

  Comparer_Single: IComparer = (
    VTable: @Comparer_Single.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Single
  );

  EqualityComparer_Single: IEqualityComparer = (
    VTable: @EqualityComparer_Single.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Single;
    GetHashCode: @GetHashCode_Single
  );

  Comparer_Double: IComparer = (
    VTable: @Comparer_Double.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Double
  );

  EqualityComparer_Double: IEqualityComparer = (
    VTable: @EqualityComparer_Double.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Double;
    GetHashCode: @GetHashCode_Double
  );

  Comparer_Extended: IComparer = (
    VTable: @Comparer_Extended.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Extended
  );

  EqualityComparer_Extended: IEqualityComparer = (
    VTable: @EqualityComparer_Extended.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Extended;
    GetHashCode: @GetHashCode_Extended
  );

  Comparer_String: IComparer = (
    VTable: @Comparer_String.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_String
  );

  EqualityComparer_String: IEqualityComparer = (
    VTable: @EqualityComparer_String.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_String;
    GetHashCode: @GetHashCode_String
  );

  Comparer_Class: IComparer = (
    VTable: @Comparer_Class.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Class
  );

  EqualityComparer_Class: IEqualityComparer = (
    VTable: @EqualityComparer_Class.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Class;
    GetHashCode: @GetHashCode_Class
  );

  Comparer_Method: IComparer = (
    VTable: @Comparer_Method.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Method
  );

  EqualityComparer_Method: IEqualityComparer = (
    VTable: @EqualityComparer_Method.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Method;
    GetHashCode: @GetHashCode_Method
  );

  Comparer_LString: IComparer = (
    VTable: @Comparer_LString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_LString
  );

  EqualityComparer_LString: IEqualityComparer = (
    VTable: @EqualityComparer_LString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_LString;
    GetHashCode: @GetHashCode_LString
  );

  Comparer_WString: IComparer = (
    VTable: @Comparer_WString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_WString
  );

  EqualityComparer_WString: IEqualityComparer = (
    VTable: @EqualityComparer_LString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_WString;
    GetHashCode: @GetHashCode_WString
  );

  Comparer_UString: IComparer = (
    VTable: @Comparer_UString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_UString
  );

  EqualityComparer_UString: IEqualityComparer = (
    VTable: @EqualityComparer_UString.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_UString;
    GetHashCode: @GetHashCode_UString
  );

  Comparer_Variant: IComparer = (
    VTable: @Comparer_Variant.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Compare: @Compare_Variant
  );

  EqualityComparer_Variant: IEqualityComparer = (
    VTable: @EqualityComparer_Variant.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_Variant;
    GetHashCode: @GetHashCode_Variant
  );

  EqualityComparer_GUID: IEqualityComparer = (
    VTable: @EqualityComparer_GUID.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    Equals: @Equals_GUID;
    GetHashCode: @GetHashCode_GUID
  );

  Comparer_VTable_Binary: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @TComparerInstance.AddRef,
    @TComparerInstance.Release,
    @TComparerInstance.Compare_Binary
  );

  EqualityComparer_VTable_Binary: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @TComparerInstance.AddRef,
    @TComparerInstance.Release,
    @TComparerInstance.Equals_Binary,
    @TComparerInstance.GetHashCode_Binary
  );

  Comparer_VTable_DynArray: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @TComparerInstance.AddRef,
    @TComparerInstance.Release,
    @TComparerInstance.Compare_DynArray
  );

  EqualityComparer_VTable_DynArray: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @TComparerInstance.AddRef,
    @TComparerInstance.Release,
    @TComparerInstance.Equals_DynArray,
    @TComparerInstance.GetHashCode_DynArray
  );

function Selector_Integer(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
const
  ComparerTable: array[TOrdType, TDefaultGenericInterface] of Pointer = (
   (@Comparer_Int8, @EqualityComparer_Int8), (@Comparer_UInt8, @EqualityComparer_Int8),
   (@Comparer_Int16, @EqualityComparer_Int16), (@Comparer_UInt16, @EqualityComparer_Int16),
   (@Comparer_Int32, @EqualityComparer_Int32), (@Comparer_UInt32, @EqualityComparer_Int32)
  );
begin
  Result := ComparerTable[GetTypeData(typeInfo).OrdType, intf];
end;

function Selector_Int64(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
const
  ComparerTable: array[Boolean, TDefaultGenericInterface] of Pointer = (
   (@Comparer_Int64, @EqualityComparer_Int64), (@Comparer_UInt64, @EqualityComparer_Int64)
  );
begin
  with GetTypeData(typeInfo)^ do
    Result := ComparerTable[MaxInt64Value <= MinInt64Value, intf];
end;

function Selector_Float(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
const
  ComparerTable: array[TFloatType, TDefaultGenericInterface] of Pointer = (
   (@Comparer_Single, @EqualityComparer_Single), (@Comparer_Double, @EqualityComparer_Double),
   (@Comparer_Extended, @EqualityComparer_Extended),
   (@Comparer_UInt64, @EqualityComparer_Int64), (@Comparer_UInt64, @EqualityComparer_Int64)
  );
begin
  Result := ComparerTable[GetTypeData(typeInfo).FloatType, intf];
end;

function Selector_Binary(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
const
  ComparerTable: array[0..5, TDefaultGenericInterface] of Pointer = (
    (@Comparer_Bin64, @EqualityComparer_Int64),
    (@Comparer_UInt8, @EqualityComparer_Int8),
    (@Comparer_Bin16, @EqualityComparer_Int16),
    (@Comparer_Bin24, @EqualityComparer_Bin24),
    (@Comparer_Bin32, @EqualityComparer_Int32),

    // binary vtable
    (@Comparer_VTable_Binary, @EqualityComparer_VTable_Binary)
  );
begin
  case size of
    1, 2, 3, 4 {$IFDEF PASS_64BIT_VALUE_REGISTER}, 8{$ENDIF}:
      Result := ComparerTable[size mod 8, intf];
  else
    lock.Acquire;
    try
      Result := FindComparer(intf, typeInfo);
      if not Assigned(Result) then
      begin
        Result := MakeInstance(ComparerTable[5, intf], typeInfo, size);
        RegisterComparer(intf, typeInfo, IInterface(Result));
      end;
    finally
      lock.Release;
    end;
  end;
end;

function Selector_DynArray(intf: TDefaultGenericInterface; typeInfo: PTypeInfo; size: Integer): Pointer;
const
  ComparerTable: array[TDefaultGenericInterface] of Pointer = (
    @Comparer_VTable_DynArray, @EqualityComparer_VTable_DynArray
  );
begin
  lock.Acquire;
  try
    Result := FindComparer(intf, typeInfo);
    if not Assigned(Result) then
    begin
      Result := MakeInstance(ComparerTable[intf], typeInfo, typeInfo.TypeData.elSize);
      RegisterComparer(intf, typeInfo, IInterface(Result));
    end;
  finally
    lock.Release;
  end;
end;

const
  ComparerTable: array[TTypeKind] of TComparerInfo = (
    (Selector: @Selector_Binary),                               // tkUnknown
    (Selector: @Selector_Integer),                              // tkInteger
    (Instance: (@Comparer_UInt8, @EqualityComparer_Int8)),      // tkChar
    (Selector: @Selector_Integer),                              // tkEnumeration
    (Selector: @Selector_Float),                                // tkFloat
    (Instance: (@Comparer_String, @EqualityComparer_String)),   // tkString
    (Selector: @Selector_Binary),                               // tkSet
    (Instance: (@Comparer_Class, @EqualityComparer_Class)),     // tkClass
    (Instance: (@Comparer_Method, @EqualityComparer_Method)),   // tkMethod
    (Instance: (@Comparer_UInt16, @EqualityComparer_Int16)),    // tkWChar
    (Instance: (@Comparer_LString, @EqualityComparer_LString)), // tkLString,
    (Instance: (@Comparer_WString, @EqualityComparer_WString)), // tkWString,
    (Instance: (@Comparer_Variant, @EqualityComparer_Variant)), // tkVariant
    (Selector: @Selector_Binary),                               // tkArray
    (Selector: @Selector_Binary),                               // tkRecord
    (Instance: {$IFDEF CPU64BITS}(                              // tkInterface
      @Comparer_UInt64, @EqualityComparer_Int64){$ELSE}(
      @Comparer_UInt32, @EqualityComparer_Int32){$ENDIF}),
    (Selector: @Selector_Int64),                                // tkInt64
    (Selector: @Selector_DynArray),                             // tkDynArray
    (Instance: (@Comparer_UString, @EqualityComparer_UString)), // tkUString
    (Instance: {$IFDEF CPU64BITS}(                              // tkClassRef
      @Comparer_UInt64, @EqualityComparer_Int64){$ELSE}(
      @Comparer_UInt32, @EqualityComparer_Int32){$ENDIF}),
    (Instance: {$IFDEF CPU64BITS}(                              // tkPointer
      @Comparer_UInt64, @EqualityComparer_Int64){$ELSE}(
      @Comparer_UInt32, @EqualityComparer_Int32){$ENDIF}),
    (Instance: {$IFDEF CPU64BITS}(                              // tkProcedure
      @Comparer_UInt64, @EqualityComparer_Int64){$ELSE}(
      @Comparer_UInt32, @EqualityComparer_Int32){$ENDIF})
  {$IF Declared(tkMRecord)}
    , (Selector: @Selector_Binary)                              // tkMRecord
  {$IFEND}
  );

function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;
begin
  if info <> nil then
  begin
    with ComparerTable[info.Kind] do
    begin
      Result := Instance[intf];
      if not Assigned(Result) and Assigned(Selector) then
        Result := TComparerSelector(Selector)(intf, info, size);
    end;
  end
  else
    Result := Selector_Binary(intf, info, size);
end;

function Equals_TypeInfo(const inst: Pointer; const left, right: PPTypeInfo): Boolean;
begin
  Result := left^ = right^;
end;

function GetHashCode_TypeInfo(const inst: Pointer; const value: PPTypeInfo): Integer;
begin
  Result := DefaultHashFunction(value^, SizeOf(PTypeInfo));
end;

procedure Initialize;
begin
  lock := TCriticalSection.Create;

  comparerRegistry.ItemsInfo := TypeInfo(TArray<TRegistryItem>);
  comparerRegistry.Initialize(@Equals_TypeInfo, @GetHashCode_TypeInfo, TypeInfo(PTypeInfo));

  RegisterComparer(giEqualityComparer, TypeInfo(TGUID), IInterface(@EqualityComparer_GUID));
end;

initialization
  Initialize;

finalization
  comparerRegistry.Clear;
  lock.Free;

end.
