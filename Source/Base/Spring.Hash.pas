{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

unit Spring.Hash;

interface

{$O+,W-,Q-,R-}

function MurmurHash3(const key; len: Cardinal; seed: Integer = 0): Integer;

function MurmurHash3_Int32(const key: Integer): Integer; //inline;

function xxHash32(const key; len: Cardinal; Seed: Integer = 0): Integer;

implementation

{$POINTERMATH ON}

function RotateLeft(const x: Cardinal; const y: Byte): Cardinal; inline;
begin
  Result := (x shl y) or (x shr (32 - y));
end;

function xxHash32(const key; len: Cardinal; seed: Integer = 0): Integer;
const
  Prime1 = 2654435761;
	Prime2 = 2246822519;
	Prime3 = 3266489917;
	Prime4 = 668265263;
	Prime5 = 374761393;
{$if not defined(ASSEMBLER)}
var
  v1, v2, v3, v4: Cardinal;
  data, limit, pEnd: PByte;
begin
  data := @key;
  pEnd := data + len;
  if len >= 16 then
  begin
    limit := pEnd - 16;
    v1 := Cardinal(seed) + Prime1 + Prime2;
    v2 := Cardinal(seed) + Prime2;
    v3 := Cardinal(seed);
    v4 := Cardinal(seed) - Prime1;

    repeat
      v1 := Prime1 * RotateLeft(v1 + Prime2 * PCardinal(data)[0], 13);
      v2 := Prime1 * RotateLeft(v2 + Prime2 * PCardinal(data)[1], 13);
      v3 := Prime1 * RotateLeft(v3 + Prime2 * PCardinal(data)[2], 13);
      v4 := Prime1 * RotateLeft(v4 + Prime2 * PCardinal(data)[3], 13);
      Inc(data, 16);
    until data > limit;

    Result := RotateLeft(v1, 1) +
              RotateLeft(v2, 7) +
              RotateLeft(v3, 12) +
              RotateLeft(v4, 18);
  end
  else
    Result := Cardinal(seed) + Prime5;

  Inc(Result, len);

  while data <= pEnd - 4 do
  begin
    Result := Cardinal(Result) + PCardinal(data)^ * Prime3;
    Result := RotateLeft(Result, 17) * Prime4;
    Inc(data, 4);
  end;

  while data < pEnd do
  begin
    Result := Result + data^ * Prime5;
    Result := RotateLeft(Result, 11) * Prime1;
    Inc(data);
  end;

  Result := Result xor (Result shr 15);
  Result := Cardinal(Result) * Prime2;
  Result := Result xor (Result shr 13);
  Result := Cardinal(Result) * Prime3;
  Result := Result xor (Result shr 16);
end;
{$ELSEIF defined(CPUX86)}
asm
  // eax = key, edx = len, ecx = seed

  push    ebx
  push    esi
  push    edi
  push    ebp
  push    edx

  mov     ebx, eax                            // data := @key;
  lea     esi, [eax+edx-16]                   // limit := Pointer(NativeUInt(data) + len - 16);
  lea     eax, [ecx+Prime5]                   // Result := seed + Prime5;
  cmp     edx, 16                             // if len >= 16 then
  jb      @tail

  lea     eax, [ecx+Prime1+Prime2]            // v1 := seed + Prime1 + Prime2;
  lea     edx, [ecx+Prime2]                   // v2 := seed + Prime2;
  mov     ebp, ecx                            // v3 := seed;
  lea     edi, [ecx-Prime1]                   // v4 := seed - Prime1;

@loop16bytes:
  imul    ecx, [ebx], Prime2                  // v1 := Prime1 * rol(v1 + Prime2 * PCardinal(data)[0], 13);
  add     eax, ecx
  rol     eax, 13
  imul    eax, eax, Prime1

  imul    ecx, [ebx+4], Prime2                // v2 := Prime1 * rol(v2 + Prime2 * PCardinal(data)[1], 13);
  add     edx, ecx
  rol     edx, 13
  imul    edx, edx, Prime1

  imul    ecx, [ebx+8], Prime2                // v3 := Prime1 * rol(v3 + Prime2 * PCardinal(data)[2], 13);
  add     ebp, ecx
  rol     ebp, 13
  imul    ebp, ebp, Prime1

  imul    ecx, [ebx+12], Prime2               // v4 := Prime1 * rol(v4 + Prime2 * PCardinal(data)[3], 13);
  add     edi, ecx
  rol     edi, 13
  imul    edi, edi, Prime1

  add     ebx, 16                             // Inc(NativeUInt(data), 16);
  cmp     ebx, esi                            // until NativeUInt(data) > NativeUInt(limit);
  jbe     @loop16bytes

  rol     eax, 1                              // Result := Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
  rol     edx, 7
  add     eax, edx
  rol     ebp, 12
  rol     edi, 18
  add     eax, ebp
  add     eax, edi

@tail:
  add     esi, 16
  add     eax, [esp]                          // Inc(Result, len);
  lea     ebp, [esi-4]
  cmp     ebp, ebx                            // while NativeUInt(data) <= (NativeUInt(pEnd) - 4) do
  jb      @lessThan4bytes

@loop4bytes:
  imul    edx, [ebx], Prime3                  // Result := Result + PCardinal(data)^ * Prime3;
  add     edx, eax
  rol     edx, 17                             // Result := Rol(Result, 17) * Prime4;
  imul    eax, edx, Prime4
  add     ebx, 4                              // Inc(NativeUInt(data), 4);
  cmp     ebx, ebp                            // while NativeUInt(data) <= (NativeUInt(pEnd) - 4) do
  jbe     @loop4bytes

@lessThan4bytes:
  cmp     ebx, esi
  jnb     @finalization

@loopBytes:
  movzx   edx, byte ptr [ebx]                 // Result := Result + PByte(data)^ * Prime5;
  imul    edx, edx, Prime5
  add     edx, eax
  rol     edx, 11                             // Result := Rol(Result, 11) * Prime1;
  imul    eax, edx, Prime1
  inc     ebx                                 // Inc(NativeUInt(data));
  cmp     ebx, esi                            // while NativeUInt(data) < NativeUInt(pEnd) do
  jb      @loopBytes

@finalization:
  mov     edx, eax                            // Result := Result xor (Result shr 15);
  shr     eax, 15
  xor     eax, edx
  imul    eax, eax, Prime2                    // Result := Result * Prime2;
  mov     edx, eax                            // Result := Result xor (Result shr 13);
  shr     edx, 13
  xor     edx, eax
  imul    edx, edx, Prime3                    // Result := Result * Prime3;
  mov     eax, edx                            // Result := Result xor (Result shr 16);
  shr     eax, 16
  xor     eax, edx

  pop     ecx
  pop     ebp
  pop     edi
  pop     esi
  pop     ebx
end;
{$ELSEIF defined(CPUX64)}
asm
  // rcx = key, edx = len, r8d = seed

  push    rsi
  push    rbx

  mov     rbx, rcx                            // data := @key;
  lea     r11, [rcx+rdx]                      // pEnd := Pointer(NativeUInt(data) + len);
  lea     eax, [r8d+Prime5]                   // Result := seed + Prime5;
  cmp     edx, 16                             // if len >= 16 then
  jb      @tail

  lea     r10, [rcx+rdx-16]                   // limit := Pointer(NativeUInt(pEnd) - 16);

  lea     eax, [r8d+Prime1+Prime2]            // v1 := seed + Prime1 + Prime2;
  lea     ecx, [r8d+Prime2]                   // v2 := seed + Prime2;
                                              // v3 := seed;
  lea     r9d, [r8d-Prime1]                   // v4 := seed - Prime1;

@loop16bytes:
  imul    esi, [rbx], Prime2                  // v1 := Prime1 * rol(v1 + Prime2 * PCardinal(data)[0], 13);
  add     eax, esi
  rol     eax, 13
  imul    eax, eax, Prime1

  imul    esi, [rbx+4], Prime2                // v2 := Prime1 * rol(v2 + Prime2 * PCardinal(data)[1], 13);
  add     ecx, esi
  rol     ecx, 13
  imul    ecx, ecx, Prime1

  imul    esi, [rbx+8], Prime2                // v3 := Prime1 * rol(v3 + Prime2 * PCardinal(data)[2], 13);
  add     r8d, esi
  rol     r8d, 13
  imul    r8d, r8d, Prime1

  imul    esi, [rbx+12], Prime2               // v4 := Prime1 * rol(v4 + Prime2 * PCardinal(data)[3], 13);
  add     r9d, esi
  rol     r9d, 13
  imul    r9d, r9d, Prime1

  add     rbx, 16                             // Inc(NativeUInt(data), 16);
  cmp     rbx, r10                            // until NativeUInt(data) > NativeUInt(pLimit);
  jbe     @loop16bytes

  rol     eax, 1                              // Result := Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
  rol     ecx, 7
  add     eax, ecx
  rol     r8d, 12
  rol     r9d, 18
  add     eax, r8d
  add     eax, r9d

@tail:
  add     eax, edx                            // Inc(Result, len);
  lea     rcx, [r11-4]
  cmp     rcx, rbx                            // while NativeUInt(data) <= (NativeUInt(pEnd) - 4) do
  jb      @lessThan4bytes

@loop4bytes:
  imul    edx, [rbx], Prime3                  // Result := Result + PCardinal(data)^ * Prime3;
  add     edx, eax
  rol     edx, 17                             // Result := Rol(Result, 17) * Prime4;
  imul    eax, edx, Prime4
  add     rbx, 4                              // Inc(NativeUInt(data), 4);
  cmp     rbx, rcx                            // while NativeUInt(data) <= (NativeUInt(pEnd) - 4) do
  jbe     @loop4bytes

@lessThan4bytes:
  cmp     rbx, r11
  jnb     @finalization

@loopBytes:
  movzx   edx, byte ptr [rbx]                 // Result := Result + PByte(data)^ * Prime5;
  imul    edx, edx, Prime5
  add     edx, eax
  rol     edx, 11                             // Result := Rol(Result, 11) * Prime1;
  imul    eax, edx, Prime1
  inc     rbx                                 // Inc(NativeUInt(data));
  cmp     rbx, r11                            // while NativeUInt(data) < NativeUInt(pEnd) do
  jb      @loopBytes

@finalization:
  mov     ecx, eax                            // Result := Result xor (Result shr 15);
  shr     ecx, 15
  xor     eax, ecx
  imul    eax, eax, Prime2                    // Result := Result * Prime2;
  mov     ecx, eax                            // Result := Result xor (Result shr 13);
  shr     ecx, 13
  xor     eax, ecx
  imul    eax, eax, Prime3                    // Result := Result * Prime3;
  mov     ecx, eax                            // Result := Result xor (Result shr 16);
  shr     ecx, 16
  xor     eax, ecx

  pop rbx
  pop rsi
end;
{$IFEND}

function MurmurHash3_Int32(const key: Integer): Integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{$IF not defined(ASSEMBLER)}
var
  res: Cardinal;
begin
  res := Cardinal(key);
  res := RotateLeft(res * c1, r1);
  res := RotateLeft(res * c2, r2);

  res := (res * m + n) xor 4;

  res := (res xor (res shr 16)) * f1;
  res := (res xor (res shr 13)) * f2;
  res := res xor (res shr 16);
  Result := res;
end;
{$ELSEIF defined(CPUX86)}
asm
  imul    eax, eax, c1
  rol     eax, r1
  imul    eax, eax, c2
  rol     eax, r2
  lea     eax, [eax+n+eax*4]
  mov     edx, eax
  shr     eax, 16
  xor     edx, 4
  xor     eax, edx
  imul    eax, eax, f1
  mov     edx, eax
  shr     edx, 13
  xor     edx, eax
  imul    edx, edx, f2
  mov     eax, edx
  shr     eax, 16
  xor     eax, edx
end;
{$ELSE}
asm
  imul    ecx, ecx, c1
  rol     ecx, r1
  imul    ecx, ecx, c2
  rol     ecx, r2
  lea     eax, [rcx+n+rcx*4]
  mov     edx, eax
  shr     eax, 16
  xor     edx, 4
  xor     eax, edx
  imul    eax, eax, f1
  mov     edx, eax
  shr     edx, 13
  xor     edx, eax
  imul    edx, edx, f2
  mov     eax, edx
  shr     eax, 16
  xor     eax, edx
end;
{$IFEND}

function MurmurHash3(const key; len: Cardinal; seed: Integer = 0): Integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{$IF not defined(ASSEMBLER)}
var
  data: PCardinal;
  k: Cardinal;
  tail: PByte;
label
  case1, case2;
begin
  Result := seed;
  data := @key;
  {$POINTERMATH ON}
  tail := @data[len shr 2];
  while data < tail do
  {$POINTERMATH OFF}
  begin
    k := data^ * c1;
    k := RotateLeft(k, r1);
    k := k * c2;

    Result := Cardinal(Result) xor k;
    Result := RotateLeft(Result, r2);
    Result := Cardinal(Result) * m + n;

    Inc(data);
  end;

  k := 0;
  case Byte(len) and 3 of
    3:
    begin
      k := k or tail[2] shl 16;
      goto case2;
    end;
    2:
    case2:
    begin
      k := k or tail[1] shl 8;
      goto case1;
    end;
    1:
    case1:
    begin
      k := k or tail[0];
      k := k * c1;
      k := RotateLeft(k, r1);
      k := k * c2;
      Result := Cardinal(Result) xor k;
    end;
  end;

  Result := Cardinal(Result) xor len;

  Result := Result xor (Result shr 16);
  Result := Cardinal(Result) * f1;
  Result := Result xor (Result shr 13);
  Result := Cardinal(Result) * f2;
  Result := Result xor (Result shr 16);
end;
{$ELSEIF defined(CPUX86)}
asm
  push  ebx
  push  esi
  mov   ebx, eax
  mov   esi, ecx
  mov   ecx, edx
  and   ecx, not 3
  add   ecx, eax
  cmp   eax, ecx
  jnb    @remaining_bytes

@loop:
  imul  eax, [ebx], c1
  add   ebx, 4
  rol   eax, r1
  imul  eax, eax, c2
  xor   eax, esi
  rol   eax, r2
  lea   esi, [eax+n+eax*4]
  cmp   ecx, ebx
  ja    @loop

@remaining_bytes:
  mov   eax, edx
  and   eax, 3
  cmp   eax, 2
  je    @two_bytes
  cmp   eax, 3
  je    @three_bytes
  cmp   eax, 1
  je    @one_byte

@finalization:
  xor   esi, edx
  mov   edx, esi
  shr   edx, 16
  xor   edx, esi
  imul  edx, edx, f1
  mov   eax, edx
  shr   eax, 13
  xor   eax, edx
  imul  eax, eax, f2
  mov   edx, eax
  shr   edx, 16
  xor   eax, edx
  pop   esi
  pop   ebx
  ret

@one_byte:
  movzx eax, byte ptr [ecx]
  jmp   @tail

@three_bytes:
  movzx eax, word ptr [ecx]
  movzx ebx, byte ptr [ecx+2]
  sal   ebx, 16
  xor   eax, ebx
  jmp   @tail

@two_bytes:
  movzx eax, word ptr [ecx]
@tail:
  imul  eax, eax, c1
  rol   eax, 15
  imul  eax, eax, c2
  xor   esi, eax
  jmp   @finalization
end;
{$ELSE}
asm
  .noframe

  mov   r9d, edx
  and   edx, not 3
  mov   eax, r8d
  add   rdx, rcx
  cmp   rcx, rdx
  jnb   @remaining_bytes

@loop:
  imul  r8d, [rcx], c1
  add   rcx, 4
  rol   r8d, r1
  imul  r8d, r8d, c2
  xor   r8d, eax
  rol   r8d, r2
  lea   eax, [r8+n+r8*4]
  cmp   rdx, rcx
  ja   @loop

@remaining_bytes:
  mov   ecx, r9d
  and   ecx, 3
  cmp   ecx, 2
  je    @two_bytes
  cmp   ecx, 3
  je    @three_bytes
  cmp   ecx, 1
  je    @one_byte

@finalization:
  xor   eax, r9d
  mov   edx, eax
  shr   edx, 16
  xor   edx, eax
  imul  edx, edx, f1
  mov   eax, edx
  shr   eax, 13
  xor   eax, edx
  imul  eax, eax, f2
  mov   edx, eax
  shr   edx, 16
  xor   eax, edx
  ret

@one_byte:
  xor   ecx, ecx
@one_byte_noinit:
  movzx edx, byte ptr [rdx]
  xor   edx, ecx
  imul  edx, edx, c1
  rol   edx, 15
  imul  edx, edx, c2
  xor   eax, edx
  jmp   @finalization
@three_bytes:
  movzx r8d, byte ptr [rdx+2]
  sal   r8d, 16
@two_bytes_noinit:
  movzx ecx, byte ptr [rdx+1]
  sal   ecx, 8
  xor   ecx, r8d
  jmp   @one_byte_noinit
@two_bytes:
  xor   r8d, r8d
  jmp   @two_bytes_noinit
end;
{$IFEND}

end.
