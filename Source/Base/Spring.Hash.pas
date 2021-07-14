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

unit Spring.Hash;

interface

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
    v1 := Seed + Prime1 + Prime2;
    v2 := Seed + Prime2;
    v3 := Seed;
    v4 := Seed - Prime1;

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
    Result := Seed + Prime5;

  Inc(Result, len);

  while data <= pEnd - 4 do
  begin
    Result := Result + PCardinal(data)^ * Prime3;
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
  Result := Result * Prime2;
  Result := Result xor (Result shr 13);
  Result := Result * Prime3;
  Result := Result xor (Result shr 16);
end;
{$ELSEIF defined(CPUX86)}
asm
  push    ebx
  push    esi
  push    edi
  push    ebp
  push    edx

  mov     ebx, eax                            // data := @key;
  lea     esi, [eax+edx]                      // pEnd := Pointer(NativeUInt(data) + len);
  lea     eax, [ecx+Prime5]                   // Result := seed + cPrime32x5;
  cmp     edx, 15                             // if len >= 16 then
  jbe     @tail

  mov     ebp, ecx
  sub     esi, 16                             // limit := Pointer(NativeUInt(pEnd) - 16);

  lea     eax, [ebp+Prime1+Prime2]            // v1 := seed + Prime1 + Prime2;
  lea     edx, [ebp+Prime2]                   // v2 := seed + Prime2;
                                              // v3 := seed;
  lea     edi, [ebp-Prime1]                   // v4 := seed - Prime1;

@loop16bytes:
  imul    ecx, [ebx], Prime2                  // v1 := Prime1 * rol(v1 + Prime2 * PCardinal(data)[0], 13);
  add     ebx, 16
  add     eax, ecx
  rol     eax, 13
  imul    eax, eax, Prime1

  imul    ecx, [ebx-12], Prime2               // v2 := Prime1 * rol(v2 + Prime2 * PCardinal(data)[1], 13);
  add     edx, ecx
  rol     edx, 13
  imul    edx, edx, Prime1

  imul    ecx, [ebx-8], Prime2                // v3 := Prime1 * rol(v3 + Prime2 * PCardinal(data)[2], 13);
  add     ecx, ebp
  rol     ecx, 13
  imul    ecx, ecx, Prime1
  mov     ebp, ecx

  imul    ecx, [ebx-4], Prime2                // v4 := Prime1 * rol(v4 + Prime2 * PCardinal(data)[3], 13);
  add     ecx, edi
  rol     ecx, 13
  imul    edi, ecx, Prime1

  cmp     ebx, esi                            // until NativeUInt(data) > NativeUInt(pLimit);
  jbe     @loop16bytes

  add     esi, 16
  rol     eax, 1                              // Result := Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
  rol     edx, 7
  add     eax, edx
  rol     ebp, 12
  rol     edi, 18
  add     edi, ebp
  add     eax, edi

@tail:
  lea     ebp, [esi-4]
  add     eax, [esp]                          // Inc(Result, Len);
  cmp     ebp, ebx
  jb      @lessThan4bytes

@loop4bytes:
  imul    edx, [ebx], Prime3                  // Result := Result + PCardinal(data)[0] * Prime3;
  add     ebx, 4                              // Inc(NativeUInt(data), 4);
  add     edx, eax
  rol     edx, 17                             // Result := Rol(Result, 17) * Prime4;
  imul    eax, edx, Prime4
  cmp     ebp, ebx                            // while NativeUInt(data) <= (NativeUInt(pEnd) - 4) do
  jnb     @loop4bytes

@lessThan4bytes:
  cmp     ebx, esi
  jnb     @finalization

@loopBytes:
  movzx   edx, byte ptr [ebx]                 // Result := Result + PByte(data)^ * Prime5;
  inc     ebx
  imul    edx, edx, Prime5
  add     edx, eax
  rol     edx, 11                             // Result := Rol(Result, 11) * Prime1;
  imul    eax, edx, Prime1
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
     //xxHashTest.dpr.124: begin
     push rsi
     push rbx
     //xxHashTest.dpr.125: ABuffer:= @HashData;
     mov rbx,rcx
     //xxHashTest.dpr.126: pEnd:= pointer(NativeUInt(ABuffer) + Len);
     //movsxd rax,edx
     lea r11,[rcx+rdx]
     //xxHashTest.dpr.127: if Len >= 16 then begin
     cmp edx,$10
     jl @SmallHash
     //xxHashTest.dpr.128: pLimit:= pointer(NativeUInt(pEnd) - 16);
     lea r10,[rcx+rdx-$10]
     //xxHashTest.dpr.129: v1:= Seed + cPrime32x1 + cPrime32x2;
     lea eax,[r8d+$24234428]
     //xxHashTest.dpr.130: v2:= Seed + cPrime32x2;
     lea ecx,[r8d+$85ebca77]
     //xxHashTest.dpr.131: v3:= Seed;
     mov r9d,r8d
     //xxHashTest.dpr.132: v4:= Seed - cPrime32x1;
     add r8d,$61c8864f
     //xxHashTest.dpr.135: v1:= cPrime32x1 * rol(v1 + cPrime32x2 * PCardinal(data)[0], 13);
@StartRepeat16:
     imul esi,[rbx],$85ebca77
     add eax,esi
     rol eax,$0d
     imul eax,eax,$9e3779b1
     //xxHashTest.dpr.136: v2:= cPrime32x1 * rol(v2 + cPrime32x2 * PCardinal(data)[1], 13);
     imul esi,[rbx+$04],$85ebca77
     add ecx,esi
     rol ecx,$0d
     imul ecx,ecx,$9e3779b1
     //xxHashTest.dpr.137: v3:= cPrime32x1 * rol(v3 + cPrime32x2 * PCardinal(data)[2], 13);
     imul esi,[rbx+$08],$85ebca77
     add r9d,esi
     rol r9d,$0d
     imul r9d,r9d,$9e3779b1
     //xxHashTest.dpr.138: v4:= cPrime32x1 * rol(v4 + cPrime32x2 * PCardinal(data)[3], 13);
     imul esi,[rbx+$0c],$85ebca77
     add r8d,esi
     rol r8d,$0d
     imul r8d,r8d,$9e3779b1
     //xxHashTest.dpr.139: Inc(NativeUInt(ABuffer), 16);
     add rbx,$10
     //xxHashTest.dpr.140: until not(NativeUInt(ABuffer) <= NativeUInt(pLimit));
     cmp rbx,r10
     jbe @StartRepeat16
     //xxHashTest.dpr.142: Result:= Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
     rol eax,1
     rol ecx,$07
     add eax,ecx
     rol r9d,$0c
     add eax,r9d
     rol r8d,$12
     add eax,r8d
     jmp @Tail
@SmallHash:
     //xxHashTest.dpr.144: else Result:= Seed + cPrime32x5;
     lea eax,[r8d+$165667b1]
     //xxHashTest.dpr.146: Inc(Result, Len);
@Tail:
     add eax,edx
     jmp @EndWhile4Bytes
@StartWhile4Bytes:
     //xxHashTest.dpr.149: Result:= Result + PCardinal(ABuffer)^ * cPrime32x3;
     imul ecx,[rbx],$c2b2ae3d
     add ecx,eax
     //xxHashTest.dpr.150: Result:= Rol(Result, 17) * cPrime32x4;
     //mov ecx,eax
     rol ecx,$11
     imul eax,ecx,$27d4eb2f
     //mov eax,ecx
     //xxHashTest.dpr.151: Inc(NativeUInt(ABuffer), 4);
     add rbx,$04
     //xxHashTest.dpr.148: while NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 4) do begin
@EndWhile4Bytes:
     lea rcx,[r11-$04]
     cmp rbx,rcx
     jbe @StartWhile4Bytes
     jmp @EndWhileBytes
     //xxHashTest.dpr.155: Result:= Result + PByte(ABuffer)^ * cPrime32x5;
@StartWhileBytes:
     movzx rcx,byte ptr [rbx]
     imul ecx,ecx,$165667b1
     add eax,ecx
     //xxHashTest.dpr.156: Result:= Rol(Result, 11) * cPrime32x1;
     //mov ecx,eax
     rol eax,$0b
     imul eax,eax,$9e3779b1
     //mov eax,ecx
     //xxHashTest.dpr.157: Inc(NativeUint(ABuffer));
     add rbx,$01
     //xxHashTest.dpr.154: while NativeUInt(ABuffer) < NativeUInt(pEnd) do begin
@EndWhileBytes:
     cmp rbx,r11
     jb @StartWhileBytes
     //xxHashTest.dpr.160: Result:= Result xor (Result shr 15);
     mov ecx,eax
     shr ecx,$0f
     xor eax,ecx
     //xxHashTest.dpr.161: Result:= Result * cPrime32x2;
     imul eax,eax,$85ebca77
     //xxHashTest.dpr.162: Result:= Result xor (Result shr 13);
     mov ecx,eax
     shr ecx,$0d
     xor eax,ecx
     //xxHashTest.dpr.163: Result:= Result * cPrime32x3;
     imul eax,eax,$c2b2ae3d
     //xxHashTest.dpr.164: Result:= Result xor (Result shr 16);
     mov ecx,eax
     shr ecx,$10
     xor eax,ecx
     //xxHashTest.dpr.165: end;
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
begin
  Result := RotateLeft(key * c1, r1);
  Result := RotateLeft(Result * c2, r2);

  Result := (Result * m + n) xor 4;

  Result := (Result xor (Result shr 16)) * f1;
  Result := (Result xor (Result shr 13)) * f2;
  Result := Result xor (Result shr 16);
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

    Result := Result xor k;
    Result := RotateLeft(Result, r2);
    Result := Result * m + n;

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
      Result := Result xor k;
    end;
  end;

  Result := Result xor len;

  Result := Result xor (Result shr 16);
  Result := Result * f1;
  Result := Result xor (Result shr 13);
  Result := Result * f2;
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
