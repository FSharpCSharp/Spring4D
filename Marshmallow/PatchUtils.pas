unit PatchUtils;

interface

// Thanks to Andreas Hausladen

function FindMethodBytes(StartAddress: Pointer; const Bytes: array of SmallInt; MaxCount: Integer): PByte;
function GetActualAddr(Proc: Pointer): Pointer;
function GetVirtualMethod(Instance: TObject; VMTOffset: Integer): Pointer;
procedure RedirectFunction(OrgProc, NewProc: Pointer);

implementation

uses
  SysUtils, Windows;

function FindMethodBytes(StartAddress: Pointer; const Bytes: array of SmallInt; MaxCount: Integer): PByte;

  function XCompareMem(P: PByte; C: PSmallint; Len: Integer): Boolean;
  begin
    while (Len > 0) and ((C^ = -1) or (P^ = Byte(C^))) do
    begin
      Dec(Len);
      Inc(P);
      Inc(C);
    end;
    Result := Len = 0;
  end;

var
  FirstByte: Byte;
  EndAddress: PByte;
  Len: Integer;
begin
  FirstByte := Bytes[0];
  Len := Length(Bytes) - 1;
  Result := StartAddress;
  EndAddress := Result + MaxCount;
  while Result < EndAddress do
  begin
    while (Result < EndAddress) and (Result[0] <> FirstByte) do
      Inc(Result);
    if (Result < EndAddress) and XCompareMem(Result + 1, @Bytes[1], Len) then
      Exit;
    Inc(Result);
  end;
  Result := nil;
end;

function GetActualAddr(Proc: Pointer): Pointer;
type
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;
begin
  Result := Proc;
  if (Proc <> nil) and (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
    Result := PAbsoluteIndirectJmp(Proc).Addr^;
end;

function GetVirtualMethod(Instance: TObject; VMTOffset: Integer): Pointer;
asm
  mov ecx, [eax]
  mov eax, [ecx + edx]
end;

procedure RedirectFunction(OrgProc, NewProc: Pointer);
type
  TJmpBuffer = packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
{$IF COMPILERVERSION < 23}
  n: Cardinal;
{$ELSE}
  n: NativeUInt;
{$IFEND}
  JmpBuffer: TJmpBuffer;
begin
  JmpBuffer.Jmp := $E9;
  JmpBuffer.Offset := PByte(NewProc) - (PByte(OrgProc) + 5);
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @JmpBuffer, SizeOf(JmpBuffer), n) then
    RaiseLastOSError;
end;

end.
