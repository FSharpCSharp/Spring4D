unit Spring.Patches.QC98671;

interface

procedure ApplyPatch;

implementation

uses
  PatchUtils,
  Rtti,
  SysUtils,
  TypInfo,
  Windows;

type
  PInterceptFrame = Pointer;

  PParamLoc = ^TParamLoc;
  TParamLoc = record
    FTypeInfo: PTypeInfo;
    FByRefParam: Boolean;
    FOffset: Integer;
    procedure SetArg(AFrame: PInterceptFrame; const Value: TValue);
  end;

var
  TParamLoc_SetArg: procedure(var Self: TParamLoc; AFrame: PInterceptFrame; const Value: TValue);

procedure TParamLoc.SetArg(AFrame: PInterceptFrame; const Value: TValue);
begin
  // Fix from XE2
  if FByRefParam then
    TParamLoc_SetArg(Self, AFrame, Value);
end;

procedure ApplyPatch;
const
  SetArgCallBytes: array[0..18] of SmallInt = (
    $8D, $04, $5B, // lea eax,[ebx+ebx*2]
    $8B, $55, $F0, // mov edx,[ebp-$10]
    $8D, $0C, $C2, // lea ecx,[edx+eax*8]
    $8B, $55, $FC, // mov edx,[ebp-$04]
    $8D, $04, $82, // lea eax,[edx+eax*4]
    $8B, $55, $F4, // mov edx,[ebp-$0c]
    $E8            // call TMethodImplementation.TParamLoc.SetArg
  );
var
  ctx: TRttiContext;
  p: PByte;
  offset: Integer;
  n: UINT_PTR;
begin
{$IF CompilerVersion = 22}
  // Get the code pointer of the TMethodImplementation.TInvokeInfo.GetParamLocs method for which
  // extended RTTI is available to find the private types TInvokeInfo private method SaveArguments.
  p := ctx.GetType(TMethodImplementation).GetField('FInvokeInfo').FieldType.GetMethod('GetParamLocs').CodeAddress;

  // Find for the "locs[i].SetArg(AFrame, Args[i]);" call and replace it with a call to our function.
  p := FindMethodBytes(p - 128 - SizeOf(SetArgCallBytes), SetArgCallBytes, 128 - SizeOf(SetArgCallBytes));
  if p <> nil then
  begin
    // Replace the call to SetArg with our method.
    @TParamLoc_SetArg := (p + 19 + 4) + PInteger(@p[19])^;
    offset := PByte(@TParamLoc.SetArg) - (p + 19 + 4);
    if not WriteProcessMemory(GetCurrentProcess, p + 19, @offset, SizeOf(offset), n) then
      RaiseLastOSError;
  end;
{$IFEND}
end;

initialization
  ApplyPatch;

end.
