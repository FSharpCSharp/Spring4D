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
unit Core.Utils;

interface

uses
  Rtti, TypInfo;

type
  TUtils = class sealed
  public
    class function AsVariant(const AValue: TValue): Variant;
    class function FromVariant(const AValue: Variant): TValue;
    class function TryConvert(const AFrom: TValue; ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
  end;

implementation

uses
  Core.Reflection
  ;

{ TUtils }

class function TUtils.AsVariant(const AValue: TValue): Variant;
begin
  case AValue.Kind of
    tkEnumeration:
    begin
      if AValue.TypeInfo = TypeInfo(Boolean) then
        Result := AValue.AsBoolean
      else
        Result := AValue.AsOrdinal;
    end;
    tkFloat:
    begin
      if (AValue.TypeInfo = TypeInfo(TDateTime)) then
        Result := AValue.AsType<TDateTime>
      else if (AValue.TypeInfo = TypeInfo(TDate)) then
        Result := AValue.AsType<TDate>
      else
        Result := AValue.AsExtended;
    end
    else
    begin
      Result := AValue.AsVariant;
    end;
  end;
end;


class function TUtils.FromVariant(const AValue: Variant): TValue;
begin
  Result := TValue.FromVariant(AValue);
end;

class function TUtils.TryConvert(const AFrom: TValue; ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
begin
  if (AFrom.TypeInfo <> ATypeInfo) then
  begin
    Result := AFrom.TryConvert(ATypeInfo, AResult);
  end
  else
  begin
    Result := True;
    AResult := AFrom;
  end;
end;

end.
