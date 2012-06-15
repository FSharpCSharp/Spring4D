(* SvBindings.Converters.pas
* Created: 2011-12-28 16:57:49
* Copyright (c) 2011, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net or linas@vikarina.lt
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

unit SvBindings.Converters;

interface

uses
  DSharp.Core.DataConversion, TypInfo, Classes;

type
  TEnumConverter = class(TValueConverter)
  private
    ATypeInfo: PTypeInfo;
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

  TListConverter = class(TValueConverter)
  private
    ATypeInfo: PTypeInfo;
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

  TVariantConverter = class(TValueConverter)
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

  TDateTimeYearConverter = class(TValueConverter)
  private
    vDate: TDateTime;
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

  TDateTimeMonthConverter = class(TValueConverter)
  private
    vDate: TDateTime;
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

  TDateTimeFirstDayConverter = class(TValueConverter)
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

implementation

uses
  Rtti,
  SysUtils,
  Variants,
  DateUtils;

{ TEnumConverter }

function TEnumConverter.Convert(const Value: TValue): TValue;
begin
  ATypeInfo := Value.TypeInfo;
  //from source to target
  if Value.Kind = tkEnumeration then
  begin
    Result := GetEnumValue(Value.TypeInfo, Value.ToString);
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TEnumConverter.ConvertBack(const Value: TValue): TValue;
begin
  //from target to source
  //target ItemIndex - Integer
  if not Value.TryCast(ATypeInfo, Result) then
  begin
    Result := TValue.FromOrdinal(ATypeInfo, Value.AsOrdinal);
  end;
end;

{ TListConverter }

function TListConverter.Convert(const Value: TValue): TValue;
var
  mtdEnum, mtdMoveNext: TRttiMethod;
  ctx: TRttiContext;
  rType, enumType: TRttiType;
  lEnumerator: TValue;
  propCurrent: TRttiProperty;
  sResult: string;
begin
  ATypeInfo := Value.TypeInfo;
  sResult := '';
  //from source to target
  //source (Value) TList<> , target (Result): string
  if Value.Kind in [tkClass] then
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

function TListConverter.ConvertBack(const Value: TValue): TValue;
var
  mtdAdd, mtdCreate: TRttiMethod;
  ctx: TRttiContext;
  rType: TRttiType;
  AValue, ANewValue: TValue;
  sResult: string;
  slItems: TStringList;
  i, iValue: Integer;
  arrParams: TArray<TRttiParameter>;
  instanceType: TRttiInstanceType;
begin
  //from target to source
  // target (Value) string, source (Result) TList<>
  sResult := Value.AsString;

  ctx := TRttiContext.Create;
  rType := ctx.GetType(ATypeInfo);
  //create new instance
  for mtdCreate in rType.GetMethods do
  begin
    if (mtdCreate.IsConstructor) and (Length(mtdCreate.GetParameters) = 0) then
    begin
      instanceType := rType.AsInstance;

      Result := mtdCreate.Invoke(instanceType.MetaclassType, []);

      Break;
    end;
  end;

  mtdAdd := rType.GetMethod('Add');
  if Assigned(mtdAdd) then
  begin
    arrParams := mtdAdd.GetParameters;

    if Length(arrParams)=1 then
    begin
      slItems := TStringList.Create;
      try
        slItems.Delimiter := ',';
        slItems.StrictDelimiter := True;
        slItems.DelimitedText := sResult;

        for i := 0 to slItems.Count - 1 do
        begin
          if not TryStrToInt(slItems[i], iValue) then
          begin
            AValue := slItems[i];
          end
          else
          begin
            AValue := iValue;
          end;

          if not AValue.TryCast(arrParams[0].ParamType.Handle, ANewValue) then
          begin
            TValue.Make(nil, arrParams[0].ParamType.Handle, ANewValue);

            ANewValue := iValue;
          end;

          mtdAdd.Invoke(Result, [ANewValue]);
        end;

      finally
        slItems.Free;
      end;
    end;
  end;
end;

{ TVariantConverter }

function TVariantConverter.Convert(const Value: TValue): TValue;
var
  AVal: Variant;
begin
  Result := '';
  //from source to target
  // source (Value) Variant, target (Result) string
  if Value.IsEmpty then
    Exit;

  case Value.Kind of
    tkInteger: Result := IntToStr(Value.AsInteger);
    tkFloat: Result := FloatToStr(Value.AsExtended);
    tkVariant:
    begin
      AVal := Value.AsVariant;
      if not VarIsNull(AVal) and not VarIsEmpty(AVal) then
      begin
        Result := VarToStrDef(AVal, '');
      end;
    end;
    tkInt64: Result := IntToStr(Value.AsInt64);
    tkWString, tkUString, tkLString, tkChar, tkString, tkWChar:
      Result := Value.AsString
    else
    begin
      Exit;
    end;
  end;
end;

function TVariantConverter.ConvertBack(const Value: TValue): TValue;
var
  AVal: string;
  ARes: Variant;
  ResInt, i: Integer;
  ResFloat: Double;
  ResDate: TDateTime;
begin
  //from target to source
  // target (Value) string, source (Result) Variant
  if Value.IsEmpty then
  begin
    Exit(TValue.FromVariant(Unassigned));
  end;

  if Value.Kind in [tkWString, tkUString, tkLString, tkChar, tkString, tkWChar] then
  begin
    AVal := Value.AsString;

    if AVal = '' then
      Exit(TValue.FromVariant(Unassigned));

    if not TryStrToInt(AVal, ResInt) then
    begin
      //change decimal
      for i := 1 to Length(AVal) do
      begin
        if CharInSet(AVal[i], ['.', ',']) then
          AVal[i] := FormatSettings.DecimalSeparator;
      end;

      if not TryStrToFloat(AVal, ResFloat) then
      begin
        //try datetime
        for i := 1 to Length(AVal) do
        begin
          if CharInSet(AVal[i], ['-', '/', '\']) then
            AVal[i] := FormatSettings.DateSeparator;
        end;

        if not TryStrToDate(AVal, ResDate) then
        begin
          Exit(TValue.FromVariant(Unassigned));
        end
        else
        begin
          //irasom kaip string, nes TValue neturi tkDate tipo, todel veliau convert'inant gauname
          //float tipo reiksme, o ne TDateTime
          ARes := FormatDateTime('yyyy-mm-dd', ResDate);
        end;
      end
      else
      begin
        ARes := ResFloat;
      end;

    end
    else
    begin
      ARes := ResInt;
    end;

    Result := TValue.FromVariant(ARes);

  end
  else
  begin
    Exit(TValue.FromVariant(Unassigned));
  end;
end;

{ TDateTimeYearConverter }

function TDateTimeYearConverter.Convert(const Value: TValue): TValue;
begin
  //from source to target
  // source (Value) TDateTime, target (Result) Word - Year
  if Value.TryAsType<TDateTime>(vDate) then
  begin
    Result := YearOf(vDate);
  end;
end;

function TDateTimeYearConverter.ConvertBack(const Value: TValue): TValue;
begin
  //from target to source
  // source (Value) Word - Year, target (Result) TDateTime
  Result := EncodeDate(Value.AsInteger, MonthOf(vDate), DayOf(vDate));
end;

{ TDateTimeMonthConverter }

function TDateTimeMonthConverter.Convert(const Value: TValue): TValue;
begin
  //from source to target
  // source (Value) TDateTime, target (Result) Word - Month
  if Value.TryAsType<TDateTime>(vDate) then
  begin
    Result := MonthOf(vDate);
  end;
end;

function TDateTimeMonthConverter.ConvertBack(const Value: TValue): TValue;
begin
  //from target to source
  // source (Value) Word - Year, target (Result) TDateTime
  Result := EncodeDate(YearOf(vDate), Value.AsInteger, DayOf(vDate));
end;

{ TDateTimeFirstDayConverter }

function TDateTimeFirstDayConverter.Convert(const Value: TValue): TValue;
var
  vDate: TDateTime;
begin
  if Value.TryAsType<TDateTime>(vDate) then
  begin
    Result := StartOfTheMonth(vDate);
  end;
end;

function TDateTimeFirstDayConverter.ConvertBack(const Value: TValue): TValue;
var
  vDate: TDateTime;
begin
  if Value.TryAsType<TDateTime>(vDate) then
  begin
    Result := StartOfTheMonth(vDate);
  end;
end;

end.
