(* SvClasses.pas
* Created: 2012-01-31 15:23:54
* Copyright (c) 2012, Linas Naginionis
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

unit SvClasses;

interface

uses
  Rtti, SysUtils;

type
  {$REGION 'Tuples'}

  ESvTupleException = class(Exception);
  /// <summary>
  /// Simple Tuple
  /// </summary>
  TSvSimpleTuple = record
  private
    FValues: array of TValue;
    function GetValue(const AIndex: Integer): TValue;
    procedure SetValue(const AIndex: Integer; const Value: TValue);
    function GetCount: Integer;
  public
    function Create<T>(const AVal1: T): TSvSimpleTuple; overload;
    function Create<T1, T2>(const AVal1: T1; const AVal2: T2): TSvSimpleTuple; overload;
    function Create(const AValues: array of TValue): TSvSimpleTuple; overload;

    function First: TValue;
    function Last: TValue;

    property Count: Integer read GetCount;
    property Value[const AIndex: Integer]: TValue read GetValue write SetValue;
  end;

  TSvTuple<T> = record
  private
    FValue: T;
  public
    constructor Create(const AVal: T);

    property Value1: T read FValue write FValue;
  end;

  TSvTuple<T1, T2> = record
  private
    FValue1: T1;
    FValue2: T2;
  public
    constructor Create(const AVal1: T1; const AVal2: T2);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
  end;

  TSvTuple<T1, T2, T3> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
  end;

  TSvTuple<T1, T2, T3, T4> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
    FValue4: T4;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3; const AVal4: T4);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
    property Value4: T4 read FValue4 write FValue4;
  end;

  TSvTuple<T1, T2, T3, T4, T5> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
    FValue4: T4;
    FValue5: T5;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3; const AVal4: T4;
      const AVal5: T5);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
    property Value4: T4 read FValue4 write FValue4;
    property Value5: T5 read FValue5 write FValue5;
  end;

  TSvTuple<T1, T2, T3, T4, T5, T6> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
    FValue4: T4;
    FValue5: T5;
    FValue6: T6;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3; const AVal4: T4;
      const AVal5: T5; const AVal6: T6);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
    property Value4: T4 read FValue4 write FValue4;
    property Value5: T5 read FValue5 write FValue5;
    property Value6: T6 read FValue6 write FValue6;
  end;

  TSvTuple<T1, T2, T3, T4, T5, T6, T7> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
    FValue4: T4;
    FValue5: T5;
    FValue6: T6;
    FValue7: T7;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3; const AVal4: T4;
      const AVal5: T5; const AVal6: T6; const AVal7: T7);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
    property Value4: T4 read FValue4 write FValue4;
    property Value5: T5 read FValue5 write FValue5;
    property Value6: T6 read FValue6 write FValue6;
    property Value7: T7 read FValue7 write FValue7;
  end;

  TSvTuple<T1, T2, T3, T4, T5, T6, T7, T8> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
    FValue4: T4;
    FValue5: T5;
    FValue6: T6;
    FValue7: T7;
    FValue8: T8;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3; const AVal4: T4;
      const AVal5: T5; const AVal6: T6; const AVal7: T7; const AVal8: T8);

    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
    property Value4: T4 read FValue4 write FValue4;
    property Value5: T5 read FValue5 write FValue5;
    property Value6: T6 read FValue6 write FValue6;
    property Value7: T7 read FValue7 write FValue7;
    property Value8: T8 read FValue8 write FValue8;
  end;

  {$ENDREGION}

  {$REGION 'Doc'}
  /// <summary>
  /// record for building file paths
  /// </summary>
  /// <example>
  /// <code>
  ///  function GetPath: string;
  ///  var
  ///  pBuilder: TPathBuilder;
  ///  begin
  ///    pBuilder := TPathBuilder.InitCurrentDir;
  ///    Result := pBuilder.Add('bin').Add('etc').Add('test').ToString;
  ///    //result is appdir\bin\etc\test\ in Windows
  ///    //result is appdir/bin/etc/test/ in Mac OSX
  ///  end;
  /// </code>
  /// </example>
  {$ENDREGION}
  TPathBuilder = record
  private
    sPath: string;
  public
    function Add(const APath: string; IncludeTrailingDelimiter: Boolean = True): TPathBuilder;
    function AddFile(const AFilename: string): TPathBuilder;
    function ToString(): string;
    class function Init(): TPathBuilder; static;
    class function InitCurrentDir(): TPathBuilder; static;
    class function InitHomePath(): TPathBuilder; static;
    class function InitCustomPath(const APath: string): TPathBuilder; static;
  end;


  ESvEnumException = class(Exception);
  {$REGION 'Doc'}
  /// <summary>
  /// Generic enumeration type
  /// <remarks>
  /// Should be used only for enumerations!
  /// </remarks>
  /// </summary>
  {$ENDREGION}
  TSvEnum<T> = record
  private
    FValue: NativeInt;
  private
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    class operator Implicit(const AEnum: TSvEnum<T>): T; inline;
    class operator Implicit(const AEnum: T): TSvEnum<T>; inline;
    class operator Implicit(const AEnum: TSvEnum<T>): Integer; inline;
    class operator Implicit(const AEnum: Integer): TSvEnum<T>; inline;

    function ToString(): string;

    property Value: T read GetValue write SetValue;
  end;


implementation

uses
  TypInfo;


{$REGION 'Tuples Implementation'}
{ TSvSimpleTuple<T> }

function TSvSimpleTuple.Create<T1, T2>(const AVal1: T1; const AVal2: T2): TSvSimpleTuple;
begin
  Result := Create([TValue.From<T1>(AVal1), TValue.From<T2>(AVal2)]);
end;

function TSvSimpleTuple.Create<T>(const AVal1: T): TSvSimpleTuple;
begin
  Result := Create([TValue.From<T>(AVal1)]);
end;


function TSvSimpleTuple.First: TValue;
begin
  if Count > 0 then
    Result := FValues[Low(FValues)]
  else
    raise ESvTupleException.Create('Tuple is empty');
end;

function TSvSimpleTuple.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TSvSimpleTuple.GetValue(const AIndex: Integer): TValue;
begin
  Result := FValues[AIndex];
end;

function TSvSimpleTuple.Last: TValue;
begin
  if Count > 0 then
    Result := FValues[High(FValues)]
  else
    raise ESvTupleException.Create('Tuple is empty');
end;

procedure TSvSimpleTuple.SetValue(const AIndex: Integer; const Value: TValue);
begin
  FValues[AIndex] := Value;
end;

function TSvSimpleTuple.Create(const AValues: array of TValue): TSvSimpleTuple;
var
  i: Integer;
begin
  SetLength(FValues, Length(AValues));
  for i := Low(AValues) to High(AValues) do
  begin
    FValues[i] := AValues[i];
  end;
  Result := Self;
end;

{ TSvTuple<T> }

constructor TSvTuple<T>.Create(const AVal: T);
begin
  FValue := AVal;
end;

{ TSvTuple<T1, T2> }

constructor TSvTuple<T1, T2>.Create(const AVal1: T1; const AVal2: T2);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
end;


{ TSvTuple<T1, T2, T3> }

constructor TSvTuple<T1, T2, T3>.Create(const AVal1: T1; const AVal2: T2; const AVal3: T3);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
end;

{ TSvTuple<T1, T2, T3, T4> }

constructor TSvTuple<T1, T2, T3, T4>.Create(const AVal1: T1; const AVal2: T2; const AVal3: T3;
  const AVal4: T4);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
  FValue4 := AVal4;
end;

{ TSvTuple<T1, T2, T3, T4, T5> }

constructor TSvTuple<T1, T2, T3, T4, T5>.Create(const AVal1: T1; const AVal2: T2; const AVal3: T3;
  const AVal4: T4; const AVal5: T5);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
  FValue4 := AVal4;
  FValue5 := AVal5;
end;

{ TSvTuple<T1, T2, T3, T4, T5, T6> }

constructor TSvTuple<T1, T2, T3, T4, T5, T6>.Create(const AVal1: T1; const AVal2: T2;
  const AVal3: T3; const AVal4: T4; const AVal5: T5; const AVal6: T6);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
  FValue4 := AVal4;
  FValue5 := AVal5;
  FValue6 := AVal6;
end;

{ TSvTuple<T1, T2, T3, T4, T5, T6, T7> }

constructor TSvTuple<T1, T2, T3, T4, T5, T6, T7>.Create(const AVal1: T1; const AVal2: T2;
  const AVal3: T3; const AVal4: T4; const AVal5: T5; const AVal6: T6; const AVal7: T7);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
  FValue4 := AVal4;
  FValue5 := AVal5;
  FValue6 := AVal6;
  FValue7 := AVal7;
end;

{ TSvTuple<T1, T2, T3, T4, T5, T6, T7, T8> }

constructor TSvTuple<T1, T2, T3, T4, T5, T6, T7, T8>.Create(const AVal1: T1; const AVal2: T2;
  const AVal3: T3; const AVal4: T4; const AVal5: T5; const AVal6: T6; const AVal7: T7;
  const AVal8: T8);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
  FValue4 := AVal4;
  FValue5 := AVal5;
  FValue6 := AVal6;
  FValue7 := AVal7;
  FValue8 := AVal8;
end;
{$ENDREGION}

{$REGION 'TPathBuilder Implementation'}
{ TPathBuilder }

function TPathBuilder.Add(const APath: string;
  IncludeTrailingDelimiter: Boolean): TPathBuilder;
begin
  if IncludeTrailingDelimiter then
  begin
    sPath := sPath + IncludeTrailingPathDelimiter(APath);
  end
  else
  begin
    sPath := sPath + APath;
  end;

  Result := Self;
end;

function TPathBuilder.AddFile(const AFilename: string): TPathBuilder;
begin
  sPath := sPath + AFilename;
  Result := Self;
end;

class function TPathBuilder.Init: TPathBuilder;
begin
  Result.sPath := '';
end;

class function TPathBuilder.InitCurrentDir: TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
end;

class function TPathBuilder.InitCustomPath(const APath: string): TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(APath);
end;

class function TPathBuilder.InitHomePath: TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(GetHomePath);
end;

function TPathBuilder.ToString: string;
begin
  Result := sPath;
end;
{$ENDREGION}

{$REGION 'TSvEnum<T> Implementation'}
{ TSvEnum<T> }

function TSvEnum<T>.GetValue: T;
begin
  Result := TValue.FromOrdinal(TypeInfo(T), FValue).AsType<T>;
end;

class operator TSvEnum<T>.Implicit(const AEnum: TSvEnum<T>): T;
begin
  Result := AEnum.Value;
end;

class operator TSvEnum<T>.Implicit(const AEnum: T): TSvEnum<T>;
begin
  Result.Value := AEnum;
end;

class operator TSvEnum<T>.Implicit(const AEnum: TSvEnum<T>): Integer;
begin
  Result := AEnum.FValue;
end;

class operator TSvEnum<T>.Implicit(const AEnum: Integer): TSvEnum<T>;
begin
  Result.FValue := AEnum;
end;

procedure TSvEnum<T>.SetValue(const Value: T);
var
  AValue: TValue;
begin
  AValue := TValue.From<T>(Value);
  if AValue.Kind = tkEnumeration then
    FValue := AValue.AsOrdinal
  else
    raise ESvEnumException.Create('Incorrect value type. Can set only enumerations.');
end;

function TSvEnum<T>.ToString: string;
begin
  Result := GetEnumName(TypeInfo(T), FValue);
end;
{$ENDREGION}

end.
