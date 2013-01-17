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
unit SvTesting.DUnit;

interface

uses
  TestFramework, Rtti, SysUtils;

type
  ETestTimeoutException = class(Exception);

  TClassOfException = class of Exception;

  TimeoutAttribute = class(TCustomAttribute)
  private
    FTimeout: Integer;
  public
    constructor Create(const ATimeout: Integer); virtual;

    property Timeout: Integer read FTimeout;
  end;

  TSvTestCase = class(TTestCase)
  private
    FTimeout: Integer;
  protected
    procedure Invoke(AMethod: TTestMethod); override;

    function IsTimeoutEnabled(): Boolean;
  public
    constructor Create(MethodName: string); override;
    destructor Destroy; override;

    procedure CheckEquals(expected, actual: TDate; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: TDateTime; msg: string = ''); overload; virtual;
    procedure CheckEqualsStringCaseInsensitive(const expected, actual: string; const msg: string = ''); virtual;
    procedure CheckNotEqualsStringCaseInsensitive(const expected, actual: string; const msg: string = ''); virtual;
    procedure CheckForException(aExceptionType: TClassOfException; aCode: TProc; const aMessage: string);

  end;

implementation

uses
  TypInfo,
  Diagnostics,
  SvRttiUtils,
  DateUtils;

{ TimeoutAttribute }

constructor TimeoutAttribute.Create(const ATimeout: Integer);
begin
  inherited Create();
  FTimeout := ATimeout;
end;

{ TTestCase }

procedure TSvTestCase.CheckEquals(expected, actual: TDate; msg: string);
begin
  FCheckCalled := True;
  if not SameDate(expected, actual) then
    FailNotEquals(DateToStr(expected), DateToStr(actual), msg, {$IF CompilerVersion > 23} ReturnAddress {$ELSE} ErrorAddr {$IFEND});
end;

procedure TSvTestCase.CheckEquals(expected, actual: TDateTime; msg: string);
begin
  FCheckCalled := True;
  if not SameDateTime(expected, actual) then
    FailNotEquals(DateTimeToStr(expected), DateTimeToStr(actual), msg, {$IF CompilerVersion > 23} ReturnAddress {$ELSE} ErrorAddr {$IFEND});
end;

procedure TSvTestCase.CheckEqualsStringCaseInsensitive(const expected, actual, msg: string);
begin
  FCheckCalled := True;
  if not AnsiSameText(expected, actual) then
    FailNotEquals(expected, actual, msg, {$IF CompilerVersion > 23} ReturnAddress {$ELSE} ErrorAddr {$IFEND});
end;

procedure TSvTestCase.CheckForException(aExceptionType: TClassOfException; aCode: TProc;
  const aMessage: String);
var
  WasException: Boolean;
begin
  WasException := False;
  try
    aCode;
  except
    on E: Exception do
    begin
      WasException := (E is aExceptionType);
    end;
  end;
  CheckTrue(WasException, aMessage);
end;

procedure TSvTestCase.CheckNotEqualsStringCaseInsensitive(const expected, actual, msg: string);
begin
  FCheckCalled := True;
  if AnsiSameText(expected, actual) then
    FailEquals(expected, actual, msg, {$IF CompilerVersion > 23} ReturnAddress {$ELSE} ErrorAddr {$IFEND});
end;

constructor TSvTestCase.Create(MethodName: string);
var
  FCtx: TRttiContext;
  FMethod: TRttiMethod;
  LAttribute: TimeoutAttribute;
begin
  inherited Create(MethodName);
  FTimeout := -1;
  FMethod := FCtx.GetType(Self.ClassInfo).GetMethod(MethodName);
  if FMethod.Visibility = mvPublished then
  begin
    LAttribute := TSvRtti.GetAttributeOfType<TimeoutAttribute>(FMethod);
    if Assigned(LAttribute) then
    begin
      FTimeout := LAttribute.Timeout;
    end;
  end;
end;

destructor TSvTestCase.Destroy;
begin
  inherited Destroy;
end;

procedure TSvTestCase.Invoke(AMethod: TTestMethod);
var
  LStopWatch: TStopwatch;
begin
  if IsTimeoutEnabled then
  begin
    LStopWatch := TStopwatch.StartNew;
  end;

  inherited;

  if IsTimeoutEnabled then
  begin
    LStopWatch.Stop;

    if LStopWatch.ElapsedMilliseconds > FTimeout then
      raise ETestTimeoutException.CreateFmt('Test timed out. Expected max timeout value %D ms, elapsed time: %D ms.',
        [FTimeout, LStopWatch.ElapsedMilliseconds]);
  end;
end;

function TSvTestCase.IsTimeoutEnabled: Boolean;
begin
  Result := (FTimeout <> -1);
end;

end.
