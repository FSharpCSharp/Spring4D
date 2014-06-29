(* SvClasses.pas
* Created: 2012-12-21 10:18:54
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
unit SvLogging;

interface

uses
  SysUtils
  ;

type
  {$SCOPEDENUMS ON}
  TSvLogLevel = (
    Off,
    Fatal,
    Error,
    Warn,
    Info,
    Debug,
    Trace,
    All
  );
  {$SCOPEDENUMS OFF}

  ISvLogger = interface(IInvokable)
  ['{38711C0A-B5D1-4147-A05C-B54D86BAA650}']
    procedure Fatal(const AMessage: string; AException: Exception = nil);
    procedure Error(const AMessage: string; AException: Exception = nil);
    procedure Warn(const AMessage: string; AException: Exception = nil);
    procedure Info(const AMessage: string; AException: Exception = nil);
    procedure Debug(const AMessage: string; AException: Exception = nil);
    procedure Trace(const AMessage: string; AException: Exception = nil);

    procedure Log(ALevel: TSvLogLevel; const AMessage: string; AException: Exception = nil);
    procedure LogFmt(ALevel: TSvLogLevel; const AMessage: string; const AParameters: array of const; AException: Exception = nil);

    function GetLevel: TSvLogLevel;
    procedure SetLevel(const Value: TSvLogLevel);
    property Level: TSvLogLevel read GetLevel write SetLevel;
  end;

  TSvBaseLogger = class(TInterfacedObject, ISvLogger)
  protected
    function GetLevel: TSvLogLevel; virtual; abstract;
    procedure SetLevel(const Value: TSvLogLevel); virtual; abstract;
  public
    procedure Fatal(const AMessage: string; AException: Exception = nil);
    procedure Error(const AMessage: string; AException: Exception = nil);
    procedure Warn(const AMessage: string; AException: Exception = nil);
    procedure Info(const AMessage: string; AException: Exception = nil);
    procedure Debug(const AMessage: string; AException: Exception = nil);
    procedure Trace(const AMessage: string; AException: Exception = nil);

    procedure Log(ALevel: TSvLogLevel; const AMessage: string; AException: Exception = nil); virtual; abstract;
    procedure LogFmt(ALevel: TSvLogLevel; const AMessage: string; const AParameters: array of const; AException: Exception = nil); virtual;

    property Level: TSvLogLevel read GetLevel write SetLevel;
  end;


function Logger(): ISvLogger;

procedure CreateLogger(var ALogger: ISvLogger);

implementation

type
  ESvLoggerException = class(Exception);

var
  FLogger: ISvLogger = nil;

function Logger(): ISvLogger;
begin
  if not Assigned(FLogger) then
    raise ESvLoggerException.Create('Logger not created. You must "CreateLogger" before using it.');
  Result := FLogger;
end;

procedure CreateLogger(var ALogger: ISvLogger);
begin
  FLogger := ALogger;
end;

{ TSvBaseLogger }

procedure TSvBaseLogger.Debug(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Debug, AMessage, AException);
end;

procedure TSvBaseLogger.Error(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Error, AMessage, AException);
end;

procedure TSvBaseLogger.Fatal(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Fatal, AMessage, AException);
end;

procedure TSvBaseLogger.Info(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Info, AMessage, AException);
end;

procedure TSvBaseLogger.LogFmt(ALevel: TSvLogLevel; const AMessage: string; const AParameters: array of const; AException: Exception = nil);
begin
  Log(ALevel, Format(AMessage, AParameters), AException);
end;

procedure TSvBaseLogger.Trace(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Trace, AMessage, AException);
end;

procedure TSvBaseLogger.Warn(const AMessage: string; AException: Exception);
begin
  Log(TSvLogLevel.Warn, AMessage, AException);
end;

end.
