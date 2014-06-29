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
unit SvLogging.Log4D;

interface

uses
  SvLogging
  ,Log4D
  ,SysUtils
  ;

type
  TLog4DLogger = class(TSvBaseLogger)
  private
    FLogger: TLogLogger;
  protected
    function GetLogLevel(ASvLevel: TSvLogLevel): TLogLevel;
    function ToSvLogLevel(ALevel: TLogLevel): TSvLogLevel;

    function GetLevel: TSvLogLevel; override;
    procedure SetLevel(const Value: TSvLogLevel); override;
  public
    procedure Log(ALevel: TSvLogLevel; const AMessage: string; AException: Exception = nil); override;

    constructor Create(const ALoggerName: string); virtual;
    destructor Destroy; override;

    property Logger: TLogLogger read FLogger;
  end;

implementation

var
  LOG_LEVELS: array[TSvLogLevel] of TLogLevel;

{ TLog4DLogger }

constructor TLog4DLogger.Create(const ALoggerName: string);
begin
  inherited Create;
  if FileExists('log4d.props') then
    TLogPropertyConfigurator.Configure('log4d.props');
  FLogger := TLogLogger.GetLogger(ALoggerName);
end;

destructor TLog4DLogger.Destroy;
begin
  //do not free FLogger since log4D will free it on finalization
  inherited Destroy;
end;

function TLog4DLogger.GetLevel: TSvLogLevel;
begin
  Result := ToSvLogLevel(FLogger.Level);
end;

function TLog4DLogger.GetLogLevel(ASvLevel: TSvLogLevel): TLogLevel;
begin
  Result := LOG_LEVELS[ASvLevel];
end;

procedure TLog4DLogger.Log(ALevel: TSvLogLevel; const AMessage: string; AException: Exception = nil);
begin
  FLogger.Log(GetLogLevel(ALevel), AMessage, AException);
end;

procedure TLog4DLogger.SetLevel(const Value: TSvLogLevel);
begin
  FLogger.Level := GetLogLevel(Value);
end;

function TLog4DLogger.ToSvLogLevel(ALevel: TLogLevel): TSvLogLevel;
begin
  for Result := Low(LOG_LEVELS) to High(LOG_LEVELS) do
  begin
    if (LOG_LEVELS[Result] = ALevel) then
    begin
      Exit;
    end;
  end;
  Result := TSvLogLevel.Off;
end;

procedure RegisterLogLevels();
begin
  LOG_LEVELS[TSvLogLevel.Off] := Log4D.Off;
  LOG_LEVELS[TSvLogLevel.Fatal] := Log4D.Fatal;
  LOG_LEVELS[TSvLogLevel.Error] := Log4D.Error;
  LOG_LEVELS[TSvLogLevel.Warn] := Log4D.Warn;
  LOG_LEVELS[TSvLogLevel.Info] := Log4D.Info;
  LOG_LEVELS[TSvLogLevel.Debug] := Log4D.Debug;
  LOG_LEVELS[TSvLogLevel.Trace] := Log4D.Trace;
  LOG_LEVELS[TSvLogLevel.All] := Log4D.All;
end;

initialization
  RegisterLogLevels();

end.
