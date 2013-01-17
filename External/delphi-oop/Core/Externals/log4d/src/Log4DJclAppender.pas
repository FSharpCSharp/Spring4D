unit Log4DJclAppender;

{
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}

{
  This unit contains appender that uses the JCL.
}

interface

uses
  Log4D;

type

  // File Appender, that expands Environment variables in the filename.
  TLogFileAppenderEx = class(TLogFileAppender)
  protected
    procedure SetLogFile(const Name: string); override;
  end;

implementation

uses
  JclSysInfo;

{ TLogFileAppenderEx }

procedure TLogFileAppenderEx.SetLogFile(const Name: string);
var
  Value: string;
begin
  Value := Name;
  ExpandEnvironmentVar(Value);
  inherited SetLogFile(Value);
end;

initialization
  Log4D.RegisterAppender(TLogFileAppenderEx);

end.
