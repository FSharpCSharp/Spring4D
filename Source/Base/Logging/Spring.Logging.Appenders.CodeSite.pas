unit Spring.Logging.Appenders.CodeSite;

interface

uses
  Spring.Logging;

type
  TCodeSiteAppender = class(TInterfacedObject, ILogAppender, ILoggerProperties)
  private
    fEnabled: Boolean;
    fLevels: TLogLevels;
  public
    constructor Create;

    function GetLevels: TLogLevels;
    function GetEnabled: Boolean;

    procedure SetLevels(value: TLogLevels);
    procedure SetEnabled(value: Boolean);

    procedure Send(const entry: TLogEntry);

    property Enabled: Boolean read GetEnabled write fEnabled;
    property Levels: TLogLevels read fLevels write fLevels;
  end;

implementation

uses
  SysUtils,
  Spring.Logging.Appenders.Base,
  CodeSiteLogging;

{ TCodeSiteAppender }

constructor TCodeSiteAppender.Create;
begin
  inherited;
  fEnabled := true;
  fLevels := LOG_BASIC_LEVELS;
end;

function TCodeSiteAppender.GetEnabled: Boolean;
begin
  Result := fEnabled and CodeSite.Enabled;
end;

function TCodeSiteAppender.GetLevels: TLogLevels;
begin
  Result := fLevels;
end;

procedure TCodeSiteAppender.Send(const entry: TLogEntry);
begin
  if not (entry.Level in Levels) or not Enabled then Exit;

  if entry.Color = clDefault then
    CodeSite.CategoryColor := $FFFFFF
  else CodeSite.CategoryColor := entry.Color;

  case entry.EntryType of
    TLogEntryType.Text:
      if Assigned(entry.Exception) then
        CodeSite.SendException(TLogAppenderBase.FormatMsg(entry), entry.Exception)
      else
        case entry.Level of
          TLogLevel.Unknown: ;

          TLogLevel.Verbose:
            CodeSite.SendNote(entry.Msg);

          TLogLevel.Debug,
          TLogLevel.CallStack,
          TLogLevel.SerializedData,
          TLogLevel.Text:
            CodeSite.SendMsg(entry.Msg);

          TLogLevel.Info:
            CodeSite.SendReminder(entry.Msg);

          TLogLevel.Warning:
            CodeSite.SendWarning(entry.Msg);

          TLogLevel.Error,
          TLogLevel.Fatal:
            CodeSite.SendError(entry.Msg);
        end;

    TLogEntryType.Entering:
      CodeSite.EnterMethod(TLogAppenderBase.FormatMethodName(entry.ClassType,
        entry.Msg));

    TLogEntryType.Leaving:
      CodeSite.ExitMethod(TLogAppenderBase.FormatMethodName(entry.ClassType,
        entry.Msg));
  end;
end;

procedure TCodeSiteAppender.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

procedure TCodeSiteAppender.SetLevels(value: TLogLevels);
begin
  fLevels := value;
end;

end.
