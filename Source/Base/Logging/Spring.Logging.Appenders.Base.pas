unit Spring.Logging.Appenders.Base;

interface

uses
  Spring.Logging;

type
  {$REGION 'TLogAppenderBase'}
  TLogAppenderBase = class(TInterfacedObject, ILogAppender, ILoggerProperties)
  private
    fEnabled: Boolean;
    fLevels: TLogLevels;

    function GetLevels: TLogLevels;
    function GetEnabled: Boolean;

    procedure SetLevels(value: TLogLevels);
    procedure SetEnabled(value: Boolean);
  protected const
    {$REGION 'Helper constants and functions'}
    //May or may not be used by descendants, its here just for convenience
    LEVEL : array[TLogLevel] of string = (
      '[UNKNOWN]',
      '[VERBOSE]',
      '', //CallStack
      '', //SerializedData
      '[DEBUG]',
      '[TEXT]',
      '[INFO]',
      '[WARN]',
      '[ERROR]',
      '[FATAL]'
    );
    LEVEL_FIXED : array[TLogLevel] of string = (
      '[UNK  ]',
      '[VERB ]',
      '', //CallStack
      '', //SerializedData
      '[DEBUG]',
      '[TEXT ]',
      '[INFO ]',
      '[WARN ]',
      '[ERROR]',
      '[FATAL]'
    );
    class function FormatMsg(const entry: TLogEntry): string; static; inline;
    {$ENDREGION}
  protected
    function IsEnabled(level: TLogLevel): Boolean; inline;
    procedure DoSend(const entry: TLogEntry); virtual; abstract;
  public
    constructor Create;

    procedure Send(const entry: TLogEntry);

    property Enabled: Boolean read fEnabled write fEnabled;
    property Levels: TLogLevels read fLevels write fLevels;
  end;
  {$ENDREGION}
implementation

{$REGION 'TLogAppenderBase'}
{ TLogAppenderBase }

constructor TLogAppenderBase.Create;
begin
  inherited;
  fEnabled := true;
  fLevels := LOG_BASIC_LEVELS;
end;

class function TLogAppenderBase.FormatMsg(const entry: TLogEntry): string;
begin
  if (entry.Exc = nil) then
    Result := entry.Msg
  else
  begin
    if (entry.Msg <> '') then
      Result := entry.Msg + ', ' + entry.Exc.ClassName
    else Result := entry.Exc.ClassName;
    if (entry.Exc.Message <> '') then
      Result := Result + ': ' + entry.Exc.Message;
  end;
end;

function TLogAppenderBase.GetEnabled: Boolean;
begin
  Result := Enabled;
end;

function TLogAppenderBase.GetLevels: TLogLevels;
begin
  Result := Levels;
end;

function TLogAppenderBase.IsEnabled(level: TLogLevel): Boolean;
begin
  Result:=fEnabled and (level in fLevels);
end;

procedure TLogAppenderBase.Send(const entry: TLogEntry);
begin
  if (IsEnabled(entry.Level)) then
    DoSend(entry);
end;
procedure TLogAppenderBase.SetEnabled(value: Boolean);
begin
  Enabled := value;
end;

procedure TLogAppenderBase.SetLevels(value: TLogLevels);
begin
  Levels := value;
end;
{$ENDREGION}

end.
