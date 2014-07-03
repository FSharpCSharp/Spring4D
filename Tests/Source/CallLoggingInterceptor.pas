unit CallLoggingInterceptor;

interface

uses
  DSharp.Logging,
  HasCount,
  Spring.Services.Logging,
  Spring.Interception;

type
  TCallLoggingInterceptor = class(TInterfacedObject, IInterceptor, IHasCount)
  private
    fLogger: ILog;
    fCount: Integer;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const logger: ILog); overload;
    procedure Intercept(const invocation: IInvocation);
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils;


{$REGION 'TCallLoggingInterceptor'}

constructor TCallLoggingInterceptor.Create;
begin
  fLogger := Logging;
end;

constructor TCallLoggingInterceptor.Create(const logger: ILog);
begin
  fLogger := logger;
end;

function TCallLoggingInterceptor.GetCount: Integer;
begin
  Result := fCount;
end;

procedure TCallLoggingInterceptor.Intercept(const invocation: IInvocation);
begin
  fLogger.EnterMethod(invocation.Target, invocation.Method.Name);
  try
    try
      Inc(fCount);
      invocation.Proceed;
    except
      on E: Exception do
      begin
        fLogger.LogException(E);
        raise;
      end;
    end;
  finally
    fLogger.LeaveMethod(invocation.Target, invocation.Method.Name);
  end;
end;

{$ENDREGION}


end.
