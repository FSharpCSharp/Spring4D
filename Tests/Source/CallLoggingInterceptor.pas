unit CallLoggingInterceptor;

interface

uses
  HasCount,
  Spring.Services.Logging,
  Spring.Interception;

type
  TCallLoggingInterceptor = class(TInterfacedObject, IInterceptor, IHasCount)
  private
    fLogger: ILogger;
    fCount: Integer;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const logger: ILogger); overload;
    procedure Intercept(const invocation: IInvocation);
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils;


{$REGION 'TCallLoggingInterceptor'}

constructor TCallLoggingInterceptor.Create;
begin
//  fLogger := DefaultLogger;
end;

constructor TCallLoggingInterceptor.Create(const logger: ILogger);
begin
  fLogger := logger;
end;

function TCallLoggingInterceptor.GetCount: Integer;
begin
  Result := fCount;
end;

procedure TCallLoggingInterceptor.Intercept(const invocation: IInvocation);
begin
//  fLogger.EnterMethod(invocation.Target, invocation.Method.Name);
  try
    try
      Inc(fCount);
      invocation.Proceed;
    except
      on E: Exception do
      begin
//        fLogger.LogException(E);
        raise;
      end;
    end;
  finally
//    fLogger.LeaveMethod(invocation.Target, invocation.Method.Name);
  end;
end;

{$ENDREGION}


end.
