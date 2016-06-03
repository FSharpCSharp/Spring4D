unit Spring.Reactive.Concurrency.DefaultScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.LocalScheduler;

type
  TDefaultScheduler = class(TLocalScheduler, ISchedulerPeriodic)
  private
    class var fInstance: IScheduler;
  public
    class constructor Create;
    class destructor Destroy;

    function Schedule(const action: Action): IDisposable; override;
    function SchedulePeriodic(const period: TTimeSpan;
      const action: Action): IDisposable;

    class property Instance: IScheduler read fInstance;
  end;

implementation

uses
  Classes,
  SysUtils,
  Threading,
  Spring.Reactive.Disposables;


{$REGION 'TDefaultScheduler'}

class constructor TDefaultScheduler.Create;
begin
  fInstance := TDefaultScheduler.Create;
end;

class destructor TDefaultScheduler.Destroy;
begin
  fInstance := nil;
end;

function TDefaultScheduler.Schedule(const action: Action): IDisposable;
begin
  // TODO abstract away to make this pluggable
  TThreadPool.Default.QueueWorkItem(
    procedure
    begin
      action();
    end);
end;

function TDefaultScheduler.SchedulePeriodic(const period: TTimeSpan;
  const action: Action): IDisposable;
var
  periodInMilliseconds: Integer;
  isDisposed: Boolean;
begin
  periodInMilliseconds := Trunc(period.TotalMilliseconds);
  isDisposed := False;
  TTask.Run(
    procedure
    begin
      while not isDisposed do
      begin

        // naive impl without canceling
        TThread.Sleep(periodInMilliseconds);
        action;
      end;
    end);
  Result := TAnonymousDisposable.Create(
    procedure
    begin
      isDisposed := True;
    end);
end;

{$ENDREGION}


end.
