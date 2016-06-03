unit Spring.Reactive.Concurrency.SchedulerDefaults;

interface

uses
  Spring.Reactive;

type
  SchedulerDefaults = record
  strict private
    class var fConstantTimeOperations: IScheduler;
    class var fTimeBasedOperations: IScheduler;
  public
    class constructor Create;
    class destructor Destroy;

    class property ConstantTimeOperations: IScheduler read fConstantTimeOperations;
    class property Iteration: IScheduler read fConstantTimeOperations; // TODO
    class property TimeBasedOperations: IScheduler read fTimeBasedOperations;
  end;

implementation

uses
  Spring.Reactive.Concurrency.DefaultScheduler,
  Spring.Reactive.Concurrency.ImmediateScheduler;


{$REGION 'SchedulerDefaults'}

class constructor SchedulerDefaults.Create;
begin
  fConstantTimeOperations := TImmediateScheduler.Instance;
  fTimeBasedOperations := TDefaultScheduler.Instance;
end;

class destructor SchedulerDefaults.Destroy;
begin
  fConstantTimeOperations := nil;
  fTimeBasedOperations := nil;
end;

{$ENDREGION}


end.
