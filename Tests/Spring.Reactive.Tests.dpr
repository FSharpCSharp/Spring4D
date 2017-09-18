program Spring.Reactive.Tests;

uses
{$IFDEF LEAKCHECK}
  LeakCheck,
  LeakCheck.Utils,
  LeakCheck.Cycle,
  LeakCheck.DUnit,
  LeakCheck.DUnitCycle,
  TestFramework,
  Classes,
  Rtti,
  Threading,
{$ENDIF}
  TestInsight.DUnit,
  Utilities in 'Utilities.pas',
  Spring.Reactive.Testing in 'Source\Reactive\Testing\Spring.Reactive.Testing.pas',
  Spring.Reactive.Testing.HotObservable in 'Source\Reactive\Testing\Spring.Reactive.Testing.HotObservable.pas',
  Spring.Reactive.Testing.MockObserver in 'Source\Reactive\Testing\Spring.Reactive.Testing.MockObserver.pas',
  Spring.Reactive.Testing.ReactiveTest in 'Source\Reactive\Testing\Spring.Reactive.Testing.ReactiveTest.pas',
  Spring.Reactive.Testing.Recorded in 'Source\Reactive\Testing\Spring.Reactive.Testing.Recorded.pas',
  Spring.Reactive.Testing.Subscription in 'Source\Reactive\Testing\Spring.Reactive.Testing.Subscription.pas',
  Spring.Reactive.Testing.TestScheduler in 'Source\Reactive\Testing\Spring.Reactive.Testing.TestScheduler.pas',
  Spring.Reactive.Tests.CurrentThreadScheduler in 'Source\Reactive\Concurrency\Spring.Reactive.Tests.CurrentThreadScheduler.pas',
  Spring.Reactive.Tests.DefaultScheduler in 'Source\Reactive\Concurrency\Spring.Reactive.Tests.DefaultScheduler.pas',
  Spring.Reactive.Tests.Disposables in 'Source\Reactive\Disposables\Spring.Reactive.Tests.Disposables.pas',
  Spring.Reactive.Tests.ImmediateScheduler in 'Source\Reactive\Concurrency\Spring.Reactive.Tests.ImmediateScheduler.pas',
  Spring.Reactive.Tests.Scheduler in 'Source\Reactive\Concurrency\Spring.Reactive.Tests.Scheduler.pas',
  Spring.Reactive.Tests.Subject in 'Source\Reactive\Subjects\Spring.Reactive.Tests.Subject.pas',
  Spring.Reactive.Tests.AsyncLock in 'Source\Reactive\Concurrency\Spring.Reactive.Tests.AsyncLock.pas',
  Spring.Reactive.Tests.Aggregate in 'Source\Reactive\Observable\Spring.Reactive.Tests.Aggregate.pas',
  Spring.Reactive.Tests.Multiple in 'Source\Reactive\Observable\Spring.Reactive.Tests.Multiple.pas';

function IgnoreQueueWorkerThread(const Instance: TObject; ClassType: TClass): Boolean;
var
  ctx: TRttiContext;
  obj: TObject;
begin
  Result := ClassType.ClassName = 'TThreadPool.TQueueWorkerThread';
  if Result then
  begin
    obj := ctx.GetType(ClassType).GetField('FRunningEvent').GetValue(Instance).AsObject;
    RegisterExpectedMemoryLeak(obj);
    RegisterExpectedMemoryLeak(ctx.GetType(obj.ClassType).GetField('FLock').GetValue(obj).AsObject);

    obj := ctx.GetType(ClassType).GetField('FWorkQueue').GetValue(Instance).AsObject;
    RegisterExpectedMemoryLeak(obj);
    RegisterExpectedMemoryLeak(ctx.GetType(obj.ClassType).GetField('FForeignLock').GetValue(obj).AsObject);
  end;
end;

procedure InitLeakCheck;
begin
{$IFDEF LEAKCHECK}
  MemLeakMonitorClass := TLeakCheckGraphMonitor;

  TTask.Run(procedure begin end);
  TThread.CurrentThread;

  TLeakCheck.IgnoredLeakTypes := [tkUnknown, tkUnicodeString];
  TLeakCheck.InstanceIgnoredProc := IgnoreMultipleObjects;
  AddIgnoreObjectProc([
    IgnoreRttiObjects,
    IgnoreAnonymousMethodPointers,
    IgnoreCustomAttributes,
    IgnoreQueueWorkerThread
  ]);
{$ENDIF}
end;

begin
  InitLeakCheck;
  RunRegisteredTests;
  ReportMemoryLeaksOnShutdown := True;
end.
