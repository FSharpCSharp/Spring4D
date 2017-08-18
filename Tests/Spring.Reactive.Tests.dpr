program Spring.Reactive.Tests;

uses
  Spring.TestRunner,
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
  Spring.Reactive.Tests.Aggregate in 'Source\Reactive\Observable\Spring.Reactive.Tests.Aggregate.pas';

begin
  RunRegisteredTests;
  ReportMemoryLeaksOnShutdown := True;
end.
