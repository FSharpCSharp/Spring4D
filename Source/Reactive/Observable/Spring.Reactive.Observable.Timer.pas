unit Spring.Reactive.Observable.Timer;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TTimer = class(TProducer<Integer>)
  private
    fDueTimeR: TTimeSpan;
    fPeriod: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<Integer>)
      private
        fParent: TTimer;
        fPeriod: TTimeSpan;
      public
        constructor Create(const parent: TTimer;
          const observer: IObserver<Integer>; const cancel: IDisposable);
        procedure Dispose; override;
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<Integer>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const dueTime, period: TTimeSpan; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TTimer'}

constructor TTimer.Create(const dueTime, period: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create;
  fDueTimeR := dueTime;
  fPeriod := period;
  fScheduler := scheduler;
end;

function TTimer.Run(const observer: IObserver<Integer>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  // TODO use nullable to support multiple ways

  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TTimer.TSink'}

constructor TTimer.TSink.Create(const parent: TTimer;
  const observer: IObserver<Integer>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fPeriod := fParent.fPeriod;
end;

procedure TTimer.TSink.Dispose;
begin
  inherited;
  fParent._Release;
end;

function TTimer.TSink.Run: IDisposable;
var
  dueTime: TTimeSpan;
  count: Integer;
begin
  dueTime := fParent.fDueTimeR;
  if dueTime = fPeriod then
  begin
    count := 0;
    Result := (fParent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(fPeriod,
    procedure
    begin
      if Assigned(fObserver) then
      begin
        fObserver.OnNext(count);
        Inc(count);
      end;
    end);
  end;
end;

{$ENDREGION}


end.
