unit Spring.Reactive.Observable.Range;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Producer,
  Spring.Reactive.Sink;

type
  TRange = class(TProducer<Integer>)
  private
    fStart: Integer;
    fCount: Integer;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<Integer>)
      private
        fParent: TRange;
        procedure LoopRec(const i: Integer; const recurse: Action<Integer>);
      public
        constructor Create(const parent: TRange;
          const observer: IObserver<Integer>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<Integer>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(start, count: Integer; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TRange'}

constructor TRange.Create(start, count: Integer; const scheduler: IScheduler);
begin
  inherited Create;
  fStart := start;
  fCount := count;
  fScheduler := scheduler;
end;

function TRange.Run(const observer: IObserver<Integer>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TRange.TSink'}

constructor TRange.TSink.Create(const parent: TRange;
  const observer: IObserver<Integer>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TRange.TSink.LoopRec(const i: Integer; const recurse: Action<Integer>);
begin
  if i < fParent.fCount then
  begin
    fObserver.OnNext(fParent.fStart + i);
    recurse(i + 1);
  end
  else
  begin
    fObserver.OnCompleted;
    Dispose;
  end;
end;

function TRange.TSink.Run: IDisposable;
begin
//  Result := Scheduler.Schedule<Integer>(fParent.fScheduler, 0, LoopRec);
  //TODO: do this properly
  Result := fParent.fScheduler.Schedule(
    procedure
    var
      i: Integer;
    begin
      for i := fParent.fStart to fParent.fStart + fParent.fCount - 1 do
        fObserver.OnNext(i);
      fObserver.OnCompleted;
    end);
end;

{$ENDREGION}


end.
