program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4,
  Spring,
  Spring.Console,
  Spring.Collections,
  Spring.Collections.Generators,
  Spring.Coroutines,
  Spring.Reactive.ObservableBase,
  Spring.Reactive.Subjects.Subject,
  Spring.Reactive,
  SysUtils;

type
  TLocation = record
    Latitude: Double;
    Longitude: Double;
    constructor Create(latitude, longitude: Double);
  end;

{ TLocation }

constructor TLocation.Create(latitude, longitude: Double);
begin
  Self.Latitude := latitude;
  Self.Longitude := longitude;
end;

type
  TLocationReporter = class(TDisposableObject, IObserver<TLocation>)
  private
    fName: string;
    fUnsubscriber: IDisposable;
  public
    constructor Create(const name: string);

    procedure OnNext(const value: TLocation);
    procedure OnError(const error: Exception);
    procedure OnCompleted;

    procedure Subscribe(const provider: IObservable<TLocation>); virtual;
    procedure Unsubscribe;
  end;

{ TLocationReporter }

constructor TLocationReporter.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

procedure TLocationReporter.OnCompleted;
begin
  Writeln('The Location Tracker has completed transmitting data to ', fName);
end;

procedure TLocationReporter.OnError(const error: Exception);
begin
  Writeln(fName, ': The location cannot be determined.');
end;

procedure TLocationReporter.OnNext(const value: TLocation);
begin
  Writeln(Format('%2:s: The current location is %0:.4f, %1:.4f', [value.Latitude, value.Longitude, fName]));
end;

procedure TLocationReporter.Subscribe(const provider: IObservable<TLocation>);
begin
  if Assigned(provider) then
    fUnsubscriber := provider.Subscribe(Self);
end;

procedure TLocationReporter.Unsubscribe;
begin
  fUnsubscriber.Dispose;
end;

type
  TLocationTracker = class(TObservableBase<TLocation>)
  private
    fObservers: IList<IObserver<TLocation>>;

    type
      TUnsubscriber = class(TDisposableObject)
      private
        fObservers: IList<IObserver<TLocation>>;
        fObserver: IObserver<TLocation>;
      public
        constructor Create(const observers: IList<IObserver<TLocation>>;
          const observer: IObserver<TLocation>);
        procedure Dispose; override;
      end;

  public
    constructor Create;

    function Subscribe(const observer: IObserver<TLocation>): IDisposable; override;

    procedure TrackLocation(const loc: Nullable<TLocation>);
    procedure EndTransmission;
  end;

  ELocationUnknownException = class(Exception)
  end;

{ TLocationTracker }

constructor TLocationTracker.Create;
begin
  inherited Create;
  fObservers := TCollections.CreateList<IObserver<TLocation>>;
end;

procedure TLocationTracker.EndTransmission;
var
  observer: IObserver<TLocation>;
begin
  for observer in fObservers.ToArray do
    if fObservers.Contains(observer) then
      observer.OnCompleted;
  fObservers.Clear;
end;

function TLocationTracker.Subscribe(
  const observer: IObserver<TLocation>): IDisposable;
begin
  if not fObservers.Contains(observer) then
    fObservers.Add(observer);
  Result := TUnsubscriber.Create(fObservers, observer);
end;

procedure TLocationTracker.TrackLocation(const loc: Nullable<TLocation>);
var
  observer: IObserver<TLocation>;
begin
  for observer in fObservers.ToArray do
    if not loc.HasValue then
      observer.OnError(ELocationUnknownException.Create(''))
    else
      observer.OnNext(loc);
end;

{ TLocationTracker.TUnsubscriber }

constructor TLocationTracker.TUnsubscriber.Create(
  const observers: IList<IObserver<TLocation>>;
  const observer: IObserver<TLocation>);
begin
  inherited Create;
  fObservers := observers;
  fObserver := observer;
end;

procedure TLocationTracker.TUnsubscriber.Dispose;
begin
  if Assigned(fObserver) then
    fObservers.Remove(fObserver);
  inherited;
end;

procedure Main;
var
  provider: TLocationTracker;
  reporter1, reporter2: TLocationReporter;
begin
  provider := TLocationTracker.Create;
  reporter1 := TLocationReporter.Create('FixedGPS');
  reporter1.Subscribe(provider);
  reporter2 := TLocationReporter.Create('MobileGPS');
  reporter2.Subscribe(provider);

  provider.TrackLocation(TLocation.Create(47.6456, -122.1312));
  reporter1.Unsubscribe;
  provider.TrackLocation(TLocation.Create(47.6677, -122.1199));
  provider.TrackLocation(Nullable.Null);
  provider.EndTransmission;
end;

procedure Main2;
var
  range: IObservable<Integer>;
begin
  range := Observable.Range(10, 15);
  range
    .SkipWhile(
    function(const i: Integer): Boolean
    begin
      Result := i < 15;
    end)
    .TakeWhile(
    function(const i: Integer): Boolean
    begin
      Result := i < 20;
    end)
    .Subscribe(
    procedure(const i: Integer)
    begin
      Writeln(i);
    end,
    procedure
    begin
      Writeln('Completed');
    end);
end;

procedure Main3;
var
  subject: ISubject<Integer>;
begin
  subject := TSubject<Integer>.Create;
  subject
    .SkipLast(2)
    .Subscribe(
      procedure(const value: Integer)
      begin
        Writeln(value);
      end,
      procedure begin Writeln('completed') end);
  Writeln('Pushing 1');
  subject.OnNext(1);
  Writeln('Pushing 2');
  subject.OnNext(2);
  Writeln('Pushing 3');
  subject.OnNext(3);
  Writeln('Pushing 4');
  subject.OnNext(4);
  subject.OnCompleted();

  Writeln('----------');

  subject := TSubject<Integer>.Create;
  subject
    .TakeLast(2)
    .Subscribe(
      procedure(const value: Integer)
      begin
        Writeln(value);
      end,
      procedure begin Writeln('completed') end);
  Writeln('Pushing 1');
  subject.OnNext(1);
  Writeln('Pushing 2');
  subject.OnNext(2);
  Writeln('Pushing 3');
  subject.OnNext(3);
  Writeln('Pushing 4');
  subject.OnNext(4);
  Writeln('Completing');
  subject.OnCompleted();

//  Sleep(1000); // to ensure that the completed is processed before method is left and subscription is being disposed
end;

procedure Main4;
var
  subject: ISubject<Integer>;
  any, all: IObservable<Boolean>;
begin
  subject := TSubject<Integer>.Create;
  subject
    .Subscribe(
    procedure(const value: Integer)
    begin
      Writeln(value);
    end,
    procedure begin Writeln('subject completed') end);
  any := subject.Any;
  any.Subscribe(
    procedure(const b: Boolean)
    begin
      Writeln('The subject has any values? ', b);
    end);
//  subject.OnNext(1);
  subject.OnCompleted;

  Writeln('----------');

  subject := TSubject<Integer>.Create;
  subject
    .Subscribe(
    procedure(const value: Integer)
    begin
      Writeln(value);
    end,
    procedure begin Writeln('subject completed') end);
  all := subject.All(function(const i: Integer): Boolean begin Result := i < 5; end);
  all.Subscribe(
    procedure(const b: Boolean)
    begin
      Writeln('All values less than 5? ', b);
    end);
  subject.OnNext(1);
  subject.OnNext(2);
  subject.OnNext(6);
  subject.OnNext(2);
  subject.OnNext(1);
  subject.OnCompleted;
end;

function IsEven(const x: Integer): Boolean;
begin
  Result := not Odd(x);
end;

procedure WriteLine(const value: Integer); overload;
begin
  Writeln(value);
end;

procedure WriteLine(const value: string); overload;
begin
  Writeln(value);
end;


function PriceIncrease(const w: IList<Integer>): Double;
begin
  Result := (w.Last - w.First) div (w.First * 100);
end;

procedure Fibonacci;
var
  i: IObservable<Integer>;
begin
  i := Observable.From<Integer>([1, 2, 3, 5, 8, 13, 21, 34, 55, 89]);
  Observable
    .Buffer<Integer>(i, 2, 1).Where(
    function(const w: IList<Integer>): Boolean
    begin
      Result := w.Last - w.First > 10;
    end)
    .ForEach(
    procedure(const w: IList<Integer>)
    begin
      Writeln(w[0], '->', w[1]);
    end);
end;

function GetInput: IEnumerable<string>;
begin
  Result := TGenerator<string>.Create(
    procedure
    var
      s: string;
    begin
      while True do
      begin
        Readln(s);
        if s = '' then Break;
        Yield(s);
      end;
    end);
end;

procedure WorkshopEventProcessing;
var
  src: IObservable<string>;
  res: IObservable<IGroupedObservable<Integer,string>>;
begin
  src := Enumerable.ToObservable<string>(GetInput);

  res := Observable.GroupBy<string,Integer>(src,
     function(const s: string): Integer
     begin
       Result := Length(s);
     end);

  res.ForEach(
    procedure(const g: IGroupedObservable<Integer,string>)
    begin
      Writeln('New group with length = ', g.Key);
      g.Subscribe(
        procedure(const x: string)
        begin
          Writeln('  ', x, ' member of ', g.Key);
        end);
    end);
end;

procedure SkipUntil;
var
  subject: ISubject<Integer>;
  otherSubject: ISubject<Integer>;
begin
  subject := TSubject<Integer>.Create;
  otherSubject := TSubject<Integer>.Create;
  Observable.SkipUntil<Integer,Integer>(subject, otherSubject).Subscribe(
    procedure(const i: Integer)
    begin
      Writeln(i);
    end,
    procedure
    begin
      Writeln('Completed');
    end);
  subject.OnNext(1);
  subject.OnNext(2);
  subject.OnNext(3);
  otherSubject.OnNext(0);
  subject.OnNext(4);
  subject.OnNext(5);
  subject.OnNext(6);
  subject.OnNext(7);
  subject.OnNext(8);
  subject.OnCompleted;
end;

procedure TakeUntil;
var
  subject: ISubject<Integer>;
  otherSubject: ISubject<Integer>;
begin
  subject := TSubject<Integer>.Create;
  otherSubject := TSubject<Integer>.Create;
  Observable.TakeUntil<Integer,Integer>(subject, otherSubject).Subscribe(
    procedure(const i: Integer)
    begin
      Writeln(i);
    end,
    procedure
    begin
      Writeln('Completed');
    end);
  subject.OnNext(1);
  subject.OnNext(2);
  subject.OnNext(3);
  otherSubject.OnNext(0);
  subject.OnNext(4);
  subject.OnNext(5);
  subject.OnNext(6);
  subject.OnNext(7);
  subject.OnNext(8);
  subject.OnCompleted;
end;

procedure Interval;
begin
  Observable.Interval(TTimeSpan.FromMilliseconds(500))
    .Take(10)
    .Subscribe(procedure(const i: Integer) begin Writeln(i) end);
end;

begin
  try
//    WorkshopEventProcessing;
//    SkipUntil;
//    TakeUntil;
    Interval;
    ReportMemoryLeaksOnShutdown := True;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
