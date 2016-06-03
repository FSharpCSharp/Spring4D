unit Spring.Reactive.TimeInterval;

interface

uses
  TimeSpan;

type
  TTimeInterval<T> = record
  private
    fInterval: TTimeSpan;
    fValue: T;
  public
    constructor Create(const value: T; const interval: TTimeSpan);
    property Interval: TTimeSpan read fInterval;
    property Value: T read fValue;
  end;

implementation


{$REGION 'TTimeInterval<T>'}

constructor TTimeInterval<T>.Create(const value: T; const interval: TTimeSpan);
begin
  fInterval := interval;
  fValue := value;
end;

{$ENDREGION}


end.
