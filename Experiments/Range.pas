unit Range;

{$I Spring.inc}

interface

uses
  Generics.Defaults,
  Spring.System;

type
  {$IFDEF SUPPORTS_REGION} {$REGION 'TRange<T>'} {$ENDIF}

  {$IFDEF SUPPORTS_GENERICS}

  IRange<T> = interface
    function GetIsEmpty: Boolean;
    function Contains(const value: T): Boolean; overload;
    function Contains(const range: IRange<T>): Boolean; overload;
    function CompareTo(const range: IRange<T>): Integer;
    function Intersect(const range: IRange<T>): IRange<T>;
    function Union(const range: IRange<T>): IRange<T>;
    function Complement(const range: IRange<T>): IRange<T>;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TRange<T> = record
  private
    fComparer: IComparer<T>;
    fStart: TNullable<T>;
    fEnd: TNullable<T>;
    fIncludeStart: Boolean;
    fIncludeEnd: Boolean;
    function GetIsOpen: Boolean;
    function GetIsClosed: Boolean;
    function GetIsEmpty: Boolean;
    function GetComparer: IComparer<T>;
  public
    constructor Create(const start, &end: TNullable<T>;
      includeStart, includeEnd: Boolean);
    function Contains(const value: T): Boolean; overload;
    function Contains(const range: TRange<T>): Boolean; overload;
    property Start: TNullable<T> read fStart;
    property &End: TNullable<T> read fEnd;
    property IncludeStart: Boolean read fIncludeStart;
    property IncludeEnd: Boolean read fIncludeEnd;
    property IsOpen: Boolean read GetIsOpen;
    property IsClosed: Boolean read GetIsClosed;
    property IsEmpty: Boolean read GetIsEmpty;
//    function Intersect(const range: TRange<T>): TRange<T>;
//    function Union(const range: TRange<T>): TRange<T>;
//    function Complement(const range: TRange<T>): TRange<T>;
//    function ToString: string;
    { class function }
    class function Between(const start, &end: T): TRange<T>; static;
    class function EqualTo(const value: T): TRange<T>; static;
    class function GreaterThan(const value: T): TRange<T>; static;
    class function GreaterThanOrEqualTo(const value: T): TRange<T>; static;
    class function LessThan(const value: T): TRange<T>; static;
    class function LessThanOrEqualTo(const value: T): TRange<T>; static;
    { Comparison }
    class operator Equal(const left, right: TRange<T>): Boolean;
    class operator NotEqual(const left, right: TRange<T>): Boolean;
  end;

  TDateTimeRange = TRange<TDateTime>;
  TDateRange     = TRange<TDate>;
  TTimeRange     = TRange<TTime>;

  TIntegerRange  = TRange<Integer>;
  TInt64Range    = TRange<Int64>;

  TDoubleRange   = TRange<Double>;
  TCurrencyRange = TRange<Currency>;

  {$ENDIF ~SUPPORTS_GENERICS}

  {$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


implementation


{$IFDEF SUPPORTS_REGION} {$REGION 'TRange<T>'} {$ENDIF}

{$IFDEF SUPPORTS_GENERICS}

constructor TRange<T>.Create(const start, &end: TNullable<T>;
  includeStart, includeEnd: Boolean);
begin
//  inherited Create;
  fComparer := TComparer<T>.Default;
  if (not start.HasValue or not &end.HasValue) or
    (GetComparer.Compare(start.value, &end.value) <= 0) then
  begin
    fStart        := start;
    fEnd          := &end;
    fIncludeStart := includeStart;
    fIncludeEnd   := includeEnd;
  end
  else
  begin
    fStart        := &end;
    fEnd          := start;
    fIncludeStart := includeEnd;
    fIncludeEnd   := includeStart;
  end;
end;

function TRange<T>.GetComparer: IComparer<T>;
begin
  if fComparer = nil then
  begin
    fComparer := TComparer<T>.Default;
  end;
end;

function TRange<T>.Contains(const value: T): Boolean;
var
  m, n: Integer;
begin
  Result := True;
  if fStart.HasValue then
  begin
    m := GetComparer.Compare(value, fStart.value);
    Result := (m > 0) or (fIncludeStart and (m = 0));
  end;
  if fEnd.HasValue then
  begin
    n := GetComparer.Compare(value, fEnd.value);
    Result := Result and ((n < 0) or (fIncludeEnd and (n = 0)));
  end;
end;

function TRange<T>.Contains(const range: TRange<T>): Boolean;
begin
  Result := range.Start.HasValue and Contains(range.Start.value);
  Result := Result and range.&End.HasValue and Contains(range.&End.value);
end;

function TRange<T>.GetIsOpen: Boolean;
begin
  Result := not IncludeStart and not IncludeEnd;
end;

function TRange<T>.GetIsClosed: Boolean;
begin
  Result := IncludeStart and IncludeEnd;
end;

function TRange<T>.GetIsEmpty: Boolean;
begin
  Result := IsOpen and (fStart.Equals(fEnd));
end;

class function TRange<T>.Between(const start, &end: T): TRange<T>;
begin
  Result := TRange<T>.Create(TNullable<T>.Create(start), TNullable<T>.Create(&end), True, True);
end;

class function TRange<T>.EqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Between(value, value);
end;

class function TRange<T>.GreaterThan(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(value, nil, False, False);
end;

class function TRange<T>.GreaterThanOrEqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(value, nil, True, False);
end;

class function TRange<T>.LessThan(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(nil, value, False, False);
end;

class function TRange<T>.LessThanOrEqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(nil, value, False, True);
end;

class operator TRange<T>.Equal(const left, right: TRange<T>): Boolean;
begin
  Result := (left.Start = right.Start) and
    (left.&End = right.&End) and
    (left.IncludeStart = right.IncludeStart) and
    (left.IncludeEnd = right.IncludeEnd);
end;

class operator TRange<T>.NotEqual(const left, right: TRange<T>): Boolean;
begin
  Result := (left.Start <> right.Start) or
    (left.&End <> right.&End) or
    (left.IncludeStart <> right.IncludeStart) or
    (left.IncludeEnd <> right.IncludeEnd);
end;

{$ENDIF ~SUPPORTS_GENERICS}

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}

end.
