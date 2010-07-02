unit Spring.Tests.Reflection;

interface

uses
  TestFramework,
  TestExtensions;

type

  TTestLocation = class(TTestCase)
  strict private
    type
      TPoint = record
        X, Y: Integer;
      end;
      TPoints = array[0..9] of TPoint;

      TInnerObject = class
      private
        fName: string;
      public
        constructor Create;
        property Name: string read fName;
      end;

      TOuterObject = class
      private
        fName: string;
        fNumber: Integer;
        fInner: TInnerObject;
        fCoords: TPoints;
      public
        constructor Create;
        destructor Destroy; override;
        property Name: string read fName;
        property Number: Integer read fNumber;
        property Inner: TInnerObject read fInner;
      end;

  published
    procedure TestNativeType;
    procedure TestArray;
    procedure TestObject;
  end;

implementation

uses
  Rtti,
  Spring.Reflection;


{$REGION 'TTestLocation.TOuterObject'}

constructor TTestLocation.TOuterObject.Create;
var
  i: Integer;
begin
  fInner := TInnerObject.Create;
  fName := 'Outer Object';
  fNumber := 15;
  Randomize;
  for i := Low(fCoords) to High(fCoords) do
  begin
    fCoords[i].X := i * i;
    fCoords[i].Y := i + i;
  end;
end;

destructor TTestLocation.TOuterObject.Destroy;
begin
  fInner.Free;
end;

{$ENDREGION}


{$REGION 'TTestLocation.TInnerObject'}

constructor TTestLocation.TInnerObject.Create;
begin
  fName := 'Inner Object';
end;

{$ENDREGION}


{$REGION 'TTestLocation'}

procedure TTestLocation.TestNativeType;
var
  obj: TOuterObject;
  location: TLocation;
begin
  obj := TOuterObject.Create;
  location := TLocation.FromValue(obj);
  CheckEquals(location.Follow('.fName').GetValue.ToString, 'Outer Object');
  CheckEquals(location.Follow('.fNumber').GetValue.AsInteger, 15);
  obj.Free;
end;

procedure TTestLocation.TestArray;
var
  obj: TOuterObject;
  coords: TPoints;
  location: TLocation;
begin
  obj := TOuterObject.Create;
  location := TLocation.FromValue(obj);
  CheckTrue(location.Follow('.fCoords').GetValue.TryAsType<TPoints>(coords));
  CheckEquals(High(coords), 9);
  obj.Free;
end;

procedure TTestLocation.TestObject;
var
  outerObj: TOuterObject;
  obj: TInnerObject;
  root, location: TLocation;
begin
  outerObj := TOuterObject.Create;
  root := TLocation.FromValue(outerObj);
  try
    location := root.Follow('.Inner');
    CheckTrue(location.GetValue.TryAsType<TInnerObject>(obj));
    CheckEquals(obj.Name, 'Inner Object');
  finally
    outerObj.Free;
  end;
end;

{$ENDREGION}


end.
