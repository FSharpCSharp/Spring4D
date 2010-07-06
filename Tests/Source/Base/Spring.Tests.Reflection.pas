unit Spring.Tests.Reflection;

interface

uses
  TestFramework,
  TestExtensions;

type
  TTestValueExpression = class(TTestCase)
  strict private
    type
      TPoint = record
        X, Y: Integer;
      end;
      TPoints = array[0..9] of TPoint;

      IInnerObject = interface
      ['{726369BA-80A8-4F03-BB98-F4DB68720DEF}']
      {$REGION 'Property Getters and Setters'}
        procedure SetName(const value: string);
        function GetName: string;
        function GetPoint: TTestValueExpression.TPoint;
      {$ENDREGION}

        property Name: string read GetName write SetName;
        property Point: TPoint read GetPoint;
      end;

      TInnerObject = class(TInterfacedObject, IInnerObject)
      private
        fName: string;
        fPoint: TPoint;
        procedure SetName(const value: string);
        function GetName: string;
        function GetPoint: TPoint;
      public
        constructor Create;
      end;

      IOuterObject = interface
      ['{DB1E0234-D04B-4A4B-B5FB-7A8BC7BE02B6}']
      {$REGION 'Property Getters and Setters'}
        procedure SetName(const value: string);
        function GetName: string;
        procedure SetNumber(const value: Integer);
        function GetNumber: Integer;
        function GetCoords: TTestValueExpression.TPoints;
        function GetInner: TTestValueExpression.TInnerObject;
      {$ENDREGION}

        property Name: string read GetName write SetName;
        property Number: Integer read GetNumber write SetNumber;
        property Coords: TPoints read GetCoords;
        property Inner: TInnerObject read GetInner;
      end;

      TOuterObject = class(TInterfacedObject, IOuterObject)
      private
        fName: string;
        fNumber: Integer;
        fInner: TInnerObject;
        fCoords: TPoints;
        procedure SetName(const value: string);
        function GetName: string;
        procedure SetNumber(const value: Integer);
        function GetNumber: Integer;
        function GetCoords: TPoints;
        function GetInner: TInnerObject;
      public
        constructor Create;
        destructor Destroy; override;
      end;

  published
    procedure TestGetPropertyRecordType;
    procedure TestSetPropertyRecordType;
    procedure TestGetPropertyNativeType;
    procedure TestSetPropertyNativeType;
    procedure TestGetPropertyDrillDownNativeType;
    procedure TestSetPropertyDrillDownNativeType;
    procedure TestGetPropertyArray;
    procedure TestSetPropertyArray;
    procedure TestGetPropertyObject;
    procedure TestGetFieldNativeType;
    procedure TestSetFieldNativeType;
    procedure TestGetFieldDrillDownNativeType;
    procedure TestSetFieldDrillDownNativeType;
    procedure TestGetFieldArray;
    procedure TestGetFieldObject;
  end;

implementation

uses
  Rtti,
  Spring.Reflection;


{$REGION 'TTestValueExpression.TOuterObject'}

constructor TTestValueExpression.TOuterObject.Create;
var
  i: Integer;
begin
  fInner := TInnerObject.Create;
  fName := 'Outer Object';
  fNumber := 15;
  for i := Low(fCoords) to High(fCoords) do
  begin
    fCoords[i].X := i * i;
    fCoords[i].Y := i + i;
  end;
end;

destructor TTestValueExpression.TOuterObject.Destroy;
begin
  fInner.Free;
end;

function TTestValueExpression.TOuterObject.GetCoords: TPoints;
begin
  Result := fCoords;
end;

function TTestValueExpression.TOuterObject.GetInner: TInnerObject;
begin
  Result := fInner;
end;

function TTestValueExpression.TOuterObject.GetName: string;
begin
  Result := fName;
end;

function TTestValueExpression.TOuterObject.GetNumber: Integer;
begin
  Result := fNumber;
end;

procedure TTestValueExpression.TOuterObject.SetName(const value: string);
begin
  if value <> fName then
    fName := value;
end;

procedure TTestValueExpression.TOuterObject.SetNumber(const value: Integer);
begin
  if value <> fNumber then
    fNumber := value;
end;

{$ENDREGION}


{$REGION 'TTestValueExpression.TInnerObject'}

constructor TTestValueExpression.TInnerObject.Create;
begin
  fName := 'Inner Object';
  fPoint.X := 15;
  fPoint.Y := 20;
end;

function TTestValueExpression.TInnerObject.GetName: string;
begin
  Result := fName;
end;

function TTestValueExpression.TInnerObject.GetPoint: TPoint;
begin
  Result := fPoint;
end;

procedure TTestValueExpression.TInnerObject.SetName(const value: string);
begin
  if value <> fName then
    fName := value;
end;

{$ENDREGION}


{$REGION 'TTestValueExpression'}

procedure TTestValueExpression.TestGetFieldArray;
var
  obj: IOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckTrue(expression.Follow('.fCoords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
end;

procedure TTestValueExpression.TestGetFieldNativeType;
var
  obj: IOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckEquals('Outer Object', expression.Follow('.fName').Value.ToString);
  CheckEquals(15, expression.Follow('.fNumber').Value.AsInteger);
end;

procedure TTestValueExpression.TestSetFieldNativeType;
var
  obj: IOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckEquals('Outer Object', expression.Follow('.fName').Value.ToString);
  expression.Follow('.fName').SetValue('Test Outer Object');
  CheckEquals('Test Outer Object', expression.Follow('.fName').Value.ToString);
  CheckEquals(15, expression.Follow('.fNumber').Value.AsInteger);
  expression.Follow('.fNumber').SetValue(18);
  CheckEquals(18, expression.Follow('.fNumber').Value.AsInteger);
end;

procedure TTestValueExpression.TestGetFieldDrillDownNativeType;
var
  obj: IOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  expression := root.Follow('.fInner.fName');
  CheckEquals('Inner Object', expression.Value.AsString);
end;

procedure TTestValueExpression.TestSetFieldDrillDownNativeType;
var
  obj: IOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  expression := root.Follow('.fInner.fName');
  CheckEquals('Inner Object', expression.Value.AsString);
  expression.SetValue('Test Inner Object');
  CheckEquals('Test Inner Object', expression.Value.AsString);
end;

procedure TTestValueExpression.TestGetFieldObject;
var
  outerObj: IOuterObject;
  obj: IInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(outerObj));
  expression := root.Follow('.fInner');
  CheckTrue(expression.Value.TryAsType<IInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
end;

procedure TTestValueExpression.TestGetPropertyNativeType;
var
  obj: IOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckEquals('Outer Object', expression.Follow('.Name').Value.ToString);
  CheckEquals(15, expression.Follow('.Number').Value.AsInteger);
end;

procedure TTestValueExpression.TestGetPropertyDrillDownNativeType;
var
  obj: IOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  expression := root.Follow('.Inner.Name');
  CheckEquals('Inner Object', expression.Value.AsString);
end;

procedure TTestValueExpression.TestSetPropertyDrillDownNativeType;
var
  obj: IOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  expression := root.Follow('.Inner.Name');
  CheckEquals('Inner Object', expression.Value.AsString);
  expression.SetValue('Test Inner Object');
  CheckEquals('Test Inner Object', expression.Value.AsString);
end;

procedure TTestValueExpression.TestGetPropertyArray;
var
  obj: IOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckTrue(expression.Follow('.Coords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
end;

procedure TTestValueExpression.TestGetPropertyObject;
var
  outerObj: IOuterObject;
  obj: IInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(TValue.From<IOuterObject>(outerObj));
  expression := root.Follow('.Inner');
  CheckTrue(expression.Value.TryAsType<IInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
end;

procedure TTestValueExpression.TestGetPropertyRecordType;
var
  obj: IInnerObject;
  expression: IValueExpression;
begin
  obj := TInnerObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IInnerObject>(obj));
  CheckEquals(15, expression.Follow('.Point.X').Value.AsInteger);
end;

procedure TTestValueExpression.TestSetPropertyRecordType;
var
  obj: IInnerObject;
  expression: IValueExpression;
begin
  obj := TInnerObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IInnerObject>(obj));
  CheckEquals(15, expression.Follow('.Point.X').Value.AsInteger);
  expression.Follow('.Point.X').SetValue(22);
  CheckEquals(22, expression.Follow('.Point.X').Value.AsInteger);
end;

procedure TTestValueExpression.TestSetPropertyArray;
var
  obj: IOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));
  CheckEquals(4, expression.Follow('.Coords[2].X').Value.AsInteger);
  expression.Follow('.Coords[2].X').SetValue(8);
  CheckEquals(8, expression.Follow('.Coords[2].X').Value.AsInteger);
end;

procedure TTestValueExpression.TestSetPropertyNativeType;
var
  obj: IOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(TValue.From<IOuterObject>(obj));

  CheckEquals('Outer Object', expression.Follow('.Name').Value.ToString);

  expression.Follow('.Name').SetValue('Set Native Type Test');
  CheckEquals('Set Native Type Test', expression.Follow('.Name').Value.ToString);

  CheckEquals(15, expression.Follow('.Number').Value.AsInteger);

  expression.Follow('.Number').SetValue(18);
  CheckEquals(18, expression.Follow('.Number').Value.AsInteger);
end;

{$ENDREGION}


end.
