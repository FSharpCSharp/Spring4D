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
        property Coords: TPoints read fCoords;
        property Inner: TInnerObject read fInner;
      end;

  published
    procedure TestPropertyNativeType;
    procedure TestPropertyDrillDownNativeType;
    procedure TestPropertyArray;
    procedure TestPropertyObject;
    procedure TestFieldNativeType;
    procedure TestFieldDrillDownNativeType;
    procedure TestFieldArray;
    procedure TestFieldObject;
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

{$ENDREGION}


{$REGION 'TTestValueExpression.TInnerObject'}

constructor TTestValueExpression.TInnerObject.Create;
begin
  fName := 'Inner Object';
end;

{$ENDREGION}


{$REGION 'TTestValueExpression'}

procedure TTestValueExpression.TestFieldArray;
var
  obj: TOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(obj);
  CheckTrue(expression.Follow('.fCoords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
  obj.Free;
end;

procedure TTestValueExpression.TestFieldNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(obj);
  CheckEquals('Outer Object', expression.Follow('.fName').Value.ToString);
  CheckEquals(15, expression.Follow('.fNumber').Value.AsInteger);
  obj.Free;
end;

procedure TTestValueExpression.TestFieldDrillDownNativeType;
var
  outerObj: TOuterObject;
  obj: TInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(outerObj);
  expression := root.Follow('.fInner.fName');
  CheckEquals('Inner Object', expression.Value.AsString);
  outerObj.Free;
end;

procedure TTestValueExpression.TestFieldObject;
var
  outerObj: TOuterObject;
  obj: TInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(outerObj);
  expression := root.Follow('.fInner');
  CheckTrue(expression.Value.TryAsType<TInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
  outerObj.Free;
end;

procedure TTestValueExpression.TestPropertyNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(obj);
  CheckEquals('Outer Object', expression.Follow('.Name').Value.ToString);
  CheckEquals(15, expression.Follow('.Number').Value.AsInteger);
  obj.Free;
end;

procedure TTestValueExpression.TestPropertyDrillDownNativeType;
var
  outerObj: TOuterObject;
  obj: TInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(outerObj);
  expression := root.Follow('.Inner.Name');
  CheckEquals('Inner Object', expression.Value.AsString);
  outerObj.Free;
end;

procedure TTestValueExpression.TestPropertyArray;
var
  obj: TOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.FromValue(obj);
  CheckTrue(expression.Follow('.Coords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
  obj.Free;
end;

procedure TTestValueExpression.TestPropertyObject;
var
  outerObj: TOuterObject;
  obj: TInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.FromValue(outerObj);
  expression := root.Follow('.Inner');
  CheckTrue(expression.Value.TryAsType<TInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
  outerObj.Free;
end;

{$ENDREGION}


end.
