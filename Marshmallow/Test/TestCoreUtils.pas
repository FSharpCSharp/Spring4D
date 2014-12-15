unit TestCoreUtils;

interface

uses
  TestFramework
  ,Spring.Persistence.Core.Utils
  ;

type
  TTestCoreUtils = class(TTestCase)
  private

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TryConvert_Nullable();
    {$IFDEF PERFORMANCE_TESTS}
    procedure TryConvert_Nullable_Speed();
    {$ENDIF}
  end;

implementation

uses
  Rtti
  ,TestEntities
  ,Diagnostics
  ,SysUtils
  ,Spring
  ,Spring.Persistence.Core.Types
  ,Spring.Collections
  ;

{ TTestCoreUtils }

procedure TTestCoreUtils.Setup;
begin
  inherited;
end;

procedure TTestCoreUtils.TearDown;
begin
  inherited;
end;

procedure TTestCoreUtils.TryConvert_Nullable;
var
  LFrom, LResult: TValue;
  bOK: Boolean;
  LEntity: TCustomer;
  LValue: Nullable<Double>;
  LSpringValue: Nullable<string>;
  LOrder: TCustomer_Orders;
begin
  //Spring Nullable
  LEntity := TCustomer.Create;
  try
    LFrom := 'Bob';
    bOK := TUtils.TryConvert(LFrom, TypeInfo(Nullable<string>), LEntity, LResult);
    CheckTrue(bOK);
    CheckEquals('Nullable<System.string>', string(LResult.TypeInfo.Name));
    CheckTrue(LResult.TryAsType<Nullable<string>>(LSpringValue));
    CheckTrue(LSpringValue.HasValue);

    CheckTrue(TUtils.TryGetNullableTypeValue(LResult, LFrom));
    CheckEquals('Bob', LFrom.AsString);
  finally
    LEntity.Free;
  end;
  //Marshmallow Nullable
  LOrder := TCustomer_Orders.Create;
  try
    LFrom := 256.12;
    bOK := TUtils.TryConvert(LFrom, TypeInfo(Nullable<Double>), LOrder, LResult);
    CheckTrue(bOK);
    CheckEquals('Nullable<System.Double>', string(LResult.TypeInfo.Name));
    CheckTrue(LResult.TryAsType<Nullable<Double>>(LValue));
    CheckTrue(LValue.HasValue);
    CheckEquals(256.12, LValue.Value, 0.001);

    CheckTrue(TUtils.TryGetNullableTypeValue(LResult, LFrom));
    CheckEquals(256.12, LFrom.AsExtended, 0.001);
  finally
    LOrder.Free;
  end;
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TTestCoreUtils.TryConvert_Nullable_Speed;
var
  LFrom, LResult: TValue;
  bOK: Boolean;
  LEntity: TCustomer;
  LOrder: TCustomer_Orders;
  LCount, i: Integer;
  sw: TStopwatch;
begin
  LEntity := TCustomer.Create;
  bOK := False;
  try
    LFrom := 'Bob';
    LCount := 100000;

    sw := TStopwatch.StartNew;
    for i := 1 to LCount do
    begin
      bOK := TUtils.TryConvert(LFrom, TypeInfo(Nullable<string>), LEntity, LResult);
    end;

    sw.Stop;
    CheckTrue(bOK);
    CheckEquals('Nullable<System.string>', string(LResult.TypeInfo.Name));

    Status(Format('Set %D Spring Nullable<string> values in %D ms', [LCount, sw.ElapsedMilliseconds]));

    sw := TStopwatch.StartNew;
    for i := 1 to LCount do
    begin
      bOK := TUtils.TryConvert(LFrom, TypeInfo(Nullable<string>), LEntity, LResult);
    end;
    sw.Stop;
    CheckTrue(bOK);
    Status(Format('Set %D simple string values in %D ms', [LCount, sw.ElapsedMilliseconds]));
  finally
    LEntity.Free;
  end;

  LOrder := TCustomer_Orders.Create;
  try
    sw := TStopwatch.StartNew;
    for i := 1 to LCount do
    begin
      bOK := TUtils.TryConvert(LFrom, TypeInfo(Nullable<Double>), LOrder, LResult);
    end;
    sw.Stop;
    CheckTrue(bOK);
    CheckEquals('Nullable<System.Double>', string(LResult.TypeInfo.Name));

    Status(Format('Set %D Marshmallow Nullable<double> values in %D ms', [LCount, sw.ElapsedMilliseconds]));
  finally
    LOrder.Free;
  end;
end;
{$ENDIF}

initialization
  RegisterTest(TTestCoreUtils.Suite);

end.
