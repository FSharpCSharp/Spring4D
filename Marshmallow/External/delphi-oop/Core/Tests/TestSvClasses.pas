unit TestSvClasses;

interface

uses
  TestFramework, SysUtils, Classes, SvClasses;

type
  TestTSvTuples = class(TTestCase)
  published
    procedure TestTuples1;
    procedure TestTuples2;
    procedure TestTuples3;
    procedure TestTuples4;
    procedure TestTuples5;
    procedure TestTuples8;
  end;

  TestTPathBuilder = class(TTestCase)
  private
    FPathBuilder: TPathBuilder;
  published
    procedure TestAdd();
    procedure TestAddFile();
    procedure TestGoUpFolder();
  end;

implementation

const
  STRING_TEST = 'testValue';
  INTEGER_TEST = 12345678;

{ TestTSvTuples }

procedure TestTSvTuples.TestTuples1;
var
  tuple: TSvSimpleTuple;
begin
  tuple.Create(STRING_TEST);
  CheckEqualsString(STRING_TEST, tuple.Value[0].AsString);
  Status(tuple.Value[0].AsString);
end;

procedure TestTSvTuples.TestTuples2;
var
  tuple: TSvSimpleTuple;
begin
  tuple.Create([INTEGER_TEST, STRING_TEST]);
  CheckEquals(INTEGER_TEST, tuple.Value[0].AsInteger);
  CheckEqualsString(STRING_TEST, tuple.Value[1].AsString);
end;

procedure TestTSvTuples.TestTuples3;
var
  tuple: TSvSimpleTuple;
begin
  tuple.Create([INTEGER_TEST, STRING_TEST, INTEGER_TEST]);
  CheckEquals(INTEGER_TEST, tuple.Value[0].AsInteger);
  CheckEqualsString(STRING_TEST, tuple.Value[1].AsString);
  CheckEquals(INTEGER_TEST, tuple.Value[2].AsInteger);
end;

procedure TestTSvTuples.TestTuples4;
var
  tuple: TSvSimpleTuple;
begin
  tuple.Create([INTEGER_TEST, STRING_TEST, INTEGER_TEST, INTEGER_TEST]);
  CheckEquals(INTEGER_TEST, tuple.Value[0].AsInteger);
  CheckEqualsString(STRING_TEST, tuple.Value[1].AsString);
  CheckEquals(INTEGER_TEST, tuple.Value[2].AsInteger);
  CheckEquals(INTEGER_TEST, tuple.Value[3].AsInteger);
end;

procedure TestTSvTuples.TestTuples5;
var
  tuple: TSvSimpleTuple;
begin
  tuple.Create([INTEGER_TEST, STRING_TEST, INTEGER_TEST, INTEGER_TEST, INTEGER_TEST]);
  CheckEquals(INTEGER_TEST, tuple.Value[0].AsInteger);
  CheckEqualsString(STRING_TEST, tuple.Value[1].AsString);
  CheckEquals(INTEGER_TEST, tuple.Value[2].AsInteger);
  CheckEquals(INTEGER_TEST, tuple.Value[3].AsInteger);
  CheckEquals(INTEGER_TEST, tuple.Value[4].AsInteger);
end;

procedure TestTSvTuples.TestTuples8;
var
  tuple: TSvTuple<string,Integer,string,Integer,string,Integer,string,Integer>;
begin
  tuple.Create(STRING_TEST, INTEGER_TEST, STRING_TEST, INTEGER_TEST, STRING_TEST, INTEGER_TEST, STRING_TEST, INTEGER_TEST);

  CheckEquals(INTEGER_TEST, tuple.Value2);
  CheckEquals(INTEGER_TEST, tuple.Value4);
  CheckEquals(INTEGER_TEST, tuple.Value6);
  CheckEquals(INTEGER_TEST, tuple.Value8);

  CheckEqualsString(STRING_TEST, tuple.Value1);
  CheckEqualsString(STRING_TEST, tuple.Value3);
  CheckEqualsString(STRING_TEST, tuple.Value5);
  CheckEqualsString(STRING_TEST, tuple.Value7);
end;

{ TestTPathBuilder }

procedure TestTPathBuilder.TestAdd;
var
  LPath: string;
begin
  LPath := 'E:\Testing\Demo';
  FPathBuilder := TPathBuilder.InitCustomPath(LPath, False);
  CheckEqualsString(LPath, FPathBuilder.ToString);

  FPathBuilder.Add('SubDemo', False);
  CheckEqualsString(LPath + '\SubDemo', FPathBuilder.ToString);

  FPathBuilder.Add('SubDemo2', True);
  CheckEqualsString(LPath + '\SubDemo\SubDemo2\', FPathBuilder.ToString);

end;

procedure TestTPathBuilder.TestAddFile;
var
  LPath: string;
begin
  LPath := 'E:\Testing\Demo';
  FPathBuilder := TPathBuilder.InitCustomPath(LPath, False);
  CheckEqualsString(LPath, FPathBuilder.ToString);

  FPathBuilder.AddFile('SubDemo.txt');
  CheckEqualsString(LPath + '\SubDemo.txt', FPathBuilder.ToString);
end;

procedure TestTPathBuilder.TestGoUpFolder;
var
  LPath: string;
begin
  LPath := 'E:\Testing\Demo\SubDemo\';
  FPathBuilder := TPathBuilder.InitCustomPath(LPath);
  CheckEqualsString(LPath, FPathBuilder.ToString);

  FPathBuilder.GoUpFolder(1);
  CheckEqualsString('E:\Testing\Demo\', FPathBuilder.ToString);

  FPathBuilder := TPathBuilder.InitCustomPath('E:\Testing\Demo\SubDemo\SubDemo2\SubDemo3');
  FPathBuilder.GoUpFolder(3);
  CheckEqualsString('E:\Testing\Demo\', FPathBuilder.ToString);
end;

initialization
  RegisterTest(TestTSvTuples.Suite);
  RegisterTest(TestTPathBuilder.Suite);

end.
