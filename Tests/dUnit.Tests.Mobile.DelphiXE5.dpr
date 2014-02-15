program dUnit.Tests.Mobile.DelphiXE5;

uses
  FMXTestRunner in 'Source\dUnit\FMXTestRunner.pas' {FMXTestRunner},
  TestFramework;

{$R *.res}

type
	TestSampleCase = class(TTestCase)
	published
		procedure TestSomething;
		procedure TestSomethingElse;
		procedure TestNoTest;
		procedure TestFail;
		//procedure TestLeak; //Not supported right now
	end;

{ TestTSet }

{procedure TestTSet.TestLeak;
begin
	Check(true);
	TObject.Create.__ObjAddRef;
end;}

procedure TestSampleCase.TestFail;
begin
	Fail('Error');
end;

procedure TestSampleCase.TestNoTest;
begin

end;

procedure TestSampleCase.TestSomething;
begin
	CheckTrue(true, 'Fail');
end;

procedure TestSampleCase.TestSomethingElse;
begin
	Check(true);
end;

begin
  RegisterTest(TestSampleCase.Suite);
  TFMXTestRunner.RunRegisteredTests;
end.
