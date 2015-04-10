unit Demo.Spring.FMX.Coroutines.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo,
  Spring,
  Spring.Collections,
  Spring.Collections.Sequences,
  Spring.Coroutines, FMX.Controls.Presentation, FMX.ScrollBox;

{.$DEFINE USE_THREAD}

type
  TfrmCoroutinesMain = class(TForm)
    memText: TMemo;
    cmdRun: TButton;
    tmrCheck: TTimer;
    procedure cmdRunClick(Sender: TObject);
    procedure tmrCheckTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
{$IFDEF USE_THREAD}
    FThread : TThread;
{$ENDIF}
  public
    { Public declarations }
  end;

var
  frmCoroutinesMain: TfrmCoroutinesMain;

implementation

{$R *.fmx}

var
  output: TStringBuilder;

procedure Fibonacci(const count: Integer);
var
  a, b, c: UInt64;
  i: Integer;
begin
  a := 1;
  b := 1;
  Yield(a);
  Yield(b);
  for i := 3 to count do
  begin
    c := a + b;
    a := b;
    b := c;
    Yield(c);
  end;
end;

procedure Main;
var
  fibo1: IEnumerable<UInt64>;
  fibo2: Func<Integer,IEnumerable<UInt64>>;
  i: UInt64;
  foo: Func<Integer,Integer>;
begin
  fibo1 := TGenerator<Integer, UInt64>.Create(Fibonacci).Bind(10);
  for i in fibo1 do
    output.Append(i).Append(' ');
  output.AppendLine;
  fibo2 := TGenerator<Integer, UInt64>.Create(Fibonacci);
  for i in fibo2(10) do
    output.Append(i).Append(' ');
  output.AppendLine;

  foo := TCoroutine<Integer,Integer>.Create(
    procedure(const a: Integer)
    begin
      Yield(a);
      Yield(a + 1);
      Yield(a + 2);
    end);

  output.Append(foo(1)).AppendLine;
  output.Append(foo(2)).AppendLine;
  output.Append(foo(1)).AppendLine;
  output.Append(foo(2)).AppendLine;
end;

procedure TfrmCoroutinesMain.cmdRunClick(Sender: TObject);
begin
  cmdRun.Enabled := False;
  tmrCheck.Enabled := False;
  if (not Assigned(output)) then
    output := TStringBuilder.Create
  else output.Clear;

{$IFDEF USE_THREAD}
  FThread.Free;
  FThread := TThread.CreateAnonymousThread(Main);
  FThread.FreeOnTerminate := False;
  FThread.Start;

  tmrCheck.Enabled := True;
{$ELSE}
  Main;
  memText.Text := output.ToString;
  cmdRun.Enabled := True;
{$ENDIF}
end;

procedure TfrmCoroutinesMain.FormDestroy(Sender: TObject);
begin
{$IFDEF USE_THREAD}
  FThread.Free;
{$ENDIF}
end;

procedure TfrmCoroutinesMain.tmrCheckTimer(Sender: TObject);
begin
{$IFDEF USE_THREAD}
  if (FThread.Finished) then
  begin
    tmrCheck.Enabled := False;
    memText.Text := output.ToString;
    cmdRun.Enabled := True;
  end;
{$ENDIF}
end;

end.
