unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring.Collections,
  Spring.Lambda,
  Spring.Reactive;

procedure TForm1.FormCreate(Sender: TObject);
type
  TMouseDownEventArgs = record
    Sender: TObject;
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer;
  end;
const
  x: Lambda<Integer> = ();
var
  clickStream: IObservable<TMouseDownEventArgs>;
  multiClickStream: IObservable<Integer>;
  singleClickStream: IObservable<Integer>;
begin
  clickStream := TObservable.FromEventPattern<TMouseDownEventArgs>(Label1, 'OnMouseDown');

//  with clickStream._
//    .Buffer<TMouseDownEventArgs>(clickStream.Throttle(250))._
//    .Select<Integer>(
//    function(const x: IList<TMouseDownEventArgs>): Integer
//    begin
//      Result := x.Count;
//    end) do
  with TObservable.Select<IList<TMouseDownEventArgs>, Integer>(
    TObservable.Buffer<TMouseDownEventArgs,TMouseDownEventArgs>(clickStream, clickStream.Throttle(250)),
    function(const x: IList<TMouseDownEventArgs>): Integer
    begin
      Result := x.Count;
    end) do
  begin
    multiClickStream := Where(x >= 2);
    singleClickStream := Where(x = 1);
  end;

  multiClickStream.Subscribe(
    procedure(const numClicks: Integer)
    begin
      Caption := 'clicked x' + numClicks.ToString;
    end);

  singleClickStream.Subscribe(
    procedure(const _: Integer)
    begin
      Caption := 'click';
    end);

  TObservable.Merge<Integer>([multiClickStream, singleClickStream])
    .Throttle(1000)
    .Subscribe(
    procedure(const _: Integer)
    begin
      Caption := '';
    end);
end;

end.
