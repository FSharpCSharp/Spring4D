unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm104 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form104: TForm104;

implementation

{$R *.dfm}

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Concurrency;

type
  TNotifyEventArgs = record
    Sender: TObject;
  end;

  TMouseEventArgs = record
    Sender: TObject;
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer;
  end;

  TMouseMoveEventArgs = record
    Sender: TObject;
    Shift: TShiftState;
    X, Y: Integer;
  end;

procedure TForm104.FormCreate(Sender: TObject);
var
  mouseMoves: IObservable<TMouseMoveEventArgs>;
  mouseDowns: IObservable<TMouseEventArgs>;
  mouseUps: IObservable<TMouseEventArgs>;
  mouseDrags: IObservable<TMouseMoveEventArgs>;
  mouseDblClicks: IObservable<TNotifyEventArgs>;
begin
  ReportMemoryLeaksOnShutdown := True;
  Canvas.Pen.Width := 5;
  Canvas.Pen.Color := clBlue;

  mouseMoves := TObservable.FromEventPattern<TMouseMoveEventArgs>(Self, 'OnMouseMove');
  mouseDowns := TObservable.FromEventPattern<TMouseEventArgs>(Self, 'OnMouseDown');
  mouseUps := TObservable.FromEventPattern<TMouseEventArgs>(Self, 'OnMouseUp');
  mouseDblClicks := TObservable.FromEventPattern<TNotifyEventArgs>(Self, 'OnDblClick');

  mouseDrags := mouseMoves._
    .SkipUntil<TMouseMoveEventArgs,TMouseEventArgs>(mouseDowns)._
    .TakeUntil<TMouseMoveEventArgs,TMouseEventArgs>(mouseUps)
    .Repeated;

  mouseDowns
    .ObserveOn(TScheduler.MainThread)
    .Subscribe(
    procedure(const event: TMouseEventArgs)
    begin
      Canvas.MoveTo(event.X, event.Y);
    end);

  mouseDrags
    .ObserveOn(TScheduler.MainThread)
    .Subscribe(
    procedure(const event: TMouseMoveEventArgs)
    begin
      Canvas.LineTo(event.X, event.Y);
    end);

  mouseDblClicks
    .ObserveOn(TScheduler.MainThread)
    .Subscribe(
    procedure(const event: TNotifyEventArgs)
    begin
      Canvas.FillRect(ClientRect);
    end);
end;

end.
