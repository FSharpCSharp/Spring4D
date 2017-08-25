unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Spring.Reactive;

type
  TTextChangedEvent = procedure(Sender: TObject; const Text: string) of object;

  TEdit = class(Vcl.StdCtrls.TEdit)
  private
    FOnTextChange: TTextChangedEvent;
  protected
    procedure Change; override;
  public
    property OnTextChange: TTextChangedEvent read FOnTextChange write FOnTextChange;
  end;

type
  TNotifyEventArgs = record
    Sender: TObject;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    sub: IDisposable;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring.Reactive.Concurrency;

type
  TProgressBarHelper = class helper for TProgressBar
  private
    function GetPositoon: Integer;
    procedure SetPosition(const Value: Integer);
  public
    procedure StepIt();
    property Position: Integer read GetPositoon write SetPosition;
  end;


{ TProgressBarHelper }

function TProgressBarHelper.GetPositoon: Integer;
begin
  Result := inherited Position;
end;

procedure TProgressBarHelper.SetPosition(const Value: Integer);
begin
  if Value <> inherited Position then
  begin
    if Value >= Max then
    begin
      Max := Max + 1;
      inherited Position := Max;
      Max := Max - 1;
    end
    else
    begin
      inherited Position := Value + 1;
      inherited Position := Value;
    end;
  end;
end;

procedure TProgressBarHelper.StepIt;
begin
  Position := Position + Step;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  textChange: IObservable<TNotifyEventArgs>;
begin
  textChange := TObservable.FromEventPattern<TNotifyEventArgs>(Edit1, 'OnChange');

  textChange.Subscribe(
    procedure (const args: TNotifyEventArgs)
    begin
      ProgressBar1.Position := 0;
      sub := TObservable.Interval(10).Take(100)._.TakeUntil<Int64,TNotifyEventArgs>(textChange)
        .ObserveOn(TScheduler.MainThread)
        .Subscribe(
          procedure(const i: Int64)
          begin
            ProgressBar1.StepIt;
          end);
    end);

  textChange
    .Throttle(1000)
    .ObserveOn(TScheduler.MainThread)
    .Subscribe(
    procedure(const args: TNotifyEventArgs)
    begin
      Caption := Format('searching for %s', [TEdit(args.Sender).Text]);
    end);
end;

{ TEdit }

procedure TEdit.Change;
begin
  inherited Change;
  if Assigned(FOnTextChange) then FOnTextChange(Self, Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(sub) then sub.Dispose;
end;

end.
