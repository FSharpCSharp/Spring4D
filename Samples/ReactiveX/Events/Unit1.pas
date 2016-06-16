unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring.Reactive;

type
  TMouseMoveEventArgs = record
    Sender: TObject;
    Shift: TShiftState;
    X, Y: Integer;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TObservable.FromEventPattern<TMouseMoveEventArgs>(Self, 'OnMouseMove')
    .Where(function(const args: TMouseMoveEventArgs): Boolean
    begin
      Result := ssCtrl in args.Shift;
    end)
    .Sample(100)
    .Subscribe(
    procedure(const args: TMouseMoveEventArgs)
    begin
      Caption := Format('%d.%d', [args.X, args.Y]);
    end);
end;

end.
