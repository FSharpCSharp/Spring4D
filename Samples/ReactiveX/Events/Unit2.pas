unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring.Reactive;

type
  TNotifyEventArgs = record
    Sender: TObject;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TObservable.FromEventPattern<TNotifyEventArgs>(Edit1, 'OnChange')
    .Throttle(1000)
    .Subscribe(
    procedure(const args: TNotifyEventArgs)
    begin
      Caption := Format('searching for %s', [TEdit(args.Sender).Text]);
    end);
end;

end.
