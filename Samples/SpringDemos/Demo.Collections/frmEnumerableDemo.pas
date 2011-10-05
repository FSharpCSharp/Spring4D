unit frmEnumerableDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls
  , Spring
  , Spring.Collections
  , System.Generics.Collections
  , Spring.Collections.Extensions
  ;

type
   TIntegerStringPair = TPair<integer, string>;

type
  TEnumerationDemoForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    List: IList<TPair<integer, string>>;
    procedure Clear;
    function CreateAnotherList: IList<TPair<integer, string>>;
  public
    { Public declarations }
  end;

var
  EnumerationDemoForm: TEnumerationDemoForm;

implementation

{$R *.dfm}



procedure TEnumerationDemoForm.Button1Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
begin
  Clear;
  for Pair in List do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;
end;


procedure TEnumerationDemoForm.Button2Click(Sender: TObject);
var
  Predicate: Spring.TPredicate<TIntegerStringPair>;
  Pair: TIntegerStringPair;
  Enumerable: TWhereEnumerable<TIntegerStringPair>;
begin
  Clear;
  Predicate := function(const Pair: TIntegerStringPair): Boolean
               begin
                 Result :=  Pair.Key mod 2 = 0;
               end;
  Enumerable := TWhereEnumerable<TIntegerStringPair>.Create(List, Predicate);

  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Button3Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
  Enumerable: TSkipEnumerable<TIntegerStringPair>;
begin
  Clear;
  // Skip the first seven
  Enumerable := TSkipEnumerable<TIntegerStringPair>.Create(List, 7);

  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Button4Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
  Predicate: Spring.TPredicate<TIntegerStringPair>;
  Enumerable: TSkipWhileEnumerable<TIntegerStringPair>;
begin
  Clear;
  Predicate := function(const Pair: TIntegerStringPair): Boolean
               begin
                 Result :=  Pair.Key > 5;
               end;

  Enumerable := TSkipWhileEnumerable<TIntegerStringPair>.Create(List, Predicate);
  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Button5Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
  Enumerable: TTakeEnumerable<TIntegerStringPair>;
begin
  Clear;
  // Only "take" the first seven
  Enumerable := TTakeEnumerable<TIntegerStringPair>.Create(List, 7);

  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Button6Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
  Predicate: Spring.TPredicate<TIntegerStringPair>;
  Enumerable: TTakeWhileEnumerable<TIntegerStringPair>;
begin
  Clear;
  Predicate := function(const Pair: TIntegerStringPair): Boolean
               begin
                 Result :=  Pair.Key < 5;
               end;

  Enumerable := TTakeWhileEnumerable<TIntegerStringPair>.Create(List, Predicate);
  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Button7Click(Sender: TObject);
var
  Pair: TIntegerStringPair;
  Enumerable: TConcatEnumerable<TIntegerStringPair>;
  TempList: IList<TIntegerStringPair>;
begin
  Clear;
  TempList := CreateAnotherList;

  Enumerable := TConcatEnumerable<TIntegerStringPair>.Create(List, TempList);

  for Pair in Enumerable do
  begin
    Memo1.Lines.Add(Pair.Value);
  end;

end;

procedure TEnumerationDemoForm.Clear;
begin
  Memo1.Clear;
end;

function TEnumerationDemoForm.CreateAnotherList: IList<TPair<integer, string>>;
begin
  Result := Spring.Collections.TList<TIntegerStringPair>.Create;
  Result.Add(TIntegerStringPair.Create(11, 'eleven'));
  Result.Add(TIntegerStringPair.Create(12, 'twelve'));
  Result.Add(TIntegerStringPair.Create(13, 'thirteen'));
  Result.Add(TIntegerStringPair.Create(14, 'fourteen'));
  Result.Add(TIntegerStringPair.Create(15, 'fifteen'));
end;

procedure TEnumerationDemoForm.FormCreate(Sender: TObject);
begin
  Clear;
  List := Spring.Collections.TList<TIntegerStringPair>.Create;
  List.Add(TIntegerStringPair.Create(1, 'one'));
  List.Add(TIntegerStringPair.Create(2, 'two'));
  List.Add(TIntegerStringPair.Create(3, 'three'));
  List.Add(TIntegerStringPair.Create(4, 'four'));
  List.Add(TIntegerStringPair.Create(5, 'five'));
  List.Add(TIntegerStringPair.Create(6, 'six'));
  List.Add(TIntegerStringPair.Create(7, 'seven'));
  List.Add(TIntegerStringPair.Create(8, 'eight'));
  List.Add(TIntegerStringPair.Create(9, 'nine'));
  List.Add(TIntegerStringPair.Create(10, 'ten'));
end;

end.
