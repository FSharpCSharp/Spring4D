(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit ViewTestBindings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SvBindings, DSharp.Bindings, Spin, StdCtrls, ComCtrls, ExtCtrls,
  DSharp.Bindings.VCLControls, DSharp.Bindings.VCLControls.SpinEdit;

type
  TfrmTest = class(TForm, IBindableView)
    [Bind('Name', 'Text')]
    edt1: TEdit;
    [Bind('Name', 'Caption')]
    lbl1: TLabel;
    Memo1: TMemo;
    [Bind('IsEnabled', 'Enabled')]
    Button1: TButton;
    [Bind('IsChecked', 'Checked')]
    CheckBox1: TCheckBox;
   // [Bind('Items', 'Items')]
    ListBox1: TListBox;
    [Bind('ID', 'Value')]
    SpinEdit1: TSpinEdit;
    [Bind('Color', 'Selected')]
    ColorBox1: TColorBox;
    [Bind('Date', 'Date')]
    DateTimePicker1: TDateTimePicker;
    [Bind('Points', 'Position')]
    TrackBar1: TTrackBar;
  private
    { Private declarations }
    FBinder: TBindingGroup;
    function GetBinder: TBindingGroup;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Binder: TBindingGroup read GetBinder;
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.dfm}

{ TfrmTest }

constructor TfrmTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBinder := TBindingGroup.Create(Self);
end;

destructor TfrmTest.Destroy;
begin
  FBinder.Free;
  inherited Destroy;
end;

function TfrmTest.GetBinder: TBindingGroup;
begin
  Result := FBinder;
end;

end.
