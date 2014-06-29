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
unit DataObject;

interface

uses
  Classes, Graphics, DSharp.Core.PropertyChangedBase, Rtti;

const
  CNAME = 'Foobar';
  CID = 58;
  CPOINTS = 5;

type
  TData = class(TPropertyChangedBase)
  private
    FName: string;
    FID: Integer;
    FDate: TDateTime;
    FPoints: Integer;
    FColor: TColor;
    FIsChecked: Boolean;
    FItems: TStrings;
    FIsEnabled: Boolean;
    FCaption: string;
    FCurrentDate: TDateTime;
    procedure SetName(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetDate(const Value: TDateTime);
    procedure SetPoints(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetIsChecked(const Value: Boolean);
    procedure SetIsEnabled(const Value: Boolean);
    procedure SetItems(const Value: TStrings);
    procedure SetCaption(const Value: string);
    procedure SetCurrentDate(const Value: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetDefaults();
    procedure Clear();

    property Name: string read FName write SetName;
    property Caption: string read FCaption write SetCaption;
    property CurrentDate: TDateTime read FCurrentDate write SetCurrentDate;
    property ID: Integer read FID write SetID;
    property Date: TDateTime read FDate write SetDate;
    property Points: Integer read FPoints write SetPoints;
    property Color: TColor read FColor write SetColor;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property IsEnabled: Boolean read FIsEnabled write SetIsEnabled;
    property Items: TStrings read FItems write FItems;
  end;

implementation

uses
  DateUtils,
  TypInfo,
  StrUtils;

{ TData }

procedure TData.Clear;
begin
  SetDefaults;
  FItems.Clear;
end;

constructor TData.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TData.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TData.SetCaption(const Value: string);
begin
  FCaption := Value;
  DoPropertyChanged('Caption');
end;

procedure TData.SetColor(const Value: TColor);
begin
  FColor := Value;
  DoPropertyChanged('Color');
end;

procedure TData.SetCurrentDate(const Value: TDateTime);
begin
  FCurrentDate := Value;
  DoPropertyChanged('CurrentDate');
end;

procedure TData.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  DoPropertyChanged('Date');
end;

procedure TData.SetDefaults;
begin
  FName := CNAME;
  FID := CID;
  FDate := Today;
  FPoints := CPOINTS;
  FColor := clBlack;
  FIsChecked := True;
  FIsEnabled := False;
  FCaption := CNAME;
  FItems.AddStrings(TArray<string>.Create(CNAME, '2', '3'));
end;

procedure TData.SetID(const Value: Integer);
begin
  FID := Value;
  DoPropertyChanged('ID');
end;

procedure TData.SetIsChecked(const Value: Boolean);
begin
  FIsChecked := Value;
  DoPropertyChanged('IsChecked');
end;

procedure TData.SetIsEnabled(const Value: Boolean);
begin
  FIsEnabled := Value;
  DoPropertyChanged('IsEnabled');
end;

procedure TData.SetItems(const Value: TStrings);
begin
  FItems := Value;
 // DoPropertyChanged('View');
end;

procedure TData.SetName(const Value: string);
begin
  FName := Value;
  DoPropertyChanged('Name');
end;

procedure TData.SetPoints(const Value: Integer);
begin
  FPoints := Value;
  DoPropertyChanged('Points');
end;

end.
