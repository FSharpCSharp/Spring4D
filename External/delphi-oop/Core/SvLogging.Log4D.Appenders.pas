(* SvClasses.pas
* Created: 2012-12-21 10:18:54
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net or linas@vikarina.lt
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
unit SvLogging.Log4D.Appenders;

interface

uses
  Log4D
  ,StdCtrls
  ,SysUtils
  ;

type
  { A custom appender to write to a named memo component. }
  TMemoAppender = class(TLogCustomAppender)
  private
    FMemo: TMemo;
  protected
    procedure DoAppend(const Message: string); override;
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const AName: string; const AMemo: TMemo); reintroduce;
  end;

  { A custom renderer for components. }
  TComponentRenderer = class(TLogCustomRenderer)
  public
    function Render(const Message: TObject): string; override;
  end;

  TConsoleAppender = class(TLogCustomAppender)
  protected
    procedure DoAppend(const Message: string); override;
  end;

  TAnonymousAppender = class(TLogCustomAppender)
  private
    FProc: TProc<string>;
  protected
    procedure DoAppend(const Message: string); override;
  public
    constructor Create(const AProc: TProc<string>); reintroduce;
    destructor Destroy; override;

  end;

implementation

uses
  Classes
  ,Controls
  ;

{ TMemoAppender }

constructor TMemoAppender.Create(const AName: string; const AMemo: TMemo);
begin
  inherited Create(AName);
  FMemo := AMemo;
end;

procedure TMemoAppender.DoAppend(const Message: string);
var
  LMsg: string;
begin
  if Assigned(FMemo) and Assigned(FMemo.Parent) then
  begin
    if Copy(Message, Length(Message) - 1, 2) = #13#10 then
      LMsg := Copy(Message, 1, Length(Message) - 2)
    else
      LMsg := Message;
    FMemo.Lines.Add(LMsg);
  end;
end;

procedure TMemoAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
end;

{ TComponentRenderer }

function TComponentRenderer.Render(const Message: TObject): string;
var
  Comp: TComponent;
begin
  if not (Message is TComponent) then
    Result := 'Object must be a TComponent'
  else
  begin
    Comp := Message as TComponent;
    if Comp is TControl then
      // Add position and size info
      with TControl(Comp) do
        Result := Format('%s: %s [%d x %d at %d, %d]',
          [Name, ClassName, Width, Height, Left, Top])
    else
      Result := Format('%s: %s', [Comp.Name, Comp.ClassName]);
  end;
end;

{ TConsoleAppender }

procedure TConsoleAppender.DoAppend(const Message: string);
begin
  if IsConsole then
    Writeln(Message);
end;

{ TAnonymousAppender }

constructor TAnonymousAppender.Create(const AProc: TProc<string>);
begin
  inherited Create('');
  FProc := AProc;
end;

destructor TAnonymousAppender.Destroy;
begin
  FProc := nil;
  inherited;
end;

procedure TAnonymousAppender.DoAppend(const Message: string);
begin
  if Assigned(FProc) then
  begin
    FProc(Message);
  end;
end;

initialization
  RegisterAppender(TMemoAppender);
  RegisterAppender(TConsoleAppender);
  RegisterAppender(TAnonymousAppender);
  RegisterRendered(TComponent);
  RegisterRenderer(TComponentRenderer);

end.
