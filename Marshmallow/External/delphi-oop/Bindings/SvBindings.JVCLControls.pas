{*******************************************************}
{                                                       }
{       SvBindings                                      }
{                                                       }
{       Copyright (C) 2011 "Linas Naginionis"           }
{                                                       }
{*******************************************************}

unit SvBindings.JVCLControls;

interface

uses
  Classes,
  ComCtrls,
  CommCtrl,
  Controls,
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications,
  DSharp.Core.DataTemplates,
  DSharp.Core.DataTemplates.Default,
  DSharp.Core.Events,
  ExtCtrls,
  Forms,
  Grids,
  Messages,
  StdCtrls,
  SysUtils,
  JvToolEdit;

type
  TJvDirectoryEdit = class(JvToolEdit.TJvDirectoryEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvComboEdit = class(JvToolEdit.TJvComboEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;

  end;


implementation

uses
  DSharp.Bindings.CollectionView.Adapters,
  DSharp.Bindings.CollectionView.VCLAdapters,
  DSharp.Bindings.Exceptions,
  DSharp.Core.Reflection;



{ TJvDirectoryEdit }

procedure TJvDirectoryEdit.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TJvDirectoryEdit.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

constructor TJvDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

{ TJvComboEdit }

procedure TJvComboEdit.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TJvComboEdit.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

constructor TJvComboEdit.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

end.
