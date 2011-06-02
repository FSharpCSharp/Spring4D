{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

///	<preliminary />
///	<summary>
///	  This namespace provides notifications when a property has been changed or
///	  is being changed, and/or a collection changed (added, removed, etc).
///	</summary>
unit Spring.Notifications;

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Extensions;

type
  TPropertyNotificationEventArgs = record
  private
    fPropertyName: string;
    fOldValue: TValue;
    fNewValue: TValue;
  public
    constructor Create(const propertyName: string; const oldValue, newValue: TValue);
    property PropertyName: string read fPropertyName;
    property OldValue: TValue read fOldValue;
    property NewValue: TValue read fNewValue;
  end;

  TPropertyNotificationEventHandler = reference to procedure(sender: TObject;
    const e: TPropertyNotificationEventArgs);

  TPropertyNotificationEvent = IDelegate<TPropertyNotificationEventHandler>;

  (*
  IPropertyListener = interface
    ['{E6812A6A-418B-4F15-AA76-DC9018973E4E}']
    procedure OnPropertyChanging(sender: TObject; const e: TPropertyNotificationEventArgs);
    procedure OnPropertyChanged(sender: TObject; const e: TPropertyNotificationEventArgs);
  end;

  IPropertyObservable = interface(IObservable<IPropertyListener>)
    ['{5F94EA63-77D8-471A-85B9-57F12212781A}']
    /// <summary>
    /// Raises the OnPropertyChanging event.
    /// </summary>
    procedure NotifyPropertyChanging(const e: TPropertyNotificationEventArgs);
    /// <summary>
    /// Raises the OnPropertyChanged event.
    /// </summary>
    procedure NotifyPropertyChanged(const e: TPropertyNotificationEventArgs);
    /// <summary>
    /// Gets or sets the value that can enable or disable the property notifications.
    /// </summary>
    property IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;
  end;

  ICollectionListener = interface
    ['{4EF9EB62-2D2B-4138-8BCC-ABA066DD54B1}']
    procedure OnCollectionChanged(sender: TObject; const e: TPropertyNotificationEventArgs);
  end;

  ICollectionObservable = interface(IObservable<ICollectionListener>)
    ['{DE97A603-3382-4740-A837-CB4E2E14FC75}']
  end;
  //*)

  /// <summary>
  /// Provides a workaround to get Count value of any computable data structure.
  /// </summary>
  /// <preliminary />
  ISupportComputable = interface
    ['{568FE8C9-95EC-4347-B1F8-67E43D6D8158}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  /// <summary>
  /// Notifies clients that a property value is changing or has changed.
  /// </summary>
  IPropertyNotification = interface
    ['{AF21205F-67F3-4B1B-96D0-AB8D7BD032C5}']
  {$REGION 'Property Getters and Setters'}
    function GetOnPropertyChanging: TPropertyNotificationEvent;
    function GetOnPropertyChanged: TPropertyNotificationEvent;
  {$ENDREGION}
    /// <summary>
    /// Gets the multicast event that occurs when a property value is changing.
    /// </summary>
    property OnPropertyChanging: TPropertyNotificationEvent read GetOnPropertyChanging;
    /// <summary>
    /// Gets the multicast event that occurs when a property value has changed.
    /// </summary>
    property OnPropertyChanged: TPropertyNotificationEvent read GetOnPropertyChanged;
  end;

  /// <summary>
  /// Uses the ISupportPropertyNotification interface to raise property
  /// notification events and enable or disable the notification.
  /// </summary>
  ISupportPropertyNotification = interface(IPropertyNotification)
    ['{D1941B1A-4DDA-4429-8348-64F7E7D98CDE}']
  {$REGION 'Property Getters and Setters'}
    function GetIsEnabled: Boolean;
    procedure SetIsEnabled(const value: Boolean);
  {$ENDREGION}
    /// <summary>
    /// Raises the OnPropertyChanging event.
    /// </summary>
    procedure NotifyPropertyChanging({sender: TObject;} const e: TPropertyNotificationEventArgs);
    /// <summary>
    /// Raises the OnPropertyChanged event.
    /// </summary>
    procedure NotifyPropertyChanged({sender: TObject;} const e: TPropertyNotificationEventArgs);
    /// <summary>
    /// Gets or sets the value that can enable or disable the property notifications.
    /// </summary>
    property IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;
  end;

  /// <summary>
  /// Provides the default implementation for the ISupportPropertyNotification
  /// and IPropertyNotification interfaces.
  /// </summary>
  /// <remarks>
  /// By default, the property notification is enabled.
  /// </remarks>
  TPropertyNotification = class(TAggregatedObject,
    ISupportPropertyNotification, IPropertyNotification, IInterface)
  private
    fSender: TObject;
    fOnPropertyChanging: TPropertyNotificationEvent;
    fOnPropertyChanged: TPropertyNotificationEvent;
    fIsEnabled: Boolean;
    function GetOnPropertyChanging: TPropertyNotificationEvent;
    function GetOnPropertyChanged: TPropertyNotificationEvent;
    function GetIsEnabled: Boolean;
    procedure SetIsEnabled(const value: Boolean);
  protected
    procedure NotifyPropertyChanging(const e: TPropertyNotificationEventArgs); virtual;
    procedure NotifyPropertyChanged(const e: TPropertyNotificationEventArgs); virtual;
    property IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;
  public
    constructor Create(const controller: IInterface);
    property OnPropertyChanging: TPropertyNotificationEvent read GetOnPropertyChanging;
    property OnPropertyChanged: TPropertyNotificationEvent read GetOnPropertyChanged;
  end;

  /// <summary>
  /// Provides a base class that supports property change notifications.
  /// </summary>
  TNotifiableObject = class abstract(TInterfaceBase, ISupportPropertyNotification, IPropertyNotification)
  protected
    // Deprecated Members
    fOwnerPropertyNotification: Pointer;
    fPropertyName: string;
    function GetOwnerPropertyNotification: ISupportPropertyNotification;
    property OwnerPropertyNotification: ISupportPropertyNotification read GetOwnerPropertyNotification;
  private
    fPropertyNotification: TPropertyNotification;
    function GetOnPropertyChanging: TPropertyNotificationEvent;
    function GetOnPropertyChanged: TPropertyNotificationEvent;
    function GetPropertyNotification: ISupportPropertyNotification;
    procedure DoPropertyChanged(sender: TObject; const e: TPropertyNotificationEventArgs);
    procedure DoPropertyChanging(sender: TObject; const e: TPropertyNotificationEventArgs);
  protected
    procedure SetProperty<T>(const propertyName: string; var oldValue: T; const newValue: T);
    function TrySetProperty<T>(const propertyName: string; var oldValue: T; const newValue: T): Boolean; overload;
    function TrySetProperty<T>(const propertyName: string; var oldValue: T; const newValue: T; comparer: IEqualityComparer<T>): Boolean; overload;
    property PropertyNotification: ISupportPropertyNotification read GetPropertyNotification implements ISupportPropertyNotification;
  public
    constructor Create(const owner: ISupportPropertyNotification; const propertyName: string); overload;
    destructor Destroy; override;
    property OnPropertyChanging: TPropertyNotificationEvent read GetOnPropertyChanging;
    property OnPropertyChanged: TPropertyNotificationEvent read GetOnPropertyChanged;
  end;

  TCollectionChangedAction = (
    caAdd,
    caRemove,
    caExtracted
//    caMove,
//    caClear
  );

  TCollectionChangedEventArgs = record
  private
    fAction: TCollectionChangedAction;
    fItem: TValue;
    fIndex: Integer;
  public
    constructor Create(action: TCollectionChangedAction; const item: TValue; index: Integer);
    property Action: TCollectionChangedAction read fAction;
    // Consider using OldItems, OldStartIndex, NewItems, NewStartIndex
    // insteading of the Item property.
    property Item: TValue read fItem;
    property Index: Integer read fIndex;
  end;

  /// <summary>
  /// TCollectionChangedEventHandler
  /// </summary>
  TCollectionChangedEventHandler = reference to procedure(sender: TObject;
    const e: TCollectionChangedEventArgs);

  TCollectionChangedEvent = IDelegate<TCollectionChangedEventHandler>;

  /// <summary>
  /// Notifies listeners of dynamic changes, such as when items get added and
  /// removed or the whole list is refreshed.
  /// </summary>
  INotifyCollectionChanged = interface
    ['{931CCE60-AD78-4A96-B21C-87BDC64CB826}']
    function GetOnCollectionChanged: TCollectionChangedEvent;
    property OnCollectionChanged: TCollectionChangedEvent read GetOnCollectionChanged;
  end;

  ///	<summary>Provides an observable collection which in turns&#160;provides
  ///	notifications when an item was added, removed or properties of an item is
  ///	changing or has changed.</summary>
  TNotifiableCollection<T: class> = class(TObjectList<T>, INotifyCollectionChanged,
    ISupportPropertyNotification, ISupportComputable)
  private
    type
      TCollectionChangedDelegate = class(TDelegate<TCollectionChangedEventHandler>)
      end;

    const
      ChangedActions: array[TCollectionNotification] of TCollectionChangedAction = (
        caAdd,
        caRemove,
        caExtracted
      );
  private
    fPropertyNotification: TPropertyNotification;
    fOnPropertyChanging: TPropertyNotificationEventHandler;
    fOnPropertyChanged: TPropertyNotificationEventHandler;
    fOnCollectionChangedEvent: TCollectionChangedEvent;
    function GetPropertyNotification: ISupportPropertyNotification;
    function GetOnCollectionChanged: TCollectionChangedEvent;
  protected
    fOwnerPropertyNotification: Pointer;
    fPropertyName: string;
    function GetOwnerPropertyNotification: ISupportPropertyNotification;
    property OwnerPropertyNotification: ISupportPropertyNotification read GetOwnerPropertyNotification;
    procedure DoOnPropertyChanging(sender: TObject; const e: TPropertyNotificationEventArgs); virtual;
    procedure DoOnPropertyChanged(sender: TObject; const e: TPropertyNotificationEventArgs); virtual;
    procedure Notify(const item: T; action: TCollectionNotification); override;
    property PropertyNotification: ISupportPropertyNotification read GetPropertyNotification implements ISupportPropertyNotification;
  public
    constructor Create(ownsObjects: Boolean = True); overload;
    constructor Create(collection: TEnumerable<T>; ownsObjects: Boolean = True); overload;
    constructor Create(const owner: ISupportPropertyNotification;
      const propertyName: string = ''); overload;
    destructor Destroy; override;
    property OnCollectionChanged: TCollectionChangedEvent read GetOnCollectionChanged;
  end;

  TObjectNotifiableCollection<T: class> = class(TNotifiableCollection<T>)
  public
    constructor Create;
  end;


implementation

uses
  Spring.Utils;


{$REGION 'TPropertyNotificationEventHandlerArgs'}

constructor TPropertyNotificationEventArgs.Create(const propertyName: string;
  const oldValue, newValue: TValue);
begin
  fPropertyName := propertyName;
  fOldValue := oldValue;
  fNewValue := newValue;
end;

{$ENDREGION}


{$REGION 'TPropertyNotification'}

constructor TPropertyNotification.Create(const controller: IInterface);
begin
  inherited Create(controller);
  fSender := TObject(controller);
  fIsEnabled := True;
end;

procedure TPropertyNotification.NotifyPropertyChanging(
  const e: TPropertyNotificationEventArgs);
var
  args: TPropertyNotificationEventArgs;
begin
  if not Assigned(fOnPropertyChanging) or not fIsEnabled then
  begin
    Exit;
  end;
  args := e;
  fOnPropertyChanging.Invoke(
    procedure (handler: TPropertyNotificationEventHandler)
    begin
      handler(fSender, args);
    end
  );
end;

procedure TPropertyNotification.NotifyPropertyChanged(
  const e: TPropertyNotificationEventArgs);
var
  args: TPropertyNotificationEventArgs;
begin
  if not Assigned(fOnPropertyChanged) or not fIsEnabled then
  begin
    Exit;
  end;
  args := e;
  fOnPropertyChanged.Invoke(
    procedure (handler: TPropertyNotificationEventHandler)
    begin
      handler(fSender, args);
    end
  );
end;

function TPropertyNotification.GetOnPropertyChanging: TPropertyNotificationEvent;
begin
  if fOnPropertyChanging = nil then
  begin
    fOnPropertyChanging := TDelegate<TPropertyNotificationEventHandler>.Create;
  end;
  Result := fOnPropertyChanging;
end;

function TPropertyNotification.GetOnPropertyChanged: TPropertyNotificationEvent;
begin
  if fOnPropertyChanged = nil then
  begin
    fOnPropertyChanged := TDelegate<TPropertyNotificationEventHandler>.Create;
  end;
  Result := fOnPropertyChanged;
end;

function TPropertyNotification.GetIsEnabled: Boolean;
begin
  Result := fIsEnabled;
end;

procedure TPropertyNotification.SetIsEnabled(
  const value: Boolean);
begin
  fIsEnabled := value;
end;

{$ENDREGION}


{$REGION 'TNotifiableObject'}

constructor TNotifiableObject.Create(const owner: ISupportPropertyNotification;
  const propertyName: string);
begin
  inherited Create;
  fOwnerPropertyNotification := Pointer(owner);
  fPropertyName := propertyName;
  OnPropertyChanged.AddHandler(DoPropertyChanged);
  OnPropertyChanging.AddHandler(DoPropertyChanging);
end;

function TNotifiableObject.TrySetProperty<T>(const propertyName: string;
  var oldValue: T; const newValue: T; comparer: IEqualityComparer<T>): Boolean;
var
  localOldValue: TValue;
  localNewValue: TValue;
  e: TPropertyNotificationEventArgs;
  ownerArgs: TPropertyNotificationEventArgs;
begin
  if comparer = nil then
  begin
    comparer := TEqualityComparer<T>.Default;
  end;
  Result := not comparer.Equals(oldValue, newValue);
  if not Result then Exit;
  if PropertyNotification.IsEnabled then
  begin
    localOldValue := TValue.From<T>(oldValue);
    localNewValue := TValue.From<T>(newValue);
    e := TPropertyNotificationEventArgs.Create(
      propertyName,
      localOldValue,
      localNewValue
    );
      PropertyNotification.NotifyPropertyChanging(e);
      oldValue := newValue;
      PropertyNotification.NotifyPropertyChanged(e);
  end
  else
  begin
    oldValue := newValue;
  end;
end;

function TNotifiableObject.TrySetProperty<T>(const propertyName: string;
  var oldValue: T; const newValue: T): Boolean;
begin
  Result := TrySetProperty<T>(propertyName, oldValue, newValue, nil);
end;

procedure TNotifiableObject.SetProperty<T>(const propertyName: string;
  var oldValue: T; const newValue: T);
begin
  TrySetProperty<T>(propertyName, oldValue, newValue, nil);
end;

function TNotifiableObject.GetPropertyNotification: ISupportPropertyNotification;
begin
  if fPropertyNotification = nil then
  begin
    fPropertyNotification := TPropertyNotification.Create(Self);
  end;
  Result := fPropertyNotification;
end;

function TNotifiableObject.GetOnPropertyChanging: TPropertyNotificationEvent;
begin
  Result := PropertyNotification.OnPropertyChanging;
end;

destructor TNotifiableObject.Destroy;
begin
  fPropertyNotification.Free;
  inherited Destroy;
end;

procedure TNotifiableObject.DoPropertyChanged(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  ownerArgs: TPropertyNotificationEventArgs;
begin
  if (OwnerPropertyNotification <> nil) and OwnerPropertyNotification.IsEnabled then
  begin
    ownerArgs := TPropertyNotificationEventArgs.Create(
      fPropertyName + '.' + e.PropertyName,
      e.OldValue,
      e.NewValue
    );
    OwnerPropertyNotification.NotifyPropertyChanged(ownerArgs);
  end;
end;

procedure TNotifiableObject.DoPropertyChanging(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  ownerArgs: TPropertyNotificationEventArgs;
begin
  if (OwnerPropertyNotification <> nil) and OwnerPropertyNotification.IsEnabled then
  begin
    ownerArgs := TPropertyNotificationEventArgs.Create(
      fPropertyName + '.' + e.PropertyName,
      e.OldValue,
      e.NewValue
    );
    OwnerPropertyNotification.NotifyPropertyChanging(ownerArgs);
  end;
end;

function TNotifiableObject.GetOnPropertyChanged: TPropertyNotificationEvent;
begin
  Result := PropertyNotification.OnPropertyChanged;
end;

function TNotifiableObject.GetOwnerPropertyNotification: ISupportPropertyNotification;
begin
  Result := ISupportPropertyNotification(fOwnerPropertyNotification);
end;

{$ENDREGION}


{$REGION 'TCollectionNotificationEventArgs'}

constructor TCollectionChangedEventArgs.Create(
  action: TCollectionChangedAction; const item: TValue; index: Integer);
begin
  fAction := action;
  fItem := item;
  fIndex := index;
end;

{$ENDREGION}


{$REGION 'TNotifiableCollection<T>'}

constructor TNotifiableCollection<T>.Create(ownsObjects: Boolean);
begin
  inherited;
  fOnPropertyChanging := DoOnPropertyChanging;
  fOnPropertyChanged := DoOnPropertyChanged;
end;

constructor TNotifiableCollection<T>.Create(collection: TEnumerable<T>;
  ownsObjects: Boolean);
begin
  inherited;
  fOnPropertyChanging := DoOnPropertyChanging;
  fOnPropertyChanged := DoOnPropertyChanged;
end;

constructor TNotifiableCollection<T>.Create(const owner: ISupportPropertyNotification;
  const propertyName: string);
begin
  Create;
  fOwnerPropertyNotification := Pointer(owner);
  fPropertyName := propertyName;
end;

destructor TNotifiableCollection<T>.Destroy;
var
  item: T;
  value: TValue;
  notification: IPropertyNotification;
begin
  for item in Self do
  begin
    value := TValue.From<T>(item);
    if TryGetInterface(value, IPropertyNotification, notification) then
    begin
      notification.OnPropertyChanging.RemoveHandler(fOnPropertyChanging);
      notification.OnPropertyChanged.RemoveHandler(fOnPropertyChanged);
    end;
  end;
  if fPropertyNotification <> nil then
    fPropertyNotification.Free;
  inherited Destroy;
end;

procedure TNotifiableCollection<T>.Notify(const item: T; action: TCollectionNotification);
var
  e: TCollectionChangedEventArgs;
  value: TValue;
  notification: IPropertyNotification;
  notificationAction: TCollectionChangedAction;
  ownerArgs: TPropertyNotificationEventArgs;
begin
  notificationAction := ChangedActions[action];
  value := TValue.From<T>(item);

  if TryGetInterface(value, IPropertyNotification, notification) then
  begin
    if action = cnAdded then
    begin
      notification.OnPropertyChanging.AddHandler(fOnPropertyChanging);
      notification.OnPropertyChanged.AddHandler(fOnPropertyChanged);
    end
    else
    begin
      notification.OnPropertyChanging.RemoveHandler(fOnPropertyChanging);
      notification.OnPropertyChanged.RemoveHandler(fOnPropertyChanged);
    end;
  end;
  if (OwnerPropertyNotification <> nil) and OwnerPropertyNotification.IsEnabled then
  begin
    ownerArgs := TPropertyNotificationEventArgs.Create(
      fPropertyName,
      TValue.Empty,
      value
    );
    OwnerPropertyNotification.NotifyPropertyChanged(ownerArgs);
  end;

  if not Assigned(fOnCollectionChangedEvent) then
  begin
    inherited Notify(item, action);
    Exit;
  end;

  e := TCollectionChangedEventArgs.Create(notificationAction, value, IndexOf(item));
  OnCollectionChanged.Invoke(
    procedure (handler: TCollectionChangedEventHandler)
    begin
      handler(Self, e);
    end
  );
  inherited Notify(item, action);
end;

procedure TNotifiableCollection<T>.DoOnPropertyChanged(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  args: TPropertyNotificationEventArgs;
  ownerArgs: TPropertyNotificationEventArgs;
begin
  args := e;
  PropertyNotification.OnPropertyChanged.Invoke(
    procedure (handler: TPropertyNotificationEventHandler)
    begin
      handler(sender, args);
    end
  );
  if (OwnerPropertyNotification <> nil) and OwnerPropertyNotification.IsEnabled then
  begin
    ownerArgs := TPropertyNotificationEventArgs.Create(
      fPropertyName + '.' + e.PropertyName,
      e.OldValue,
      e.NewValue
    );
    OwnerPropertyNotification.NotifyPropertyChanged(ownerArgs);
  end;
end;

procedure TNotifiableCollection<T>.DoOnPropertyChanging(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  args: TPropertyNotificationEventArgs;
  ownerArgs: TPropertyNotificationEventArgs;
begin
  args := e;
  PropertyNotification.OnPropertyChanging.Invoke(
    procedure (handler: TPropertyNotificationEventHandler)
    begin
      handler(sender, args);
    end
  );
  if (OwnerPropertyNotification <> nil) and OwnerPropertyNotification.IsEnabled then
  begin
    ownerArgs := TPropertyNotificationEventArgs.Create(
      fPropertyName + '.' + e.PropertyName,
      e.OldValue,
      e.NewValue
    );
    OwnerPropertyNotification.NotifyPropertyChanging(ownerArgs);
  end;
end;

function TNotifiableCollection<T>.GetOnCollectionChanged: TCollectionChangedEvent;
begin
  if fOnCollectionChangedEvent = nil then
  begin
    fOnCollectionChangedEvent := TCollectionChangedDelegate.Create;
  end;
  Result := fOnCollectionChangedEvent;
end;

function TNotifiableCollection<T>.GetOwnerPropertyNotification: ISupportPropertyNotification;
begin
  Result := ISupportPropertyNotification(fOwnerPropertyNotification);
end;

function TNotifiableCollection<T>.GetPropertyNotification: ISupportPropertyNotification;
begin
  if fPropertyNotification = nil then
  begin
    fPropertyNotification := TPropertyNotification.Create(Self);
  end;
  Result := fPropertyNotification;
end;

{$ENDREGION}


{$REGION 'TObjectNotifiableCollection<T>'}

constructor TObjectNotifiableCollection<T>.Create;
var
  list: TObjectList<T>;
begin
  list := TObjectList<T>.Create(True);
  inherited Create(list);
end;

{$ENDREGION}


end.
