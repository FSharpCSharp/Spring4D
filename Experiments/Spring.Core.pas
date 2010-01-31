{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.Core experimental;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  IEditableObject         = interface;
  INotifyPropertyChanging = interface;
  INotifyPropertyChanged  = interface;

  TEventArgs = class abstract(TObject)
  end;

  TEventHandler<TEventArgs: TEventArgs> = reference to procedure(sender: TObject; e: TEventArgs);

  /// <summary>
  /// A registry that manages subscriptions to and the publishing of events.
  /// </summary>
  IEventRegistry = interface
    ['{4BB65FE3-5CCE-4E68-97B0-C50A3F72DFB4}']
    procedure PublishEvents(sourceObject: TObject);
    procedure Subscribe(subscriber: TObject); overload;
    procedure Subscribe(subscriber: TObject; targetSourceType: PTypeInfo); overload;
  end;

  /// <summary>
  /// Provides functionality to commit or rollback changes to an object that is
  /// used as a data source.
  /// </summary>
  IEditableObject = interface
    ['{34CAD5E1-EEA1-4DA8-ABE8-B6A1BBD6085C}']
    procedure BeginEdit;
    procedure EndEdit;
    procedure CancelEdit;
  end;

  IProgressMonitor = interface
    ['{92A347D5-6E7C-43C7-85D9-718336CB6771}']
    {$REGION 'Property Getters & Setters'}
      function GetTaskName: string;
      function GetIsCancelled: Boolean;
      procedure SetTaskName(const value: string);
    {$ENDREGION}
    procedure BeginTask(const taskName: string; totalWork: Integer);
    procedure Worked(work: Integer);
    procedure Cancel;
    procedure Done;
    property TaskName: string read GetTaskName write SetTaskName;
    property IsCancelled: Boolean read GetIsCancelled;
  end;

  TPropertyChangingEventHandler = reference to procedure(sender: TObject; const propertyName: string);

  TPropertyChangedEventHandler  = reference to procedure(sender: TObject; const propertyName: string);

  /// <summary>
  /// Notifies clients that a property value is changing.
  /// </summary>
  INotifyPropertyChanging = interface
    ['{51B70703-AF70-43AB-A91B-9A20E3264A07}']
    function GetOnPropertyChanging: IDelegate<TPropertyChangingEventHandler>;
    property OnPropertyChanging: IDelegate<TPropertyChangingEventHandler> read GetOnPropertyChanging;
  end;

  /// <summary>
  /// Notifies clients that a property value has changed.
  /// </summary>
  INotifyPropertyChanged = interface
    ['{380EC508-05E2-4B8B-86E5-AC956D007DDA}']
    function GetOnPropertyChanged: IDelegate<TPropertyChangedEventHandler>;
    property OnPropertyChanged: IDelegate<TPropertyChangedEventHandler> read GetOnPropertyChanged;
  end;

//  TNotifyCollectionChangedAction = (
//    Add,
//    Remove,
//    Extract,
//    Replace,
//    Move,
//    Reset
//  );

//  TNotifyCollectionChangedArgs = record
//    Action: TNotifyCollectionChangedAction;
//    NewItems: IList<TObject>;
//    NewStartingIndex: Integer;
//    OldItems: IList<TObject>;
//    OldStartingIndex: Integer;
//  end;

//  TCollectionChangedEventHandler = reference to procedure(sender: TObject; const args: TNotifyCollectionChangedArgs);

//  INotifyCollectionChanged = interface
//    function GetOnCollectionChanged: IDelegate<TCollectionChangedEventHandler>;
//    property OnCollectionChanged: IDelegate<TCollectionChangedEventHandler> read GetOnCollectionChanged;
//  end;

  IBindingList<T> = interface(IList<T>)
    function AddNew: T;
//    property AllowNew: Boolean read GetAllowNew;
//    property AllowRemove: Boolean read GetAllowRemove;
//    property AllowEdit: Boolean read GetAllowEdit;
  end;

  IHierarchyData = interface
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      function GetHasChildren: Boolean;
      function GetHasParent: Boolean;
      function GetChildren: IList<IHierarchyData>;
      function GetParent: IHierarchyData;
    {$ENDREGION}
    property Name: string read GetName;
    property HasChildren: Boolean read GetHasChildren;
    property HasParent: Boolean read GetHasParent;
    property Children: IList<IHierarchyData> read GetChildren;
    property Parent: IHierarchyData read GetParent;
  end;

  DisplayNameAttribute = class(TCustomAttribute)
  private
    fDisplayName: string;
  public
    constructor Create(const displayName: string);
    property DisplayName: string read fDisplayName;
  end;

implementation


{ TDisplayNameAttribute }

constructor DisplayNameAttribute.Create(const displayName: string);
begin
  inherited Create;
  fDisplayName := displayName;
end;

end.
