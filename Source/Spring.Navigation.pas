{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.Navigation;

{$I Spring.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, RTTI,
  Spring.System, Spring.Collections;

type
  /// <summary>
  /// IDisplayName
  /// </summary>
  IDisplayName = interface
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
  end;

  /// <summary>
  /// Represents a navigatable location
  /// </summary>
  ILocatable = interface(IDisplayName)
    procedure Locate;
  end;

  /// <summary>
  /// Tracks navigation history.
  /// </summary>
  INavigationHistory = interface
    {$REGION 'Property Getters & Setters'}
      function GetCanGoBack: Boolean;
      function GetCanGoForward: Boolean;
      function GetCurrentLocation: ILocatable;
    {$ENDREGION}
    procedure GoBack;
    procedure GoForward;
    procedure GoToLocation(const location: ILocatable);
    function GetLocations: ICollection<ILocatable>;
    function GetBackList: ICollection<ILocatable>;
    function GetForwardList: ICollection<ILocatable>;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property CurrentLocation: ILocatable read GetCurrentLocation;
  end;

  ENavigationHistory = class(Exception);

  // NOT READY
  TNavigationHistory = class(TInterfacedObject, INavigationHistory)
  private
    fLocations: ICollection<ILocatable>;
    fBackList: ICollection<ILocatable>;
    fForwardList: ICollection<ILocatable>;
    fCurrentLocation: ILocatable;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetCurrentLocation: ILocatable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GoBack;
    procedure GoForward;
    procedure GoToLocation(const location: ILocatable);
    function GetLocations: ICollection<ILocatable>;
    function GetBackList: ICollection<ILocatable>;
    function GetForwardList: ICollection<ILocatable>;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property CurrentLocation: ILocatable read GetCurrentLocation;
  end;

  TLocatableList = TListAdapter<ILocatable>;

implementation

{ TNavigationHistory }

constructor TNavigationHistory.Create;
begin
  inherited Create;
  fLocations := TContainer.CreateList<ILocatable>;
  fBackList := TContainer.CreateList<ILocatable>;
  fForwardList := TContainer.CreateList<ILocatable>;
end;

destructor TNavigationHistory.Destroy;
begin

  inherited Destroy;
end;

procedure TNavigationHistory.GoBack;
begin
  if fBackList.Count = 0 then
    raise ENavigationHistory.Create('Back List is empty.');
  //
end;

procedure TNavigationHistory.GoForward;
begin
  if fForwardList.Count = 0 then
    raise ENavigationHistory.Create('Forward List is empty.');
  //
end;

procedure TNavigationHistory.GoToLocation(const location: ILocatable);
begin

end;

function TNavigationHistory.GetLocations: ICollection<ILocatable>;
begin
  Result := fLocations;
end;

function TNavigationHistory.GetBackList: ICollection<ILocatable>;
begin
  Result := fBackList;
end;

function TNavigationHistory.GetForwardList: ICollection<ILocatable>;
begin
  Result := fForwardList;
end;

function TNavigationHistory.GetCanGoBack: Boolean;
begin
  Result := fBackList.Count > 0;
end;

function TNavigationHistory.GetCanGoForward: Boolean;
begin
  Result := fForwardList.Count > 0;
end;

function TNavigationHistory.GetCurrentLocation: ILocatable;
begin
  Result := fCurrentLocation;
end;

end.
