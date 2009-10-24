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

{TODO -oOwner -cGeneral : TRectHelper}
{TODO -oOwner -cGeneral : TSizeHelper}
{TODO -oOwner -cGeneral : TPointHelper}
{TODO -oOwner -cGeneral : TControlHelper}

unit Spring.Helpers experimental;

{$I Spring.inc}

interface

uses
  Classes,
  Types,
  SysUtils,
  ComObj,
  Spring.DesignPatterns;

type
  /// <summary>
  /// Record helper for TGuid
  /// </summary>
  TGuidHelper = record helper for TGuid
  private
    class function GetEmpty: TGuid; static;
    function GetIsEmpty: Boolean;
  public
    class function Create(const guidString: string): TGuid; static;
    class function NewGuid: TGuid; static;
    function Equals(const guid: TGuid): Boolean;
    function ToString: string;
    function ToQuotedString: string;
    property IsEmpty: Boolean read GetIsEmpty;
    class property Empty: TGuid read GetEmpty;
  end;

  TMethodHelper = record helper for TMethod
  public
    class function Create(const objectAddress, methodAddress: Pointer): TMethod; static;
  end;

  ISnapshot = Spring.DesignPatterns.ISnapshot;

  TPersistentSnapshot = class(TInterfacedObject, ISnapshot, IInterface)
  private
    fSnapshot: TPersistent;
  protected
    property Snapshot: TPersistent read fSnapshot;
  public
    constructor Create(snapshot: TPersistent);
    destructor Destroy; override;
  end deprecated;

  TPersistentHelper = class helper for TPersistent
  public
    function CreateSnapshot<T: TPersistent, constructor>: ISnapshot;
    procedure Restore(const snapshot: ISnapshot);
  end deprecated;

  (*

  TPointHelper = record helper for TPoint

  end;

  TSizeHelper = record helper for TSize

  end;

  TRectHelper = record helper for TRect

  end;

  //*)

implementation


{$REGION 'TGuidHelper'}

class function TGuidHelper.Create(const guidString: string): TGuid;
begin
  Result := StringToGUID(guidString);
end;

class function TGuidHelper.NewGuid: TGuid;
begin
  ComObj.OleCheck(SysUtils.CreateGUID(Result));
end;

function TGuidHelper.Equals(const guid: TGuid): Boolean;
begin
  Result := SysUtils.IsEqualGUID(Self, guid);
end;

function TGuidHelper.GetIsEmpty: Boolean;
begin
  Result := Self.Equals(TGuid.Empty);
end;

function TGuidHelper.ToString: string;
begin
  Result := SysUtils.GUIDToString(Self);
end;

function TGuidHelper.ToQuotedString: string;
begin
  Result := QuotedStr(Self.ToString);
end;

class function TGuidHelper.GetEmpty: TGuid;
const
  EmptyGuid: TGUID = (
    D1: 0;
    D2: 0;
    D3: 0;
    D4: (0, 0, 0, 0, 0, 0, 0, 0);
  );
begin
  Result := EmptyGuid;
end;

//class operator TGuidHelper.Equal(const left, right: TGuid) : Boolean;
//begin
//  Result := left.Equals(right);
//end;
//
//class operator TGuidHelper.NotEqual(const left, right: TGuid) : Boolean;
//begin
//  Result := not left.Equals(right);
//end;

{$ENDREGION}


{$REGION 'TMethodHelper'}

class function TMethodHelper.Create(const objectAddress,
  methodAddress: Pointer): TMethod;
begin
  Result.Code := methodAddress;
  Result.Data := objectAddress;
end;

{$ENDREGION}


{$REGION 'TPersistentSnapshot'}

constructor TPersistentSnapshot.Create(snapshot: TPersistent);
begin
  inherited Create;
  fSnapshot := snapshot;
end;

destructor TPersistentSnapshot.Destroy;
begin
  fSnapshot.Free;
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TPersistentHelper'}

function TPersistentHelper.CreateSnapshot<T>: ISnapshot;
var
  storage: T;
begin
  storage := T.Create;
  try
    storage.Assign(Self);
  except
    storage.Free;
    raise;
  end;
  Result := TPersistentSnapshot.Create(storage);
end;

procedure TPersistentHelper.Restore(const snapshot: ISnapshot);
begin
  if snapshot is TPersistentSnapshot then
  begin
    Assign(TPersistentSnapshot(snapshot).Snapshot);
  end;
end;

{$ENDREGION}

end.
