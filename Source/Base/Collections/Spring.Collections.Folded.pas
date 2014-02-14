{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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

unit Spring.Collections.Folded;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Collections.Lists;

type
  TList<T> = class(Spring.Collections.Lists.TList<IInterface>)
  protected
    function GetElementType: PTypeInfo; override;
  end;

  TObjectList<T> = class(Spring.Collections.Lists.TObjectList<TObject>, IObjectList)
  protected
    function GetElementType: PTypeInfo; override;
  end;

  TCollectionsHelper = class helper for TCollections
  public
    class function CreateList<T>: IList<T>; overload; static;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateObjectList<T: class>(ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
  end;

implementation

uses
  TypInfo;


{$REGION 'TList<T>'}

function TList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

function TObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TCollectionsHelper'}

class function TCollectionsHelper.CreateList<T>: IList<T>;
var
  info: PTypeInfo;
begin
  info := TypeInfo(T);
  case info.Kind of
    tkClass: IList<TObject>(Result) := TObjectList<T>.Create(False);
    tkInterface: IList<IInterface>(Result) := TList<T>.Create;
  else
    raise EInvalidOperationException.CreateFmt('cannot create folded list for type: "%s"', [UTF8ToString(info.Name)]);
  end;
end;

class function TCollectionsHelper.CreateList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  IList<TObject>(Result) := TObjectList<T>.Create(ownsObjects);
end;

class function TCollectionsHelper.CreateObjectList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  IList<TObject>(Result) := TObjectList<T>.Create(ownsObjects);
end;

class function TCollectionsHelper.CreateObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  IList<TObject>(Result) := TObjectList<T>.Create(ownsObjects);
  Result.AddRange(values);
end;

{$ENDREGION}


end.
