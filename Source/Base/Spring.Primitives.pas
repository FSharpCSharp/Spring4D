{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 Alexandru Ciobanu                       }
{                                                                           }
{           http://alex.ciobanu.org                                         }
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

unit Spring.Primitives;
interface
uses SysUtils,
     TypInfo,
     Rtti,
     Generics.Defaults,
     Spring,
     Spring.Collections;

{$REGION 'Comparer'}
type
  ///  <summary>Defines the basic traits of a comparer.</summary>
  IComparer = interface
    ['{C8A264DA-A4E0-4AA6-B312-FA977E4C0CA0}']

    ///  <summary>Compares two values of the described type.</summary>
    ///  <param name="ALeft">The value that is being compared.</param>
    ///  <param name="ARight">The value that is being compared to.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <paramref name="ALeft"/> is less than <paramref name="ARight"/>. If the result is zero,
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>. And finally, if the result is greater than zero,
    ///  <paramref name="ALeft"/> is greater than <paramref name="ARight"/>.</returns>
    function Compare(const ALeft, ARight: TValue): Integer;
  end;

  ///  <summary>The metaclass for the comparer class. Used when registering custom comparers.</summary>
  TComparerClass = class of TComparer;

  ///  <summary>The abstract and facade comparer class.</summary>
  TComparer = class abstract(TInterfacedObject, IComparer)
  private
    class var FKnownTypes: IDictionary<PTypeInfo, TComparerClass>;
    class var FLock: TObject;

    class constructor Create;
    class destructor Destroy;
  public
    ///  <summary>Compares two values of the described type.</summary>
    ///  <param name="ALeft">The value that is being compared.</param>
    ///  <param name="ARight">The value that is being compared to.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <paramref name="ALeft"/> is less than <paramref name="ARight"/>. If the result is zero,
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>. And finally, if the result is greater than zero,
    ///  <paramref name="ALeft"/> is greater than <paramref name="ARight"/>.</returns>
    function Compare(const ALeft, ARight: TValue): Integer; virtual; abstract;

    ///  <summary>Creates a new instance of this comparer type.</summary>
    ///  <remarks>This method is abtract and should be implemented in descendant classes.</remarks>
    constructor Create; virtual; abstract;
  end;

  ///  <summary>The abstract and facade generic comparer class.</summary>
  TComparer<T> = class abstract(TComparer, IComparer, IComparer<T>)
  private class var
    FCachedComparer: IComparer<T>;

  public
    ///  <summary>Compares two values of the described type.</summary>
    ///  <param name="ALeft">The value that is being compared.</param>
    ///  <param name="ARight">The value that is being compared to.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <paramref name="ALeft"/> is less than <paramref name="ARight"/>. If the result is zero,
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>. And finally, if the result is greater than zero,
    ///  <paramref name="ALeft"/> is greater than <paramref name="ARight"/>.</returns>
    function Compare(const ALeft, ARight: T): Integer; reintroduce; overload; virtual; abstract;

    ///  <summary>Compares two values of the described type.</summary>
    ///  <param name="ALeft">The value that is being compared.</param>
    ///  <param name="ARight">The value that is being compared to.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <paramref name="ALeft"/> is less than <paramref name="ARight"/>. If the result is zero,
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>. And finally, if the result is greater than zero,
    ///  <paramref name="ALeft"/> is greater than <paramref name="ARight"/>.</returns>
    function Compare(const ALeft, ARight: TValue): Integer; overload; override;

    ///  <summary>Retreives the default comparer for the given type.</summary>
    ///  <returns>A new comparer interface that can compare instances of the given type.</returns>
    class function Default: IComparer<T>;

    ///  <summary>Registers a new custom comparer.</summary>
    ///  <param name="AComparer">The class of the comparer to register.</param>
    class procedure Register(const AComparer: TComparerClass);

    ///  <summary>Unregisters a custom comparer for this type.</summary>
    class procedure Unregister();
  end;

  ///  <summary>An adapter for the RTL's comparer interfaces.<summary>
  TRTLComparerAdapter<T> = class(Spring.Primitives.TComparer<T>)
  private
    FRTLComparer: IComparer<T>;

  public
    ///  <summary>Compares two values of the described type.</summary>
    ///  <param name="ALeft">The value that is being compared.</param>
    ///  <param name="ARight">The value that is being compared to.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <paramref name="ALeft"/> is less than <paramref name="ARight"/>. If the result is zero,
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>. And finally, if the result is greater than zero,
    ///  <paramref name="ALeft"/> is greater than <paramref name="ARight"/>.</returns>
    function Compare(const ALeft, ARight: T): Integer; override;

    ///  <summary>Prepares this adapter class.</summary>
    ///  <remarks>Internally this constructor requests an RTL comparer interface that will be used.</remarks>
    constructor Create; override;
  end;
{$ENDREGION}

implementation
uses
  Spring.ResourceStrings;

{ TRTLComparerAdapter<T> }

function TRTLComparerAdapter<T>.Compare(const ALeft, ARight: T): Integer;
begin
  { Use the RTL comparer for our dirty deeds }
  Result := FRTLComparer.Compare(ALeft, ARight);
end;

constructor TRTLComparerAdapter<T>.Create;
begin
  { Obtain the RTL comparer }
  FRTLComparer := Generics.Defaults.TComparer<T>.Default;
end;

{ TComparer }

class constructor TComparer.Create;
begin
  FLock := TObject.Create;
end;

class destructor TComparer.Destroy;
begin
  FLock.Free;
end;

{ TComparer<T> }

function TComparer<T>.Compare(const ALeft, ARight: TValue): Integer;
begin
  Result := Compare(ALeft.AsType<T>, ARight.AsType<T>);
end;

class function TComparer<T>.Default: IComparer<T>;
var
  LTypeInfo: PTypeInfo;
  LClass: TComparerClass;
  LComparer: TComparer<T>;
begin
  LTypeInfo := TypeInfo(T);
  ASSERT(Assigned(LTypeInfo));

  { Check if we already have a comparer }
  if Assigned(FCachedComparer) then
    Result := FCachedComparer
  else begin
    MonitorEnter(FLock);
    try
      { Either collect the custom class or go to RTL for answers }
      if FKnownTypes.TryGetValue(LTypeInfo, LClass) then
        LComparer := LClass.Create() as TComparer<T>
      else
        LComparer := TRTLComparerAdapter<T>.Create();

      { Extract the interafce and register a known memory leak! This is related to an RTL bug. }
      Result := LComparer;
      RegisterExpectedMemoryLeak(LComparer);

      { Cache this comparer for later }
      FCachedComparer := Result;
    finally
      MonitorExit(FLock);
    end;
  end;
end;

class procedure TComparer<T>.Register(const AComparer: TComparerClass);
var
  LTypeInfo: PTypeInfo;
begin
  { Check stuff }
  TArgument.CheckNotNull(AComparer, 'AComparer');
  TArgument.CheckInheritsFrom(AComparer, TComparer<T>, 'AComparer');

  { Get the type info }
  LTypeInfo := TypeInfo(T);
  ASSERT(Assigned(LTypeInfo));

  { Simply register the type. And clear the stored interface. }
  FKnownTypes[LTypeInfo] := AComparer;
  FCachedComparer := nil;
end;

class procedure TComparer<T>.Unregister;
var
  LTypeInfo: PTypeInfo;
begin
  { Get the type info }
  LTypeInfo := TypeInfo(T);
  ASSERT(Assigned(LTypeInfo));

  { Simply unregister the type. And clear the stored interface. }
  FKnownTypes.Remove(LTypeInfo);
  FCachedComparer := nil;
end;

end.
