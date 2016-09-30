{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Data.Sorting;

interface

uses
  DB,
  Rtti,
  Spring.Collections,
  Spring.Data.IndexList;

type
  TCompareRecords = function(const Item1, Item2: TObject): Integer of object;

  TMergeSort = class sealed
  private
    class procedure MergeSort(ALow, AHigh: Integer; const ADataList: TIndexList; const Compare: TCompareRecords);
  public
    class procedure Sort(const ADataList: TIndexList; const AComparator: TCompareRecords);
  end;

  TInsertionSort = class sealed
  private
    class procedure InsertionSort(ALow, AHigh: Integer; const ADataList: TIndexList; const Compare: TCompareRecords);
  public
    class procedure Sort(AStartIndex: Integer; const ADataList: TIndexList; const AComparator: TCompareRecords);
  end;

implementation

{ TMergeSort }

class procedure TMergeSort.MergeSort(ALow, AHigh: Integer;
  const ADataList: TIndexList; const Compare: TCompareRecords);
var
  LCache: TArray<TObject>;

  procedure Merge(Low, Mid, High: Integer);
  var
    i, j, k: Integer;
  begin
    for i := Low to High do
      LCache[i] := ADataList.Objects[i];
    i := Low;
    j := Mid + 1;
    k := Low;
    while (i <= Mid) and (j <= High) do
    begin
      if Compare(LCache[i], LCache[j]) <= 0 then
      begin
        ADataList.Objects[k] := LCache[i];
        Inc(i);
      end
      else
      begin
        ADataList.Objects[k] := LCache[j];
        Inc(j);
      end;
      Inc(k);
    end;

    while i <= Mid do
    begin
      ADataList.Objects[k] := LCache[i];
      Inc(k);
      Inc(i);
    end;
  end;

  procedure PerformMergeSort(ALowIndex, AHighIndex: Integer; CompareMethod: TCompareRecords);
  var
    iMid: Integer;
  begin
    if ALowIndex < AHighIndex then
    begin
      iMid:= (AHighIndex + ALowIndex) div 2;
      PerformMergeSort(ALowIndex, iMid, CompareMethod);
      PerformMergeSort(iMid + 1, AHighIndex, CompareMethod);
      Merge(ALowIndex, iMid, AHighIndex);
    end;
  end;

begin
  SetLength(LCache, ADataList.Count);
  PerformMergeSort(ALow, AHigh, Compare);
end;

class procedure TMergeSort.Sort(const ADataList: TIndexList;
  const AComparator: TCompareRecords);
begin
  MergeSort(0, ADataList.Count - 1, ADataList, AComparator);
end;

{ TInsertionSort }

class procedure TInsertionSort.InsertionSort(ALow, AHigh: Integer;
   const ADataList: TIndexList; const Compare: TCompareRecords);
var
  i, j : Integer;
  LTemp: TObject;
Begin
  for i := ALow + 1 to AHigh Do
  begin
    LTemp := ADataList.Objects[i];
    j := i;
    while (j > 0) and (Compare(ADataList.Objects[j - 1], LTemp) > 0) do
    begin
      ADataList.Objects[j] := ADataList.Objects[j - 1];
      Dec(j);
    end;
    ADataList.Objects[j] := LTemp;
  end;
end;

class procedure TInsertionSort.Sort(AStartIndex: Integer;
  const ADataList: TIndexList; const AComparator: TCompareRecords);
begin
  AStartIndex := AStartIndex - 1;
  if AStartIndex < 0 then
    AStartIndex := 0;
  InsertionSort(AStartIndex, ADataList.Count - 1, ADataList, AComparator);
end;

end.
