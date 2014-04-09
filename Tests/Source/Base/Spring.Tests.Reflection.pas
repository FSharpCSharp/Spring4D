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

unit Spring.Tests.Reflection;

interface

{$I Spring.inc}
{$I Spring.Tests.inc}

uses
  TestFramework;

type
  TTestType = class(TTestCase)
  published
    procedure TestIsDelegateTypeWithTFunc;
    procedure TestIsDelegateTypeWithTProc;
    procedure TestIsDelegateTypeWithTPredicate;
    procedure TestIsDelegateTypeWithNil;
    procedure TestIsDelegateTypeWithIInterface;
    procedure TestIsDelegateTypeWithTObject;
  end;

implementation

uses
  SysUtils,
  Spring.Reflection;


{$REGION 'TTestType'}

procedure TTestType.TestIsDelegateTypeWithIInterface;
begin
  CheckFalse(TType.IsDelegate(TypeInfo(IInterface)));
end;

procedure TTestType.TestIsDelegateTypeWithNil;
begin
  CheckFalse(TType.IsDelegate(nil));
end;

procedure TTestType.TestIsDelegateTypeWithTFunc;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TFunc<Integer>)));
end;

procedure TTestType.TestIsDelegateTypeWithTObject;
begin
  CheckFalse(TType.IsDelegate(TypeInfo(TObject)));
end;

procedure TTestType.TestIsDelegateTypeWithTPredicate;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TPredicate<Integer>)));
end;

procedure TTestType.TestIsDelegateTypeWithTProc;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TProc<Integer>)));
end;

{$ENDREGION}


end.
