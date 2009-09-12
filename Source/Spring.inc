{***************************************************************************}
{                                                                           }
{               Delphi Spring Project                                       }
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

{$IFNDEF SPRING_INC}
{$DEFINE SPRING_INC}

{$I jedi.inc}                // Import JEDI Development environment directives

{$IFDEF VER210}
  {$DEFINE DELPHI2010_UP}
  {$DEFINE DELPHI2009_UP}
  {$DEFINE SUPPORTS_GENERICS}
  {$DEFINE SUPPORTS_ENHANCED_RECORDS}
  {$DEFINE SUPPORTS_REGION}
  {$DEFINE SUPPORTS_SEALED}
  {$DEFINE SUPPORTS_ABSTRACT}
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'Delphi Spring Framework should be compiled in Delphi 2010 or later.'}
{$ENDIF}

{$IFDEF DELPHI2006_UP}
  {$DEFINE SUPPORTS_REGION}  // Enable REGION Directive in Delphi 2006 or later.
{$ENDIF}

{.$SCOPEDENUMS ON}           // Enable Scoped Enumerations

{$WARN SYMBOL_PLATFORM OFF}

{$ENDIF ~SPRING_INC}
