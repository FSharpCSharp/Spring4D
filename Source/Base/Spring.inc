{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 spring4d Team                           }
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

{$IFNDEF SPRING_INC}
{$DEFINE SPRING_INC}

// Import JEDI Development environment directives
// Will assert correct CPU type and defaults to CPU386 if CPU type is unknown
// (Delphi older than XE3).
{$I jedi.inc}

{$IFDEF DELPHIXE5_UP}
  {$LEGACYIFEND ON}
{$ENDIF}

// JEDI will assert that at least CPU386 is defined
{$IFDEF CPU386}
  // Keep this for older delphi compatibility (older than XE3)
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUARM}
  // Not used by default as it adds some more overhead due to closure copying
  // and is only required in few corner cases
  {.$DEFINE FIX_EVENT_INVOKES}
{$ENDIF}

{$IF Defined(CPUX86) OR Defined(CPUX64)}
  {$DEFINE SPRING_HAS_GENERIC_EVENTS}
{$ELSE}
  // Current System.Rtti doesn't support creating implementations that we need
  // so until that happens Events cannot be supported on non-Intel architectures
{$IFEND}

//If we're running on Android or iOS, we're sure we're running using FMX
//platform if FMX is to be used on x86/x64 FMX conditional should be defined
//in the project settings by hand
{$IFDEF ANDROID}
  {$DEFINE FMX}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE FMX}
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'This framework requires Delphi 2010 or higher.'}}
{$ENDIF}

{$IF Defined(MSWINDOWS)}

{$ELSE IF Defined(MACOS) OR Defined(IOS) or Defined(ANDROID)}
  {$IFNDEF SPRING_NO_WARNING}
    {$MESSAGE WARN 'This framework currently is experimental on Mac OS X, iOS, Android and has not been tested there yet.'}
  {$ENDIF}
{$ELSE}
  {$MESSAGE ERROR 'This framework currently only supports Windows (tested) and Mac OS X, iOS, Android (experimental).'}
{$IFEND}

{$IFNDEF NEXTGEN}
  {$WARN UNIT_PLATFORM OFF}    // Turn off UNIT_PLATFORM warning
  {$WARN SYMBOL_PLATFORM OFF}  // Turn off SYMBOL_PLATFORM warning
{$ENDIF}

{$ENDIF ~SPRING_INC}
