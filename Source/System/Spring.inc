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

{$I jedi.inc}                // Import JEDI Development environment directives

{$IFNDEF DELPHIXE_UP}
  {$MESSAGE ERROR 'This framework requires Delphi XE or higher.'}}
{$ENDIF}

{$IFDEF CPUX64}
  {$MESSAGE WARN 'This framework is not ready for Win64'}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'This framework is only available for Windows at present.'}
{$ENDIF}

{.$SCOPEDENUMS ON}           // Enable Scoped Enumerations

{$WARN UNIT_PLATFORM OFF}    // Turn off UNIT_PLATFORM warning
{$WARN SYMBOL_PLATFORM OFF}  // Turn off SYMBOL_PLATFORM warning

{$IFNDEF DELPHI2007_UP}
  {$DEFINE DisableEnhancedRecords}
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
  {$DEFINE DisableGenerics}
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
  {$DEFINE DisableReflection}
{$ENDIF}

{$ENDIF ~SPRING_INC}
