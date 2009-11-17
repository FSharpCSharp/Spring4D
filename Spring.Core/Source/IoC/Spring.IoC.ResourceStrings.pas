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

unit Spring.IoC.ResourceStrings;

interface

resourcestring

  SNonGuidInterfaceServicesAreNotSupported = 'Non-Guid Interface Services are not supported.';
  SCircularDependencyDetected = 'A circle was detected when trying to resolve the dependency: %s.';
  SCannotResolveDependency = 'Cannot resolve the dependency: %s.';
  SUnexpectedDependencyParameterType = 'Unexpected dependency parameter type.';
  SNoComponentFound = 'No component was registered for the service type: %s.';
  SLifetimeManagerWasExpected = 'LifetimeTypeManager was expected.';
  SMethodMustBeConstructor = 'The constructorMethod should be a constructor method.';


  {$REGION 'Spring.Logging'}

//  SAllDescription      = 'ALL';
//  STraceDescription    = 'TRACE';
//  SDebugDescription    = 'DEBUG';
//  SInfoDescription     = 'INFO';
//  SWarnDescription     = 'WARN';
//  SErrorDescription    = 'ERROR';
//  SFatalDescription    = 'FATAL';
//  SOffDescription      = 'OFF';

  {$ENDREGION}

implementation

end.
