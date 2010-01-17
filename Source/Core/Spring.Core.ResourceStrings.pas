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

unit Spring.Core.ResourceStrings;

{$I Spring.inc}

interface

resourcestring

  // Spring.IoC.*

  SNonGuidInterfaceServicesAreNotSupported = 'Non-Guid Interface Services are not supported.';
  SIncompatibleTypes = 'The component type "%s" is incompatible with the service type "%s".';
  SDuplicatedName = 'Duplicated name found: "%s".';
  SCircularDependencyDetected = 'A circle was detected when trying to resolve the dependency: %s.';
  SCannotResolveDependency = 'Cannot resolve the dependency: %s.';
  SUnexpectedDependencyParameterType = 'Unexpected dependency parameter type.';
  SNoComponentRegistered = 'No component was registered for the service type: %s.';
  SInvalidServiceName = 'Invalid service name: "%s".';
  SUnsatisfiedConstructorParameters = 'Unsatisfied parameters for constructor.';
  SUnsatisfiedMethodParameterTypes = 'Unsatisfied parameter types for the method: %s.';
  SUnsatisfiedDependency = 'Unsatisfied dependency for the service type "%0:s".';
  SUnsatisfiedResolutionArgumentCount = 'Unsatisfied resolution argument count.';
  SLifetimeManagerMissing = 'LifetimeTypeManager was expected.';
  SUnresovableInjection = 'Unresovable injection.';
  SMemberHasNoTarget = 'The member injection "%s" has no target.';
  SUnsatisfiedConstructor = 'Unsatisfied constructor.';
  SMethodMustBeConstructor = 'The constructorMethod should be a constructor method.';
  SNoSuchMember = 'No such member: "%s".';
  SNoSuchMethod = 'No such method: "%s".';
  SNoSuchProperty = 'No such property: "%s".';
  SNoSuchField = 'No such field: "%s".';
  SInjectionTargetNeeded = 'Injection target needed.';
  SUnsatisfiedTarget = 'Unsatisfied target: "%s".';
  SComponentNotFound = 'Cannot found the component registered.';
  SUnexpectedLifetimeType = 'Unexpected lifetimeType.';

  SActivatorDelegateExpected = 'An activator delegate was expected.';

  // Spring.Logging

  SAllDescription      = 'ALL';
  STraceDescription    = 'TRACE';
  SDebugDescription    = 'DEBUG';
  SInfoDescription     = 'INFO';
  SWarnDescription     = 'WARN';
  SErrorDescription    = 'ERROR';
  SFatalDescription    = 'FATAL';
  SOffDescription      = 'OFF';

implementation

end.
