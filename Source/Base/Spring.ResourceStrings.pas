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

unit Spring.ResourceStrings;

interface

resourcestring

  {$REGION 'Spring.System'}

  SNotSupportedException       = '%s is not supported.';
  SNotImplementedException     = '%s is not implemented.';

  SArgumentOutOfRangeException = 'EArgumentOutOfRangeException: %s';
  SArgumentNullException       = 'EArgumentNullException: %s';
  SInvalidEnumArgument         = 'Invalid enum argument: %s';
  SUnexpectedTypeKindArgument  = 'Unexpected type kind %s for the argument %s.';

  SCannotAccessRegistryKey     = 'Cannot access the registry key: %s.';

  SAbstractClassCreation       = 'Cannot create the abstract class: %s.';

  SFileNotFoundException          = 'File not found: %s';
  SDirectoryNotFoundException     = 'Directory not found: %s';
  SNullableTypeHasNoValue         = 'Invalid operation, Nullable type has no value.';
  SCannotAssignPointerToNullable  ='Cannot assigned non-null pointer to nullable type.';
  STypeNotRegistered              = '%s was not registered.';
  SCannotModifyReadOnlyValue      = 'Cannot modify read-only value.';
  SServiceNotExists               = 'The service "%s" does not exist.';
  STimeoutException               = 'Timeout';
  SInsufficientMemoryException    = 'Insufficient memory.';

  SUnexpectedParameterLength = 'Unexpected parameter length.';

  SNoTypeInfo          = 'No type information found.';
  SUnexpectedTypeKind  = 'Unexpected type kind: %s.';
  SNotEnumeratedType   = 'Type "%s" is not enumerated type.';
  SIncorrectFormat     = 'Unable to convert %s.';
  SInvalidDateTime     = '"%S" is not a valid date and time.';
  SIllegalFieldCount   = 'fieldCount is more than the number of components defined in the current Version object.';

  SUnknownOSDescription      = 'Unknown Operating System';
  SWin95Description          = 'Microsoft Windows 95';
  SWin98Description          = 'Microsoft Windows 98';
  SWinMEDescription          = 'Microsoft Windows ME';
  SWinNT351Description       = 'Microsoft Windows NT 3.51';
  SWinNT40Description        = 'Microsoft Windows NT 4';
  SWinServer2000Description  = 'Microsoft Windows Server 2000';
  SWinXPDescription          = 'Microsoft Windows XP';
  SWinServer2003Description  = 'Microsoft Windows Server 2003';
  SWinVistaDescription       = 'Microsoft Windows Vista';
  SWinServer2008Description  = 'Microsoft Windows Server 2008';
  SWin7Description           = 'Microsoft Windows 7';


  SFileVersionInfoFormat =
    'File:             %s' + #13#10 +
    'InternalName:     %s' + #13#10 +
    'OriginalFilename: %s' + #13#10 +
    'FileVersion:      %s' + #13#10 +
    'FileDescription:  %s' + #13#10 +
    'Product:          %s' + #13#10 +
    'ProductVersion:   %s' + #13#10 +
    'Debug:            %s' + #13#10 +
    'Patched:          %s' + #13#10 +
    'PreRelease:       %s' + #13#10 +
    'PrivateBuild:     %s' + #13#10 +
    'SpecialBuild:     %s' + #13#10 +
    'Language:         %s' + #13#10;

  SUnknownDescription  = 'Unknown';
  SVersionDescription  = 'Version';
//  SOSVersionStringFormat = '%S Version %s %s';

  SSizeStringFormat    = '%s %s';   // e.g. '20.5 MB'

  SByteDescription     = 'Byte';
  SBytesDescription    = 'Bytes';
  SKBDescription       = 'KB';
  SMBDescription       = 'MB';
  SGBDescription       = 'GB';
  STBDescription       = 'TB';

  SDriveNotReady              = 'Drive "%S" is not ready.';

  SUnknownDriveDescription    = 'Unknown Drive';
  SNoRootDirectoryDescription = 'No Root Directory';
  SRemovableDescription       = 'Removable Drive';
  SFixedDescription           = 'Fixed Drive';
  SNetworkDescription         = 'Network Drive';
  SCDRomDescription           = 'CD-Rom Drive';
  SRamDescription             = 'Ram Drive';

  {$ENDREGION}


  {$REGION 'Spring.Collections'}

  SCannotResetEnumerator = 'Cannot reset the enumerator.';

  {$ENDREGION}


  {$REGION 'Spring.IoC'}

  SNonGuidInterfaceServicesAreNotSupported = 'Non-Guid Interface Services are not supported.';
  SCircularDependencyDetected = 'A circle was detected when trying to resolve the dependency: %s.';
  SCannotResolveDependency = 'Cannot resolve the dependency: %s.';
  SUnexpectedDependencyParameterType = 'Unexpected dependency parameter type.';
  SNoComponentFound = 'No component was registered for the service type: %s.';
  SLifetimeManagerWasExpected = 'LifetimeTypeManager was expected.';

  {$ENDREGION}


  {$REGION 'Spring.Logging'}

  SAllDescription      = 'ALL';
  STraceDescription    = 'TRACE';
  SDebugDescription    = 'DEBUG';
  SInfoDescription     = 'INFO';
  SWarnDescription     = 'WARN';
  SErrorDescription    = 'ERROR';
  SFatalDescription    = 'FATAL';
  SOffDescription      = 'OFF';

  {$ENDREGION}

implementation

end.
