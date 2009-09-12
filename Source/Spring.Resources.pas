{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
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

unit Spring.Resources;

interface

resourcestring
  SNotSupportedException       = '%s is not supported.';
  SNotImplementedException     = '%s is not implemented.';
  SArgumentOutOfRangeException = 'ArgumentOutOfRangeException: %s';
  SArgumentNullException       = 'ArgumentNullException: %s';
  SFileNotFoundException       = 'File not found: %s';
  SDirectoryNotFoundException  = 'Directory not found: %s';
  SNullableTypeHasNoValue      = 'Invalid operation, Nullable type has no value.';
  SCannotAssignPointerToNullable='Cannot assigned non-null pointer to nullable type.';
  STypeNotRegistered            = '%s was not registered.';
  SCannotModifyReadOnlyValue    = 'Cannot modify read-only value.';
  SServiceNotExists             = 'The service "%s" does not exist.';
  STimeoutException             = 'Timeout';
//  SEnumNotStarted              = 'Enum not started.';
//  SEnumEnded                   = 'Enum ended.';
//  SNotSupported_EnumReset      = 'RESET has not been supported.';

  SNoTypeInfo          = 'No type information found.';
  SUnexpectedTypeKind  = 'Unexpected type kind: %s.';
  SNotEnumeratedType   = 'Type "%s" is not enumerated type.';
  EInvalidEnumArgument = 'Invalid enum argument %s for %s, Actual value: %d.';
  SIllegalFieldCount   = 'fieldCount is more than the number of components defined in the current Version object.';

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

  SNoAdapterFound = 'Type %s has no adapter for %s.';

  SValidationRequired    = '%S is required.';
  SValidationRange       = '%s must be between %s and %s.';
  SValidationMinValue    = '%s must not be less than %s.';
  SValidationMaxValue    = '%s must not be greater than %s.';
  SValidationMinLength   = '%s must have at least %d characters.';
  SValidationMaxLength   = '%s must have at most %d characters.';

  SRepositoryNotFound          = 'Repository not registered for %s.';
  SObjectContextNotAvailable   = 'ObjectContext is not available.';

implementation

end.
