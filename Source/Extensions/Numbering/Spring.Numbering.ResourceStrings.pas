{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Numbering.ResourceStrings;

{$I Spring.inc}

interface

resourcestring
  // Spring.Numbering

  SIllegalNumber           = 'Illegal number: "%s".';
  SIllegalNumberLength     = 'Illegal number length: "%s".';
  SIllegalElement          = 'Illegal number element.';
  SUnexpectedCode          = 'Illegal number element. "%S" was unexpected.';
  SIllegalNumberEndsWith   = 'Illegal number: "%0:S". It can not end with "%1:S"';
  SInvalidDateTime         = '"%S" is not a valid date and time';
  SNumberOutOfRange        = '"%S"''s out of range.';
  SNumberOverflow          = 'The number "%s" will be overflow.';

implementation

end.
