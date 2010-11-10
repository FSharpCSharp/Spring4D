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

unit Spring.Logging.Repositories;

interface

uses
  Classes,
  SysUtils,
  XmlIntf,
  SyncObjs,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.ResourceStrings,
  Spring.Logging,
  Spring.Logging.Core,
  Spring.Logging.Utils;

type
  /// <summary>
  /// Internal implementation for the unique root logger.
  /// </summary>
  TRootLogger = class sealed(TLogger)
  protected
    function GetEffectiveLevel: TLevel; override;
    procedure SetLevel(const value: TLevel); override;
  public
    constructor Create(const repository: ILoggerRepository);
  end;

  // NEED REVIEW

  /// <summary>
  /// TLoggerRepositoryBase
  /// </summary>
  TLoggerRepositoryBase = class abstract(TInterfacedObject, ILoggerRepository)
  private
    fName: string;
//    fConfigured: Boolean;
    fThreshold: TLevel;
    function GetName: string;
    function GetThreshold: TLevel;
    procedure SetName(const value: string);
    procedure SetThreshold(const value: TLevel);
  protected
    procedure CallOnConfigurationChanged;
    procedure CallOnConfigurationReset;
    procedure CallOnShutdown;
  public
    constructor Create;
    destructor Destroy; override;
//    procedure Shutdown; virtual;
    function FindLogger(const name: string): ILogger; virtual;
    function FindAppender(const name: string): IAppender; virtual;
    function GetAppenders: ICollection<IAppender>; virtual; abstract;
    function GetCurrentLoggers: ICollection<ILogger>; virtual; abstract;
    function GetLogger(const name: string): ILogger; virtual; abstract;
    property Name: string read GetName write SetName;
    property Threshold: TLevel read GetThreshold write SetThreshold;
  end;

  /// <summary>
  /// Hierarchical organization of loggers.
  /// </summary>
  TLoggerRepository = class(TLoggerRepositoryBase)
  private
    fLoggers: IDictionary<string, ILogger>;
    fAppenders: IList<IAppender>;
    fRoot: IHierarchyLogger;
    fLock: TCriticalSection;
    fEmittedNoAppenderWarning: Boolean;
    procedure UpdateParent(const logger: IHierarchyLogger);
  protected
    function AddLogger(const name: string): IHierarchyLogger;
  protected
    procedure Lock;
    procedure Unlock;
    procedure ConfigurationChanged;
  public
    constructor Create;
    destructor Destroy; override;
    function IsDisabled(const level: TLevel): Boolean;
    function FindLogger(const name: string): ILogger; override;
    function FindAppender(const name: string): IAppender; override;
    function GetAppenders: ICollection<IAppender>; override;
    function GetCurrentLoggers: ICollection<ILogger>; override;
    function GetLogger(const name: string): ILogger; override;
    property EmittedNoAppenderWarning: Boolean read fEmittedNoAppenderWarning write fEmittedNoAppenderWarning;
    property Root: IHierarchyLogger read fRoot;
  end;

implementation

uses
  StrUtils;

function LastIndexOf(const value: Char; const s: string): Integer; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] = value then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{$REGION 'TRootLogger'}

constructor TRootLogger.Create(const repository: ILoggerRepository);
begin
  inherited Create(repository, '');
  SetLevel(TLevel.Debug);
end;

function TRootLogger.GetEffectiveLevel: TLevel;
begin
  Result := Self.Level;
end;

procedure TRootLogger.SetLevel(const value: TLevel);
begin
  if value = nil then
  begin
    TInternalLogger.Debug('RootLogger: You have tried to set a null level to root.');
  end
  else
  begin
    inherited SetLevel(value);
  end;
end;

{$ENDREGION}


{$REGION 'TLoggerRepository'}

constructor TLoggerRepository.Create;
begin
  inherited Create;
  fLoggers := TCollections.CreateDictionary<string, ILogger>;
  fRoot := TRootLogger.Create(Self);
  fThreshold := TLevel.All;
  fLock := TCriticalSection.Create;
end;

destructor TLoggerRepository.Destroy;
begin
  fLock.Free;
  inherited Destroy;
end;

function TLoggerRepository.FindAppender(const name: string): IAppender;
var
  appender: IAppender;
begin
  Result := nil;
  for appender in GetAppenders do
  begin
    if SameText(appender.Name, name) then
    begin
      Result := appender;
      Exit;
    end;
  end;
end;

function TLoggerRepository.FindLogger(const name: string): ILogger;
begin
  Lock;
  try
    fLoggers.TryGetValue(name, Result);
  finally
    Unlock;
  end;
end;

function TLoggerRepository.AddLogger(const name: string): IHierarchyLogger;
begin
  Result := TLogger.Create(Self, name);
  fLoggers[name] := Result;
end;

function TLoggerRepository.GetAppenders: ICollection<IAppender>;
begin
  Result := fAppenders;
end;

function TLoggerRepository.GetCurrentLoggers: ICollection<ILogger>;
begin
  Result := fLoggers.Values;
end;

function TLoggerRepository.GetLogger(const name: string): ILogger;
var
  logger: ILogger;
begin
  Lock;
  try
    if not fLoggers.TryGetValue(name, logger) then
    begin
      logger := AddLogger(name);
      UpdateParent(logger as IHierarchyLogger);
    end;
  finally
    Unlock;
  end;
  Result := logger;
end;

// Loop through parents of the specified logger.
// if name = 'a.b.c.d', then loop through 'a.b.c', 'a.b' and 'a'
procedure TLoggerRepository.UpdateParent(const logger: IHierarchyLogger);
var
  name: string;
  parent: IHierarchyLogger;
  index: Integer;
begin
  name := logger.Name;
  index := LastIndexOf('.', name);
  if index > 0 then
  begin
    Delete(name, index, Length(name) - index + 1);
    parent := GetLogger(name) as IHierarchyLogger;
  end
  else
  begin
    parent := Root;
  end;
  logger.SetParent(parent);
end;

function TLoggerRepository.IsDisabled(const level: TLevel): Boolean;
begin
  TArgument.CheckNotNull(level <> nil, 'level');
  Result := Threshold.IsGreaterThan(level);
end;

procedure TLoggerRepository.Lock;
begin
  fLock.Enter;
end;

procedure TLoggerRepository.Unlock;
begin
  fLock.Leave;
end;

//procedure TLoggerRepository.ResetConfiguration;
//begin
//  TInternalLogger.Debug('THierarchy: ResetConfiguration called on Hierarchy [' + Name + ']');
//  Root.CloseNestedAppenders;
//  Lock(fLoggers,
//    procedure
//    var
//      collection: ICollection<ILogger>;
//      logger: ILogger;
//    begin
//      Shutdown;
//      collection := Self.GetCurrentLoggers;
//      for logger in collection do
//      begin
//        TLogger(logger).Level := nil;
//        TLogger(logger).Additivity := True;
//      end;
//    end
//  );
//  inherited ResetConfiguration;
//end;

//procedure TLoggerRepository.Shutdown;
//begin
//  TInternalLogger.Debug('THierarchy: Shutdown called on Hierarchy [' + Name + ']');
//  Root.CloseNestedAppenders;
//  Lock(fLoggers,
//    procedure
//    var
//      collection: ICollection<ILogger>;
//      logger: ILogger;
//    begin
//      collection := Self.GetCurrentLoggers;
//      for logger in collection do
//      begin
//        TLogger(logger).CloseNestedAppenders;
//      end;
//      Root.RemoveAllAppenders;
//      for logger in collection do
//      begin
//        TLogger(logger).RemoveAllAppenders;
//      end;
//    end
//  );
//  inherited Shutdown;
//end;

procedure TLoggerRepository.ConfigurationChanged;
begin
  CallOnConfigurationChanged;
end;

{$ENDREGION}


{$REGION 'TLoggerRepositoryBase'}

constructor TLoggerRepositoryBase.Create;
begin
  inherited Create;
  fThreshold := TLevel.All;
end;

destructor TLoggerRepositoryBase.Destroy;
begin
  inherited Destroy;
end;

function TLoggerRepositoryBase.FindAppender(const name: string): IAppender;
begin
  Result := nil;
end;

function TLoggerRepositoryBase.FindLogger(const name: string): ILogger;
begin
  Result := nil;
end;

procedure TLoggerRepositoryBase.CallOnConfigurationChanged;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnConfigurationChanged;
//    end
//  );
end;

procedure TLoggerRepositoryBase.CallOnConfigurationReset;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnConfigurationReset;
//    end
//  );
end;

procedure TLoggerRepositoryBase.CallOnShutdown;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnShutdown;
//    end
//  );
end;

function TLoggerRepositoryBase.GetName: string;
begin
  Result := fName;
end;

function TLoggerRepositoryBase.GetThreshold: TLevel;
begin
  Result := fThreshold;
end;

procedure TLoggerRepositoryBase.SetName(const value: string);
begin
  fName := value;
end;

procedure TLoggerRepositoryBase.SetThreshold(const value: TLevel);
begin
  if value <> nil then
  begin
    fThreshold := value;
  end
  else
  begin
    TInternalLogger.ErrorFmt('%s: Threshold cannot be set to null. Setting to ALL', [ClassName]);
    fThreshold := TLevel.All;
  end;
end;

{$ENDREGION}

end.
