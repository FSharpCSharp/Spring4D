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
{                                                                           }
{                        KEEP IT SIMPLE & FAST!!!                           }
{                                                                           }
{***************************************************************************}

unit Spring.Logging.Appenders;

interface

uses
  Classes,
  SysUtils,
  Spring.System,
  Spring.Collections,
  Spring.Resources,
  Spring.Logging.Core;

type
  TAppenderAttachable = class(TInterfacedObject, IAppenderAttachable)
  private
    fList: ICollection<IAppender>;
    function GetAppenders: ICollection<IAppender>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAllAppenders;
    function GetAppender(const name: string): IAppender;
    function RemoveAppender(const name: string): IAppender; overload;
    function RemoveAppender(const appender: IAppender): IAppender; overload;
    property Appenders: ICollection<IAppender> read GetAppenders;
  end;

  TAppenderBase = class abstract(TInterfacedObject, IBulkAppender, IAppender, IOptionHandler, IInterface)
  private
    fName: string;
    fThreshold: TLevel;
    fLayout: ILayout;
    fHeadFilter: IFilter;
    fTailFilter: IFilter;
    fErrorHandler: IErrorHandler;
    fClosed: Boolean;
    fRecursiveGuard: Boolean;
    fRenderWriter: TStringWriter;   // TReusableStringWriter
    function GetName: string;
    procedure SetName(const value: string);
    procedure SetErrorHandler(const Value: IErrorHandler);
  private
    const
      fCRenderBufferSize = 256;
      fCRenderBufferMaxCapacity = 1024;
  protected
    procedure DoAppend(const loggingEvent: TLoggingEvent); overload; virtual; abstract;
    procedure DoAppend(const loggingEvents: ICollection<TLoggingEvent>); overload; virtual;
    procedure DoClose; virtual;
    function FilterEvent(const loggingEvent: TLoggingEvent): Boolean; virtual;
    function IsAsSevereAsThreshold(const level: TLevel): Boolean; virtual;
    function PreAppendCheck: Boolean;
    function RequiresLayout: Boolean; virtual;
    function RenderLoggingEvent(const loggingEvent: TLoggingEvent): string; overload;
    procedure RenderLoggingEvent(writer: TTextWriter; const loggingEvent: TLoggingEvent); overload;
  public
    constructor Create;
    destructor Destroy; override;
    { IOptionHandler }
    procedure ActivateOptions; virtual;
    { IBulkAppender }
    procedure Append(const loggingEvents: ICollection<TLoggingEvent>); overload;
    { Instance Methods }
    procedure AddFilter(const filter: IFilter); virtual;
    procedure ClearFilters; virtual;
    { IAppender }
    procedure Close;
    procedure Append(const loggingEvent: TLoggingEvent); overload;
    property Name: string read GetName write SetName;
    property Layout: ILayout read fLayout write fLayout;
    property ErrorHandler: IErrorHandler read fErrorHandler write SetErrorHandler;
  end;

  TConsoleAppender = class(TAppenderBase)

  end;

  TTextWriterAppender = class(TAppenderBase)

  end;

  TFileAppender = class(TTextWriterAppender)

  end;

  TRollingFileAppender = class(TFileAppender)

  end;

  TEventLogAppender = class(TAppenderBase)

  end;

  TAppenderList = TListAdapter<IAppender>;

implementation

uses Spring.Logging.Utils;

{$REGION 'TAppenderAttachable'}

constructor TAppenderAttachable.Create;
begin
  inherited Create;
  fList := TContainer.CreateList<IAppender>;
end;

destructor TAppenderAttachable.Destroy;
begin

  inherited;
end;

procedure TAppenderAttachable.AddAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fList.Add(appender);
end;

function TAppenderAttachable.GetAppender(const name: string): IAppender;
var
  appender: IAppender;
begin
  Result := nil;
  for appender in fList do
  begin
    if SameText(appender.Name, name) then
    begin
      Result := appender;
      Break;
    end;
  end;
end;

function TAppenderAttachable.GetAppenders: ICollection<IAppender>;
begin
  Result := fList;
end;

procedure TAppenderAttachable.RemoveAllAppenders;
begin
  fList.Clear;
end;

function TAppenderAttachable.RemoveAppender(const name: string): IAppender;
begin
  Result := GetAppender(name);
  Result := RemoveAppender(Result);
end;

function TAppenderAttachable.RemoveAppender(
  const appender: IAppender): IAppender;
begin
  TArgument.CheckNotNull(appender, 'appender');
  Result := appender;
  fList.Remove(Result);
end;

{$ENDREGION}


{$REGION 'TAppenderBase'}

procedure TAppenderBase.ActivateOptions;
begin
end;

procedure TAppenderBase.AddFilter(const filter: IFilter);
begin
  TArgument.CheckNotNull(filter, 'filter');
  if fHeadFilter = nil then
  begin
    fHeadFilter := filter;
    fTailFilter := filter;
  end
  else
  begin
    fTailFilter.Next := filter;
    fTailFilter := filter;
  end;
end;

procedure TAppenderBase.ClearFilters;
begin
  { TODO: Ensure that all the filters were cleaned up. }
  fHeadFilter := nil;
  fTailFilter := nil;
end;

procedure TAppenderBase.Close;
begin
  Lock(Self,
    procedure
    begin
      if not fClosed then
      begin
        DoClose;
        fClosed := True;
      end;
    end
  );
end;

constructor TAppenderBase.Create;
begin
  inherited Create;
  fErrorHandler := TOnlyOnceErrorHandler.Create(ClassName);
end;

procedure TAppenderBase.Append(const loggingEvent: TLoggingEvent);
begin
  Lock(Self,
    procedure
    begin
      if fClosed then
      begin
        ErrorHandler.Error('Attempted to append to closed appender named [' + fName + '].');
      end;
      if fRecursiveGuard then Exit;
      fRecursiveGuard := True;
      try
        try
          if FilterEvent(loggingEvent) and PreAppendCheck then
            DoAppend(loggingEvent);
        except on e: Exception do
          ErrorHandler.Error('Failed in Append', e);
        end;
      finally
        fRecursiveGuard := False;
      end;
    end
  );
end;

procedure TAppenderBase.Append(const loggingEvents: ICollection<TLoggingEvent>);
var
  filteredEvents: ICollection<TLoggingEvent>;
begin
  Lock(Self,
    procedure
    var
      loggingEvent: TLoggingEvent;
    begin
      if fClosed then
      begin
        ErrorHandler.Error('Attempted to append to closed appender named [' + fName + '].');
      end;
      if fRecursiveGuard then Exit;
      fRecursiveGuard := True;
      try
        try
//            filteredEvents := TListAdapter<TLoggingEvent>.Create;  // BUG-FIX: INTERNAL ERROR: L817
          for loggingEvent in loggingEvents do
          begin
            if FilterEvent(loggingEvent) then
              filteredEvents.Add(loggingEvent);
          end;
          if (filteredEvents.Count > 0) and PreAppendCheck then
            DoAppend(filteredEvents);
        except on e: Exception do
          ErrorHandler.Error('Failed in Bulk Append', e);
        end;
      finally
        fRecursiveGuard := False;
      end;
    end
  );
end;

destructor TAppenderBase.Destroy;
begin
  if not fClosed then
  begin
    TInternalLogger.Debug('TAppenderBase: Destroying appender named ['+fName+'].');
    Close;
  end;
  inherited Destroy;
end;

procedure TAppenderBase.DoAppend(
  const loggingEvents: ICollection<TLoggingEvent>);
var
  loggingEvent: TLoggingEvent;
begin
  for loggingEvent in loggingEvents do
  begin
    Append(loggingEvent);
  end;
end;

procedure TAppenderBase.DoClose;
begin
end;

function TAppenderBase.FilterEvent(const loggingEvent: TLoggingEvent): Boolean;
var
  filter: IFilter;
  decision: TFilterDecision;
begin
  Result := IsAsSevereAsThreshold(loggingEvent.Level);
  if not Result then Exit;
  filter := Self.fHeadFilter;
  while filter <> nil do
  begin
    decision := filter.Decide(loggingEvent);
    case decision of
      TFilterDecision.Deny:     Exit(False);
      TFilterDecision.Natural:  filter := filter.Next;
      TFilterDecision.Accept:   Exit(True);
    end;
  end;
end;

function TAppenderBase.IsAsSevereAsThreshold(const level: TLevel): Boolean;
begin
  Result := fThreshold.IsNull or (level >= fThreshold);
end;

function TAppenderBase.PreAppendCheck: Boolean;
begin
  Result := not RequiresLayout or (fLayout <> nil);
  if not Result then
  begin
    ErrorHandler.Error('AppenderSkeleton: No layout set for the appender named ['+fName+'].');
  end;
end;

function TAppenderBase.RenderLoggingEvent(
  const loggingEvent: TLoggingEvent): string;
begin
  if fRenderWriter = nil then
  begin
    fRenderWriter := TReusableStringWriter.Create;
  end;
  TReusableStringWriter(fRenderWriter).Reset(fCRenderBufferMaxCapacity, fCRenderBufferSize);
  RenderLoggingEvent(fRenderWriter, loggingEvent);
  Result := fRenderWriter.ToString;
end;

procedure TAppenderBase.RenderLoggingEvent(writer: TTextWriter;
  const loggingEvent: TLoggingEvent);
var
  exceptionString: string;
begin
  if fLayout = nil then
  begin
    raise EInvalidOperation.Create('A Layout must be set.');
  end;
  if fLayout.IgnoresException then
  begin
    exceptionString := loggingEvent.ExceptionString;
    if exceptionString <> '' then
    begin
      fLayout.Format(writer, loggingEvent);
      writer.WriteLine(exceptionString);
    end
    else
    begin
      fLayout.Format(writer, loggingEvent);
    end;
  end
  else
  begin
    fLayout.Format(writer, loggingEvent);
  end;
end;

function TAppenderBase.RequiresLayout: Boolean;
begin
  Result := False;
end;

function TAppenderBase.GetName: string;
begin
  Result := fName;
end;

procedure TAppenderBase.SetName(const value: string);
begin
  fName := value;
end;

procedure TAppenderBase.SetErrorHandler(const Value: IErrorHandler);
begin
  if fErrorHandler <> nil then
  begin
    fErrorHandler := Value;
  end
  else
  begin
    TInternalLogger.Warn('TAppenderBase: You have tried to set a null error-handler.');
  end;
end;

{$ENDREGION}

end.
