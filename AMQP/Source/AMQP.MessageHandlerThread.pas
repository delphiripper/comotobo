unit AMQP.MessageHandlerThread;

interface
uses
  Classes,
  SysUtils,
  AMQP.Interfaces,
  AMQP.Classes,
  AMQP.Connection,
  AMQP.Message
  ;

type

  { TAMQPMessageHandlerThread }

  TAMQPMessageHandlerThread = class(TThread)
  private
    FQueue: TAMQPMessageQueue;
    FConnection: TAMQPConnection;
    FChannel: IAMQPChannel;
    FQueueName: string;
    FWasConnected: Boolean;
    FPrefetchCount: Integer;
    FConsumerTag: String;
    FStartTime, FStopTime: Int64;
    FCount: UInt64;
    FMVal: Int64;
    FSpeed: Double;
  protected
    function CheckConnection: Boolean; virtual;
    procedure DoDisconnected; virtual;
    procedure DoHandleMessage(AMsg: TAMQPMessage; var AHandled: Boolean); virtual;
    procedure DoConnected; virtual;
    procedure DoAfterOpenChannel(AChannel: IAMQPChannel); virtual;
    procedure Execute; override;
    procedure DoOnMeasure; virtual;
    procedure DoOnStopThread; virtual;
  public
    constructor Create(AConnection: TAMQPConnection; AQueueName: string; APrefetchCount: Integer); virtual;
    destructor Destroy; override;
    procedure SendNull;
    property Channel: IAMQPChannel read FChannel;
    property Connection: TAMQPConnection read FConnection;
    property Count: Uint64 read FCount;
    property Speed: Double read FSpeed;
    property MVal: Int64 read FMVal;
  end;

implementation



{ TAMQPMessageHandlerThread }

function TAMQPMessageHandlerThread.CheckConnection: Boolean;
begin
  Result := False;
  if not FConnection.IsOpen then
  begin
    if FWasConnected then
     begin
       FWasConnected := false;
       DoDisconnected;
     end;
    try
      FConnection.Connect;
      FWasConnected := True;
      FChannel := nil;
      FChannel := FConnection.OpenChannel(0, FPrefetchCount);
      DoAfterOpenChannel(FChannel);
      FChannel.BasicConsume(FQueue, FQueueName, FConsumerTag);
      DoConnected;
      Sleep(10);
      Result := True;
    except
      FConnection.AbortConnection;
      Result := False;
    end;
  end else
   Result := True;
end;

constructor TAMQPMessageHandlerThread.Create(AConnection: TAMQPConnection; AQueueName: string; APrefetchCount: Integer);
begin
 FConnection := AConnection;
 FQueueName := AQueueName;
 FQueue := TAMQPMessageQueue.Create;
 FWasConnected := False;
 FPrefetchCount := APrefetchCount;
 FConsumerTag := TGUID.NewGuid.ToString;
 FConnection.Disconnect;
 inherited Create(False);
 {$IfDef UNIX}
  _SetThreadName(Self.ThreadID, 'MsgHndl');
 {$Else}
  Self.NameThreadForDebugging('MsgHndl', Self.ThreadID);
 {$EndIf}
end;

destructor TAMQPMessageHandlerThread.Destroy;
begin
  FChannel.Close;
  FreeAndNil(FQueue);
  inherited;
end;

procedure TAMQPMessageHandlerThread.DoConnected;
begin

end;

procedure TAMQPMessageHandlerThread.DoAfterOpenChannel(AChannel: IAMQPChannel);
begin

end;

procedure TAMQPMessageHandlerThread.DoOnMeasure;
begin

end;

procedure TAMQPMessageHandlerThread.DoDisconnected;
begin

end;

procedure TAMQPMessageHandlerThread.DoHandleMessage(AMsg: TAMQPMessage; var AHandled: Boolean);
begin
  AHandled := True;
end;

procedure TAMQPMessageHandlerThread.Execute;
var Msg: TAMQPMessage;
    FHandled: Boolean;
    FDiff: Int64;
    FOldCount: UInt64;
begin
  FCount := 0;
  FOldCount:=0;
  {$IFDEF FPC}
   FStartTime:=GetTickCount64;
  {$ELSE}
   FStartTime:=GetTickCount;
  {$ENDIF}
  try
    repeat
     if CheckConnection then
     begin
      try
        Msg := FQueue.Get(3000);
        if Msg = nil then

        else
        if Pointer(Msg) = pointer(1) then
         Terminate
        else
        try
          try
            DoHandleMessage(Msg, FHandled);
            if FHandled then
              Msg.Ack;
          except
             Msg.Reject;
          end;
        finally
         Inc(FCount);
         if FHandled then
           FreeAndNil(Msg);
        end;
      except
        on E: AMQPTimeout do ;
      end;
    end else
     Sleep(5000);

      {$IFDEF FPC}
      FStopTime:= GetTickCount64;
      {$ELSE}
      FStopTime:= GetTickCount;
      {$ENDIF}
      FDiff := FStopTime - FStartTime;
      if FDiff >= 3000 then
       begin
         FStartTime := FStopTime;
         FMVal:= FCount - FOldCount;
         FSpeed := FMVal / (FDiff / 1000);
         FOldCount := FCount;
         DoOnMeasure;
       end;

    until Terminated;

  except
    On E: Exception do
     Writeln(E.ClassName+':'+E.Message);
  end;
  DoOnStopThread;
end;

procedure TAMQPMessageHandlerThread.DoOnStopThread;
begin

end;

procedure TAMQPMessageHandlerThread.SendNull;
begin
 FQueue.Put(TAMQPMessage(1));
end;

end.
