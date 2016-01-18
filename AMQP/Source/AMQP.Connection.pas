unit AMQP.Connection;

interface

Uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults, System.SyncObjs, IdTcpClient, VCL.ExtCtrls,
  AMQP.Classes, AMQP.Frame, AMQP.Header, AMQP.Method, AMQP.Protocol, AMQP.Message, AMQP.Interfaces;

Type
  TSendRecv = ( srSend, srReceive );

  TWireEvent  = Procedure( Sender: TObject; SendRecv: TSendRecv; Strings: TStrings ) of object;
  TDebugEvent = Procedure( Sender: TObject; SendRecv: TSendRecv; Strings: TStrings ) of object;
  TDumpMethod = Reference to procedure( ASendRecv: TSendRecv; AMethod: TAMQPMethod );
  TDumpHeader = Reference to procedure( ASendRecv: TSendRecv; AHeader: TAMQPHeader );
  TDumpFrame  = Reference to procedure( ASendRecv: TSendRecv; AStream: TStream );

  TAMQPThread = Class( TThread )
  Strict Private
    FConnection  : IAMQPConnection;
    FTCP         : TIdTcpClient;
    FMainQueue   : TAMQPQueue;
    FChannelList : TThreadList<IAMQPChannel>;
    FDumpMethod  : TDumpMethod;
    FDumpHeader  : TDumpHeader;
    FDumpFrame   : TDumpFrame;
    Function FindChannel( AList: TList<IAMQPChannel>; AChannelID: Word ): IAMQPChannel;
    Procedure SendFrameToChannel( AFrame: TAMQPFrame );
    Procedure SendFrameToMainChannel( AFrame: TAMQPFrame );
    Procedure SignalCloseToChannels;
    Procedure Disconnect( E: Exception );
    Procedure ServerDisconnect( Msg: String );
    function ReadFrame: TAMQPFrame;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create( AConnection: IAMQPConnection; ATCP: TIdTcpClient; AMainQueue: TAMQPQueue; AChannelList: TThreadList<IAMQPChannel>;
                        ADumpMethod: TDumpMethod; ADumpHeader: TDumpHeader; ADumpFrame: TDumpFrame );
  End;

  TAMQPConnection = Class(TSingletonImplementation, IAMQPConnection)
  Strict Private
    FTCP              : TIdTcpClient;
    FChannels         : TThreadList<IAMQPChannel>;
    FServerProperties : TAMQPServerProperties;
    FMainQueue        : TAMQPQueue;
    FThread           : TAMQPThread;
    FSendLock         : TCriticalSection;
    FDebugLock        : TCriticalSection;
    FPassword         : String;
    FVirtualHost      : String;
    FUsername         : String;
    FHeartbeatSecs    : Word;
    FMaxFrameSize     : Cardinal;
    FLastHeartbeat    : TDateTime;
    FOnWireDebug      : TWireEvent;
    FOnDebug          : TDebugEvent;
    FIsOpen           : Boolean;
    FHeartbeatTimer   : TTimer;
    // get / set methods
    function GetHost: String;
    function GetPort: Word;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Word);
    //Heartbeat handling
    procedure HeartbeatTimer( Sender: TObject );
    Procedure HeartbeatReceived;
    //Helpers
    function ThreadRunning: Boolean;
    Function ChannelNeedsToBeClosedOnServer( AChannel: IAMQPChannel ): Boolean;
    function MakeChannel: IAMQPChannel;
    Procedure ProtocolError( AErrorMessage: String );
    procedure CloseAllChannels;
    procedure CloseConnection;
    Procedure CloseChannelOnServer( AChannel: IAMQPChannel );
    procedure ServerDisconnect( AMessage: String );
    //Read / write methods
    function ReadFrame: IAMQPFrame;
    function ReadMethod( AExpected: Array of TAMQPMethodID ): IAMQPFrame;
    Procedure WriteFrame( AFrameType: Byte; AChannel: Word; APayload: TStream );
    procedure WriteHeartbeat;
    procedure WriteMethod( AChannel: Word; AMethod: TAMQPMethod );
    procedure WriteContent( AChannel, AClassID: Word; AContent: TStream );
    //Debug
    procedure DumpMethod( ASendRecv: TSendRecv; AMethod: TAMQPMethod );
    procedure DumpHeader( ASendRecv: TSendRecv; AHeader: TAMQPHeader );
    procedure DumpFrame(  ASendRecv: TSendRecv; AStream: TStream );
  Public
    Property Username         : String      read FUsername      write FUsername;
    Property Password         : String      read FPassword      write FPassword;
    Property VirtualHost      : String      read FVirtualHost   write FVirtualHost;
    Property Host             : String      read GetHost        write SetHost;
    Property Port             : Word        read GetPort        write SetPort;
    Property HeartbeatSecs    : Word        read FHeartbeatSecs write FHeartbeatSecs;
    Property MaxFrameSize     : Cardinal    read FMaxFrameSize  write FMaxFrameSize;
    Property OnWireDebug      : TWireEvent  read FOnWireDebug   write FOnWireDebug;
    Property OnDebug          : TDebugEvent read FOnDebug       write FOnDebug;
    Property LastHeartbeat    : TDateTime   read FLastHeartbeat;
    Property ServerProperties : TAMQPServerProperties read FServerProperties;
    Function IsOpen: Boolean;

    Procedure Connect;
    Procedure Disconnect;

    Function OpenChannel: IAMQPChannel;
    Procedure CloseChannel( AChannel: IAMQPChannel );

    Constructor Create;
    Destructor Destroy; Override;
  End;

implementation

Uses
  IdGlobal, IdStack,
  AMQP.MessageProperties, AMQP.Payload, AMQP.Helper, AMQP.StreamHelper, AMQP.Types, AMQP.Channel;

{ TAMQPConnection }

Function TAMQPConnection.ChannelNeedsToBeClosedOnServer(AChannel: IAMQPChannel): Boolean;
var
  Channels: TList<IAMQPChannel>;
Begin
  Channels := FChannels.LockList;
  Try
    Result := (Channels.IndexOf( AChannel ) >= 0) and
              IsOpen and
              FTCP.Connected;
  Finally
    FChannels.UnlockList;
  End;
End;

procedure TAMQPConnection.CloseChannel(AChannel: IAMQPChannel);
begin
  CloseChannelOnServer( AChannel );
  AChannel.ChannelClosed;
  FChannels.Remove( AChannel );
end;

procedure TAMQPConnection.CloseChannelOnServer(AChannel: IAMQPChannel);
var
  Frame  : IAMQPFrame;
  Method : TAMQPMethod;
begin
  if (AChannel.State = cOpen) and ChannelNeedsToBeClosedOnServer( AChannel ) then
  Begin
    Method := TAMQPMethod.CreateMethod( AMQP_CHANNEL_CLOSE );
    Try
      WriteMethod( AChannel.ID, Method );
      Frame := AChannel.Queue.Get;
      Try
        Frame.Payload.AsMethod.CheckMethod( AMQP_CHANNEL_CLOSE_OK );
      Except
        on E: Exception do
          ProtocolError( 'Unexpected method' );
      End;
    Finally
      Method.Free;
    End;
  End;
end;

procedure TAMQPConnection.Connect;
var
  Frame: IAMQPFrame;
  Method: TAMQPMethod;
begin
  if IsOpen then
    raise AMQPException.Create('Already open');

  //Reset internal state
  //FThread.Free;
  //FThread := nil;
  FServerProperties.Free;
  FServerProperties := TAMQPServerProperties.Create;

  FTCP.Connect;
  FTCP.IOHandler.Write( AMQP_Header );

  FThread.Free;
  FThread := TAMQPThread.Create( Self, FTCP, FMainQueue, FChannels, DumpMethod, DumpHeader, DumpFrame );

  Frame := ReadMethod( AMQP_CONNECTION_START );
  FServerProperties.ReadConnectionStart( Frame.Payload.AsMethod );

  Method := TAMQPMethod.CreateMethod( AMQP_CONNECTION_START_OK );
  Try
    Method.Field[ 'response' ].AsFieldTable.Field[ 'LOGIN'    ].AsLongString.Value := FUsername;
    Method.Field[ 'response' ].AsFieldTable.Field[ 'PASSWORD' ].AsLongString.Value := FPassword;
    WriteMethod( 0, Method );
  Finally
    Method.Free;
  End;

  Frame := ReadMethod( AMQP_CONNECTION_TUNE );
  FServerProperties.ReadConnectionTune( Frame.Payload.AsMethod );

  Method := TAMQPMethod.CreateMethod( AMQP_CONNECTION_TUNE_OK );
  Try
    //Method.Field[ 'channel-max' ].AsShortUInt.Value := FChannelMax;
    Method.Field[ 'frame-max' ].AsLongUInt.Value    := FMaxFrameSize;
    Method.Field[ 'heartbeat' ].AsShortUInt.Value   := FHeartbeatSecs;
    WriteMethod( 0, Method );
  Finally
    Method.Free;
  End;

  Method := TAMQPMethod.CreateMethod( AMQP_CONNECTION_OPEN );
  Try
    Method.Field[ 'virtual-host' ].AsShortString.Value := FVirtualHost;
    WriteMethod( 0, Method );
  Finally
    Method.Free;
  End;

  Frame := ReadMethod( AMQP_CONNECTION_OPEN_OK );
  FServerProperties.ReadConnectionOpenOK( Frame.Payload.AsMethod );
  FIsOpen := True;
  FHeartbeatTimer.Interval := FHeartbeatSecs * 1000;
  FHeartbeatTimer.Enabled  := True;
//  FThread := TAMQPThread.Create( Self, FTCP, FMainQueue, FChannels );
end;

constructor TAMQPConnection.Create;
begin
  FTCP := TIdTCPClient.Create( nil );
  FTCP.Host         := 'localhost';
  FTCP.Port         := 5672;
  FUsername         := '';
  FPassword         := '';
  FVirtualHost      := '/';
  FChannels         := TThreadList<IAMQPChannel>.Create;
  FLastHeartbeat    := 0;
  FOnWireDebug      := nil;
  FOnDebug          := nil;
  FServerProperties := TAMQPServerProperties.Create;
  FMainQueue        := TAMQPQueue.Create;
  FSendLock         := TCriticalSection.Create;
  FDebugLock        := TCriticalSection.Create;
  FThread           := nil;
  FIsOpen           := False;
  FHeartbeatSecs    := 180;
  FMaxFrameSize     := 131072;
  FHeartbeatTimer   := TTimer.Create( nil );
  FHeartbeatTimer.Enabled  := False;
  FHeartbeatTimer.Interval := 60000;
  FHeartbeatTimer.OnTimer  := HeartbeatTimer;
end;

destructor TAMQPConnection.Destroy;
begin
  Try
    if FTCP.Connected then
      Disconnect;
  Finally
    FThread.Free;
    FServerProperties.Free;
    FChannels.Free;
    FMainQueue.Free;
    FTCP.Free;
    FSendLock.Free;
    FDebugLock.Free;
    FHeartbeatTimer.Free;
    inherited;
  End;
end;

procedure TAMQPConnection.CloseAllChannels;

  Function GetFirstChannel: IAMQPChannel;
  var
    Channels: TList<IAMQPChannel>;
  Begin
    Channels := FChannels.LockList;
    Try
      if Channels.Count = 0 then
        Result := nil
      else
        Result := Channels[0];
    Finally
      FChannels.UnlockList;
    End;
  End;

var
  Channel: IAMQPChannel;
Begin
  Channel := GetFirstChannel;
  while Channel <> nil do
  Begin
    CloseChannel( Channel );
    Channel := GetFirstChannel;
  End;
End;

procedure TAMQPConnection.CloseConnection;
var
  Frame: IAMQPFrame;
  Method: TAMQPMethod;
begin
  if FTCP.Connected then
  Begin
    Method := TAMQPMethod.CreateMethod( AMQP_CONNECTION_CLOSE );
    Try
      WriteMethod( 0, Method );
      Frame := ReadMethod( AMQP_CONNECTION_CLOSE_OK );
    Finally
      FIsOpen := False;
      Method.Free;
    End;
  End;
end;

procedure TAMQPConnection.Disconnect;
begin
  if not IsOpen then
    raise AMQPException.Create('Already closed');
  FHeartbeatTimer.Enabled := False;
  Try
    CloseAllChannels;
    CloseConnection;
  Finally
    FThread.Free;
    FThread := nil;
    Try
      FTCP.Disconnect;
    Except
      On E: Exception do;
    End;
  End;
end;

procedure TAMQPConnection.DumpFrame(ASendRecv: TSendRecv; AStream: TStream);
var
  i: Integer;
  Bytes: TIdBytes;
  Strings: TStringList;
begin
  If Assigned(FOnWireDebug) then
  Begin
    Bytes := AStream.AsBytes;
    Strings := TStringList.Create;
    FDebugLock.Acquire;
    Try
      case Bytes[0] of
        1: Strings.Add( 'Method:' );
        2: Strings.Add( 'Header:' );
        3: Strings.Add( 'Body:' );
        8: Strings.Add( 'Heartbeat:' );
      end;
      for i := 0 to Length(Bytes)-1 do
        Strings.Add( Format( '[%4d] %3d x%s "%s"', [ i, Bytes[i], IntToHex(Bytes[i],2), Chr(Bytes[i]) ] ) );
      FOnWireDebug( self, ASendRecv, Strings );
    Finally
      FDebugLock.Release;
      Strings.Free;
    End;
  End;
end;

procedure TAMQPConnection.DumpHeader(ASendRecv: TSendRecv; AHeader: TAMQPHeader);
var
  Strings: TStringList;
begin
  If Assigned(FOnDebug) then
  Begin
    Strings := TStringList.Create;
    FDebugLock.Acquire;
    Try
      Strings.Add( AHeader.Name );
      Strings.Add( 'ClassID:       ' + IntToStr( AHeader.ClassID ) );
      Strings.Add( 'Weight:        ' + IntToStr( AHeader.Weight ) );
      Strings.Add( 'BodySize:      ' + IntToStr( AHeader.BodySize ) );
      Strings.Add( 'PropertyList:  {' );
      Strings.Add( '        ContentType:        ' + AHeader.PropertyList.ContentType.Value );
      Strings.Add( '        ContentEncoding:    ' + AHeader.PropertyList.ContentEncoding.Value );
      Strings.Add( '        ApplicationHeaders: ' + AHeader.PropertyList.ApplicationHeaders.AsString( '            ' ) );
      Strings.Add( '        DeliveryMode:       ' + AHeader.PropertyList.DeliveryMode.Value.ToString );
      Strings.Add( '        Priority:           ' + AHeader.PropertyList.Priority.Value.ToString );
      Strings.Add( '        CorrelationID:      ' + AHeader.PropertyList.CorrelationID.Value );
      Strings.Add( '        ReplyTo:            ' + AHeader.PropertyList.ReplyTo.Value );
      Strings.Add( '        Expiration:         ' + AHeader.PropertyList.Expiration.Value );
      Strings.Add( '        MessageID:          ' + AHeader.PropertyList.MessageID.Value );
      Strings.Add( '        Timestamp:          ' + IntToStr( AHeader.PropertyList.Timestamp.Value ) );
      Strings.Add( '        Type:               ' + AHeader.PropertyList.&Type.Value );
      Strings.Add( '        UserID:             ' + AHeader.PropertyList.UserID.Value );
      Strings.Add( '        AppID:              ' + AHeader.PropertyList.AppID.Value );
      Strings.Add( '        Reserved:           ' + AHeader.PropertyList.Reserved.Value );
      Strings.Add( '    }' );
      FOnDebug( Self, ASendRecv, Strings );
    Finally
      FDebugLock.Release;
      Strings.Free;
    End;
  End;
end;

procedure TAMQPConnection.DumpMethod(ASendRecv: TSendRecv; AMethod: TAMQPMethod);
var
  Text: TStringList;
begin
  If Assigned(FOnDebug) then
  Begin
    Text := TStringList.Create;
    AMethod.Display( Text );
    FDebugLock.Acquire;
    Try
      FOnDebug( Self, ASendRecv, Text );
    Finally
      FDebugLock.Release;
      Text.Free;
    End;
  End;
end;

function TAMQPConnection.GetHost: String;
begin
  Result := FTCP.Host;
end;

function TAMQPConnection.GetPort: Word;
begin
  Result := FTCP.Port;
end;

procedure TAMQPConnection.HeartbeatReceived;
begin
  FLastHeartbeat := Now;
end;

procedure TAMQPConnection.HeartbeatTimer(Sender: TObject);
begin
  if IsOpen then
    WriteHeartbeat
  else
    FHeartbeatTimer.Enabled := False;
end;

function TAMQPConnection.IsOpen: Boolean;
begin
  Result := FIsOpen;
end;

function TAMQPConnection.MakeChannel: IAMQPChannel;

  Function ChannelIdInUse( AChannelList: TList<IAMQPChannel>; AChannelID: Word ): Boolean;
  var
    Channel: IAMQPChannel;
  Begin
    for Channel in AChannelList do
      if Channel.ID = AChannelID then
        Exit( True );
    Result := False;
  End;

  Function GetNewChannelID( AChannelList: TList<IAMQPChannel> ): Word;
  Begin
    if AChannelList.Count = 65535 then
      raise AMQPException.Create('All channels in use');
    Result := 1;
    While ChannelIdInUse( AChannelList, Result ) do
      Inc( Result );
  End;

var
  Channels: TList<IAMQPChannel>;
Begin
  Channels := FChannels.LockList;
  Try
    Result := TAMQPChannel.Create( Self, GetNewChannelID( Channels ) );
    Channels.Add( Result );
  Finally
    FChannels.UnlockList;
  End;
End;

function TAMQPConnection.OpenChannel: IAMQPChannel;
var
  Frame: IAMQPFrame;
  Method: TAMQPMethod;
begin
  Result := MakeChannel;
  Method := TAMQPMethod.CreateMethod( AMQP_CHANNEL_OPEN );
  Try
    WriteMethod( Result.ID, Method );
    Frame := Result.Queue.Get;
    if (Frame.Payload.Name <> 'channel.open-ok') then
      ProtocolError( 'Expected channel.open-ok' );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPConnection.ProtocolError(AErrorMessage: String);
begin
  Disconnect;
  raise AMQPException.Create( AErrorMessage );
end;

function TAMQPConnection.ReadFrame: IAMQPFrame;
begin
  Result := nil;
  if ThreadRunning or (FMainQueue.Count > 0) then
    Result := FMainQueue.Get;
end;

function TAMQPConnection.ReadMethod(AExpected: array of TAMQPMethodID): IAMQPFrame;
var
  MethodIsExpected: Boolean;
  Method: TAMQPMethodID;
begin
  Repeat
    Result := ReadFrame;
  Until (Result = nil) or (Result.Kind <> fkHeartbeat);

  if not Assigned(Result) then
    raise AMQPException.Create('Disconnected');

  if not (Result.Payload is TAMQPMethod) then
    raise AMQPException.Create('Frame does not contain a method');

  MethodIsExpected := False;
  for Method in AExpected do
    if Result.Payload.AsMethod.IsMethod( Method ) then
      MethodIsExpected := True;

  if not MethodIsExpected then
    raise AMQPException.CreateFmt( 'Unexpected class/method: %d.%d',
                                   [ Result.Payload.AsMethod.ClassID.Value, Result.Payload.AsMethod.MethodID.Value ] );
end;

procedure TAMQPConnection.ServerDisconnect(AMessage: String);
begin
  FIsOpen := False;
  FHeartbeatTimer.Enabled := False;
end;

procedure TAMQPConnection.SetHost(const Value: String);
begin
  FTCP.Host := Value;
end;

procedure TAMQPConnection.SetPort(const Value: Word);
begin
  FTCP.Port := Value;
end;

function TAMQPConnection.ThreadRunning: Boolean;
begin
  Result := Assigned(FThread) and not FThread.Terminated;
end;

procedure TAMQPConnection.WriteContent(AChannel, AClassID: Word; AContent: TStream);
Const
  WEIGHT : Byte = $00;
Var
  Header     : TMemoryStream;
  Properties : TAMQPMessageProperties;
begin
  //Header frame
  Header := TMemoryStream.Create;
  Properties  := TAMQPMessageProperties.Create;
  Try
    Header.WriteUInt16( AClassID );
    Header.WriteUInt16( WEIGHT );
    Header.WriteUInt64( AContent.Size );
    Properties.SaveToStream( Header );
    WriteFrame( FRAME_TYPE_HEADER, AChannel, Header );
  Finally
    Properties.Free;
    Header.Free;
  End;

  //Content / body
  WriteFrame( FRAME_TYPE_CONTENT, AChannel, AContent );
  //TODO: Split into smaller packets according to settings from 'connection.tune'
end;

procedure TAMQPConnection.WriteFrame(AFrameType: Byte; AChannel: Word; APayload: TStream);
Var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Try
    Stream.WriteOctet(  AFrameType );
    Stream.WriteUInt16( AChannel );
    Stream.WriteUInt32( APayload.Size );
    Stream.CopyFrom( APayload, -1 );
    Stream.WriteOctet( FRAME_END );
    DumpFrame( srSend, Stream );
    FSendLock.Enter;
    Try
      FTCP.IOHandler.Write( Stream, 0, False );
    Finally
      FSendLock.Leave;
    End;
  Finally
    Stream.Free;
  End;
end;

procedure TAMQPConnection.WriteHeartbeat;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Try
    WriteFrame( FRAME_TYPE_HEARTBEAT, 0, Stream );
  Finally
    Stream.Free;
  End;
end;

procedure TAMQPConnection.WriteMethod(AChannel: Word; AMethod: TAMQPMethod);
Var
  Payload: TMemoryStream;
begin
  Payload := TMemoryStream.Create;
  Try
    AMethod.SaveToStream( Payload );
    DumpMethod( srSend, AMethod );
    WriteFrame( FRAME_TYPE_METHOD, AChannel, Payload );
  Finally
    Payload.Free;
  End;
end;

{ TAMQPThread }

procedure TAMQPThread.SignalCloseToChannels;
var
  Channel : IAMQPChannel;
  Frame   : TAMQPFrame;
  List    : TList<IAMQPChannel>;
begin
  List := FChannelList.LockList;
  Try
    For Channel in List do
    Begin
      Frame := TAMQPFrame.Create;
      Frame.SetKind( fkMethod );
      Frame.SetChannel( Channel.ID );
      Frame.SetPayload( TAMQPMethod.CreateMethod( AMQP_CONNECTION_CLOSE_OK ) );
      Frame.SetFrameEnd( $CE );
      Channel.Queue.Put( Frame );
      Channel.ChannelClosed;
    End;
  Finally
    FChannelList.UnlockList;
  End;
end;

constructor TAMQPThread.Create( AConnection: IAMQPConnection; ATCP: TIdTcpClient; AMainQueue: TAMQPQueue;
                                AChannelList: TThreadList<IAMQPChannel>; ADumpMethod: TDumpMethod; ADumpHeader: TDumpHeader;
                                ADumpFrame: TDumpFrame);
begin
  FConnection  := AConnection;
  FTCP         := ATCP;
  FMainQueue   := AMainQueue;
  FChannelList := AChannelList;
  FDumpMethod  := ADumpMethod;
  FDumpHeader  := ADumpHeader;
  FDumpFrame   := ADumpFrame;
  inherited Create;
end;

procedure TAMQPThread.Disconnect(E: Exception);
begin
  FConnection.Disconnect;
end;

procedure TAMQPThread.Execute;
var
  Frame: TAMQPFrame;
begin
  NameThreadForDebugging( 'AMQP' );
  Try
    Repeat
      Frame := ReadFrame;
      if Frame.Channel = 0 then
        SendFrameToMainChannel( Frame )
      else
        SendFrameToChannel( Frame );
    Until Terminated or (not FTCP.Connected);
  Except
    On E: EIdSocketError do
      ServerDisconnect( E.Message );
    On E: Exception do
      Disconnect( E );
  End;
end;

function TAMQPThread.FindChannel(AList: TList<IAMQPChannel>; AChannelID: Word): IAMQPChannel;
var
  Channel: IAMQPChannel;
begin
  for Channel in AList do
    if Channel.ID = AChannelID then
      Exit( Channel );
  Result := nil;
end;

function TAMQPThread.ReadFrame: TAMQPFrame;

//  Function ReadBytesMSB( var Buffer; Const Bytes: TIdBytes; Index, Count: Integer ): Integer;
//  var
//    Cnt: Integer;
//  begin
//    For Cnt := 0 to Count-1 do
//      PByteArray(@Buffer)[Count - Cnt - 1] := Bytes[Index + Cnt];
//    Result := Index + Count;
//  End;

const
  FRAME_HEADER_SIZE  = 7;
  METHOD_HEADER_SIZE = 4;
var
  Bytes   : TIdBytes;
  Payload : TIdBytes;
  Frame   : TAMQPFrame;
  Stream  : TMemoryStream;
begin
  Result := nil;
  Frame  := TAMQPFrame.Create;
  Stream := TMemoryStream.Create;
  Try
    FTCP.IOHandler.ReadBytes( Bytes, FRAME_HEADER_SIZE, False );
    Frame.SetKind( TFrameKind( Bytes[0] ) );
    Frame.SetChannel( Bytes[1] shl  8 + Bytes[2] );
    Frame.SetSize( Bytes[3] shl 24 + Bytes[4] shl 16 + Bytes[5] shl 8 + Bytes[6] );
    Stream.Write( @Bytes[0], Length( Bytes ) );

    FTCP.IOHandler.ReadBytes( Payload, Frame.Size + 1, False );
    Frame.SetFrameEnd( Payload[ Frame.Size ] );
    Stream.Write( @Payload[0], Length( Payload )-1 );

    FDumpFrame( srReceive, Stream );

    If Frame.FrameEnd <> $CE then
      raise AMQPException.Create('FrameEnd incorrect');

    Stream.Position := FRAME_HEADER_SIZE;
    case Frame.Kind of
      fkMethod:
        Begin
          Frame.SetPayload( TAMQPMethod.Create );
          Frame.Payload.LoadFromStream( Stream );
          FDumpMethod( srReceive, Frame.Payload.AsMethod );
        End;
      fkHeader:
        Begin
          Frame.SetPayload( TAMQPHeader.Create );
          Frame.Payload.LoadFromStream( Stream );
          FDumpHeader( srReceive, Frame.Payload.AsHeader );
        End;
      fkBody:
        Begin
          Frame.SetPayload( TAMQPBody.Create );
          Frame.Payload.LoadFromStream( Stream );
        End;
      fkHeartbeat: ;
      else
        raise AMQPException.Create('Not a method');
    end;
    Result := Frame;
    Frame := nil;
  Finally
    Frame.Free;
    Stream.Free;
  End;
end;

procedure TAMQPThread.SendFrameToChannel(AFrame: TAMQPFrame);
var
  Channel : IAMQPChannel;
  List    : TList<IAMQPChannel>;
begin
  List := FChannelList.LockList;
  Try
    Channel := FindChannel( List, AFrame.Channel );
  Finally
    FChannelList.UnlockList;
  End;
  if Channel = nil then
    raise AMQPException.CreateFmt( 'Invalid channel: Received frame for channel %d', [AFrame.Channel] );
  Channel.ReceiveFrame( AFrame );
end;

procedure TAMQPThread.SendFrameToMainChannel(AFrame: TAMQPFrame);
Begin
  Case AFrame.Kind of
    fkMethod:
      Begin
        If AFrame.Payload.AsMethod.IsMethod( AMQP_CONNECTION_CLOSE_OK ) then
        Begin
          Terminate;
          FMainQueue.Put( AFrame );
        End
        else If AFrame.Payload.AsMethod.IsMethod( AMQP_CONNECTION_CLOSE ) then
        Begin
          Terminate;
          Try
            ServerDisconnect( AFrame.Payload.AsMethod.Field[ 'reply-text' ].AsShortString.Value );
          Finally
            AFrame.Free;
          End;
          //raise AMQPException.Create('Server closed connection');
        End
        Else
          FMainQueue.Put( AFrame );
      End;
    fkHeader,
    fkBody:
      FMainQueue.Put( AFrame );
    fkHeartbeat:
      Begin
        FConnection.HeartbeatReceived;
        AFrame.Free;
      End;
  End;
End;

procedure TAMQPThread.ServerDisconnect(Msg: String);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_CONNECTION_CLOSE_OK );
  Try
    Try
      //TODO: Send close-ok
      //WriteMethod( Method );
      FConnection.ServerDisconnect( Msg );
    Finally
      FTCP.Disconnect;
    End;
  Finally
    SignalCloseToChannels;
    Method.Free;
  End;
end;

end.
