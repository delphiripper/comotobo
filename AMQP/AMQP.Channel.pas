unit AMQP.Channel;

interface

Uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  AMQP.Method, AMQP.Frame, AMQP.Message, AMQP.Protocol, AMQP.Interfaces, AMQP.Classes;

Type
  TConsumer = Class
  Strict Private
    FQueueName      : String;
    FConsumerTag    : String;
    FMessageHandler : TConsumerMethod;
    FMessageQueue   : TAMQPMessageQueue;
  Public
    Property QueueName      : String            read FQueueName;
    Property ConsumerTag    : String            read FConsumerTag;
    Property MessageHandler : TConsumerMethod   read FMessageHandler;
    Property MessageQueue   : TAMQPMessageQueue read FMessageQueue;
    Procedure Receive( AMessage: TAMQPMessage );
    Constructor Create( AQueueName, AConsumerTag: String; AMessageHandler: TConsumerMethod; AMessageQueue: TAMQPMessageQueue );
    Destructor Destroy; Override;
  End;

  TAMQPChannel = class(TInterfacedObject, IAMQPBaseChannel, IAMQPChannel)
  Strict private
    FConnection    : IAMQPConnection;
    FID            : Integer;
    FConfirmSelect : Boolean;
    FQueue         : TAMQPQueue;
    FConsumers     : TObjectList<TConsumer>;
    FDeliverConsumer : TConsumer;
    FDeliverQueue    : TObjectList<TAMQPFrame>;
    procedure AddConsumer( AQueueName, AConsumerTag: String; AMessageHandler: TConsumerMethod; AMessageQueue: TAMQPMessageQueue );
    procedure RemoveConsumer( AConsumerTag: String );
    Function FindConsumer( AConsumerTag: String ): TConsumer;
    procedure WriteMethod( AMethod: TAMQPMethod );
    procedure WriteContent( AClassID: Word; AContent: TStream );
    function ReadMethod( AExpected: Array of TAMQPMethodID ): IAMQPFrame;
    procedure CheckOpen;
    procedure ChannelClosed;
    procedure Deliver( ADeliver: TAMQPFrame );
    procedure CheckDeliveryComplete;
    Function HasCompleteMessageInQueue( AQueue: TObjectList<TAMQPFrame> ): Boolean;
    Function GetMessageFromQueue( AQueue: TObjectList<TAMQPFrame> ): TAMQPMessage;
    procedure ReceiveFrame( AFrame: TAMQPFrame );
  Public
    Function GetID        : Integer;
    Function GetQueue     : TAMQPQueue;
    Function GetState     : TAMQPChannelState;
    Function GetConsumers : TObjectList<TConsumer>;

    Procedure ExchangeDeclare( AExchangeName, AType: String; APassive: Boolean = False; ADurable: Boolean = True; ANoWait: Boolean = False );
    Procedure ExchangeDelete( AExchangeName: String; AIfUnused: Boolean = True; ANoWait: Boolean = False );
    Procedure QueueDeclare( AQueueName: String; APassive: Boolean = False; ADurable: Boolean = True; AExclusive: Boolean = False;
                            AAutoDelete: Boolean = False; ANoWait: Boolean = False );
    Procedure QueueBind( AQueueName, AExchangeName, ARoutingKey: String; ANoWait: Boolean = False );
    Procedure QueuePurge( AQueueName: String; ANoWait: Boolean = False );
    Procedure QueueDelete( AQueueName: String; AIfUnused: Boolean = True; AIfEmpty: Boolean = True; ANoWait: Boolean = False );
    Procedure QueueUnBind( AQueueName, AExchangeName, ARoutingKey: String );

    Procedure BasicPublish( AExchange, ARoutingKey: String; AData: TStream ); Overload;
    Procedure BasicPublish( AExchange, ARoutingKey: String; Const AData: String; AEncoding: TEncoding ); Overload;
    Function  BasicGet( AQueueName: String; ANoAck: Boolean = False ): TAMQPMessage;
    Procedure BasicAck( AMessage: TAMQPMessage; AMultiple: Boolean = False ); Overload;
    Procedure BasicAck( ADeliveryTag: UInt64; AMultiple: Boolean = False ); Overload;
    Procedure BasicConsume( AMessageHandler: TConsumerMethod; AQueueName, AConsumerTag: String; ANoLocal: Boolean = False;
                            ANoAck: Boolean = False; AExclusive: Boolean = False; ANoWait: Boolean = False ); Overload;
    Procedure BasicConsume( AMessageQueue: TAMQPMessageQueue; AQueueName, AConsumerTag: String; ANoLocal: Boolean = False;
                            ANoAck: Boolean = False; AExclusive: Boolean = False; ANoWait: Boolean = False ); Overload;
    Procedure BasicCancel( AConsumerTag: String; ANoWait: Boolean = False );
    Procedure BasicReject( AMessage: TAMQPMessage; ARequeue: Boolean = True ); overload;
    Procedure BasicReject( ADeliveryTag: UInt64; ARequeue: Boolean = True ); overload;

    Procedure ConfirmSelect( ANoWait: Boolean = False );
    Procedure Close;

    Constructor Create( AConnection: IAMQPConnection; AChannelID: Word );
    Destructor Destroy; Override;
  end;

implementation

Uses
  AMQP.Helper;

{ TAMQPChannel }

procedure TAMQPChannel.ChannelClosed;
begin
  FConnection := nil;
  FConsumers.Clear;
end;

procedure TAMQPChannel.CheckDeliveryComplete;
begin
  if HasCompleteMessageInQueue( FDeliverQueue ) then
  Try
    FDeliverConsumer.Receive( GetMessageFromQueue( FDeliverQueue ) );
  Finally
    FDeliverConsumer := nil;
  End;
end;

procedure TAMQPChannel.CheckOpen;
begin
  if FConnection = nil then
    raise AMQPException.Create('Channel is not open');
  if not FConnection.IsOpen then
    raise AMQPException.Create('Connection is not open');
end;

procedure TAMQPChannel.Close;
begin
  CheckOpen;
  Try
    FConnection.CloseChannel( Self );
  Finally
    FConnection := nil;
  End;
end;

procedure TAMQPChannel.ConfirmSelect(ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_CONFIRM_SELECT );
  Try
    Method.Field['no-wait'].AsBoolean.Value := ANoWait;
    WriteMethod( Method );
    if not ANoWait then
      ReadMethod( [ AMQP_CONFIRM_SELECT_OK ] );
    FConfirmSelect := True;
  Finally
    Method.Free;
  End;
end;

constructor TAMQPChannel.Create(AConnection: IAMQPConnection; AChannelID: Word);
begin
  FConnection    := AConnection;
  FID            := AChannelID;
  FConfirmSelect := False;
  FQueue         := TAMQPQueue.Create;
  FConsumers     := TObjectList<TConsumer>.Create;
  FDeliverConsumer := nil;
  FDeliverQueue    := TObjectList<TAMQPFrame>.Create;
end;

Function TAMQPChannel.FindConsumer( AConsumerTag: String ): TConsumer;
var
  Consumer: TConsumer;
Begin
  for Consumer in FConsumers do
    if (Consumer.ConsumerTag = AConsumerTag) then
      Exit( Consumer );
  Result := nil;
End;

procedure TAMQPChannel.Deliver(ADeliver: TAMQPFrame);
var
  ConsumerTag: String;
begin
  ConsumerTag := ADeliver.Payload.AsMethod.Field[ 'consumer-tag' ].AsShortString.Value;
  FDeliverConsumer := FindConsumer( ConsumerTag );
  if (FDeliverConsumer = nil) then
    raise AMQPException.Create('No consumer for consumer-tag: ' + ConsumerTag);
  FDeliverQueue.Add( ADeliver );
end;

destructor TAMQPChannel.Destroy;
begin
  if GetState = cOpen then
    Close;
  FConsumers.Free;
  FDeliverQueue.Free;
  FQueue.Free;
  inherited;
end;

procedure TAMQPChannel.ExchangeDeclare(AExchangeName, AType: String; APassive, ADurable, ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_EXCHANGE_DECLARE );
  Try
    Method.Field['exchange'].AsShortString.Value := AExchangeName;
    Method.Field['type'    ].AsShortString.Value := AType;
    Method.Field['passive' ].AsBoolean.Value     := APassive;
    Method.Field['durable' ].AsBoolean.Value     := ADurable;
    Method.Field['no-wait' ].AsBoolean.Value     := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_EXCHANGE_DECLARE_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.ExchangeDelete(AExchangeName: String; AIfUnused, ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_EXCHANGE_DELETE );
  Try
    Method.Field['exchange'].AsShortString.Value := AExchangeName;
    Method.Field['if-unused' ].AsBoolean.Value   := AIfUnused;
    Method.Field['no-wait' ].AsBoolean.Value     := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_EXCHANGE_DELETE_OK );
  Finally
    Method.Free;
  End;
end;

function TAMQPChannel.GetConsumers: TObjectList<TConsumer>;
begin
  Result := FConsumers;
end;

function TAMQPChannel.GetID: Integer;
begin
  Result := FID;
end;

function TAMQPChannel.GetMessageFromQueue(AQueue: TObjectList<TAMQPFrame>): TAMQPMessage;
var
  DeliverFrame : IAMQPFrame;
  HeaderFrame  : IAMQPFrame;
  BodyFrame    : IAMQPFrame;
  Stream       : TMemoryStream;
  BaseChannel  : IAMQPBaseChannel;
begin
  Stream := TMemoryStream.Create;
  DeliverFrame := FDeliverQueue.Extract( FDeliverQueue[0] );
  HeaderFrame  := FDeliverQueue.Extract( FDeliverQueue[0] );
  Try
    Repeat
      BodyFrame := FDeliverQueue.Extract( FDeliverQueue[0] );
      Stream.CopyFrom( BodyFrame.Payload.AsBody.Stream, -1 );
      BodyFrame := nil;
    Until Stream.Size >= HeaderFrame.Payload.AsHeader.BodySize;
    Result := TAMQPMessage.Create;
    Self.GetInterface( IAMQPBaseChannel, BaseChannel );
    Result.Channel := BaseChannel;
    Result.ReadFromData( DeliverFrame, HeaderFrame, Stream ); //DeliverFrame and HeaderFrame used as interface: Will be free'd here
  Finally
    Stream.Free;
    DeliverFrame := nil;
    HeaderFrame  := nil;
  End;
end;

procedure TAMQPChannel.BasicAck(AMessage: TAMQPMessage; AMultiple: Boolean);
begin
  BasicAck( AMessage.DeliveryTag, AMultiple );
end;

procedure TAMQPChannel.AddConsumer( AQueueName, AConsumerTag: String; AMessageHandler: TConsumerMethod; AMessageQueue: TAMQPMessageQueue);
var
  Consumer: TConsumer;
begin
  for Consumer in FConsumers do
    if (Consumer.ConsumerTag = AConsumerTag) then
      raise AMQPException.Create('Duplicate consumer');
  FConsumers.Add( TConsumer.Create( AQueueName, AConsumerTag, AMessageHandler, AMessageQueue ) );
end;

procedure TAMQPChannel.BasicAck(ADeliveryTag: UInt64; AMultiple: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_ACK );
  Try
    Method.Field['delivery-tag'].AsLongLongUInt.Value := ADeliveryTag;
    Method.Field['multiple'    ].AsBoolean.Value      := AMultiple;
    WriteMethod( Method );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.BasicCancel(AConsumerTag: String; ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  RemoveConsumer( AConsumerTag );
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_CANCEL );
  Try
    Method.Field['consumer-tag'].AsShortString.Value := AConsumerTag;
    Method.Field['no-wait'].AsBoolean.Value          := ANoWait;
    WriteMethod( Method );
    if not ANoWait then
      ReadMethod( AMQP_BASIC_CANCEL_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.BasicConsume(AMessageQueue: TAMQPMessageQueue; AQueueName, AConsumerTag: String; ANoLocal, ANoAck, AExclusive,
  ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  AddConsumer( AQueueName, AConsumerTag, nil, AMessageQueue );
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_CONSUME );
  Try
    Method.Field['queue'].AsShortString.Value        := AQueueName;
    Method.Field['consumer-tag'].AsShortString.Value := AConsumerTag;
    Method.Field['no-ack'].AsBoolean.Value           := ANoAck;
    Method.Field['no-local'].AsBoolean.Value         := ANoLocal;
    Method.Field['exclusive'].AsBoolean.Value        := AExclusive;
    Method.Field['no-wait'].AsBoolean.Value          := ANoWait;
    WriteMethod(  Method );
    if not ANoWait then
      ReadMethod( AMQP_BASIC_CONSUME_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.BasicConsume( AMessageHandler: TConsumerMethod; AQueueName, AConsumerTag: String;
                                     ANoLocal, ANoAck, AExclusive, ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  AddConsumer( AQueueName, AConsumerTag, AMessageHandler, nil );
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_CONSUME );
  Try
    Method.Field['queue'].AsShortString.Value        := AQueueName;
    Method.Field['consumer-tag'].AsShortString.Value := AConsumerTag;
    Method.Field['no-ack'].AsBoolean.Value           := ANoAck;
    Method.Field['no-local'].AsBoolean.Value         := ANoLocal;
    Method.Field['exclusive'].AsBoolean.Value        := AExclusive;
    Method.Field['no-wait'].AsBoolean.Value          := ANoWait;
    WriteMethod(  Method );
    if not ANoWait then
      ReadMethod( AMQP_BASIC_CONSUME_OK );
  Finally
    Method.Free;
  End;
end;

function TAMQPChannel.BasicGet(AQueueName: String; ANoAck: Boolean): TAMQPMessage;
var
  GetOkFrame, HeaderFrame, BodyFrame: IAMQPFrame;
  Method: TAMQPMethod;
  Stream: TMemoryStream;
  BaseChannel: IAMQPBaseChannel;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_GET );
  Try
    Method.Field['queue'].AsShortString.Value := AQueueName;
    Method.Field['no-ack'].AsBoolean.Value    := ANoAck;
    WriteMethod(  Method );
    GetOkFrame := ReadMethod( [ AMQP_BASIC_GET_OK, AMQP_BASIC_GET_EMPTY ] );
    if GetOkFrame.Payload.AsMethod.IsMethod( AMQP_BASIC_GET_EMPTY ) then
      Result := nil
    else //if GetOkFrame.Payload.AsMethod.IsMethod( AMQP_BASIC_GET_OK ) then
    Begin
      HeaderFrame := FQueue.Get;
      if HeaderFrame.Kind <> fkHeader then
        raise AMQPException.Create('Expected header frame');
      Repeat
        BodyFrame := FQueue.Get;
        Stream.CopyFrom( BodyFrame.Payload.AsBody.Stream, -1 );
      Until Stream.Size >= HeaderFrame.Payload.AsHeader.BodySize;
      Result := TAMQPMessage.Create;
      Self.GetInterface( IAMQPBaseChannel, BaseChannel );
      Result.Channel := BaseChannel;
      Result.ReadFromData( GetOkFrame, HeaderFrame, Stream );
    End;
  Finally
    GetOkFrame  := nil;
    HeaderFrame := nil;
    BodyFrame   := nil;
    Method.Free;
    Stream.Free;
  End;
end;

procedure TAMQPChannel.BasicPublish(AExchange, ARoutingKey: String; const AData: String; AEncoding: TEncoding);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create( AData, AEncoding );
  Try
    BasicPublish( AExchange, ARoutingKey, StringStream );
  Finally
    StringStream.Free;
  End;
end;

procedure TAMQPChannel.BasicReject(AMessage: TAMQPMessage; ARequeue: Boolean);
begin
  BasicReject( AMessage.DeliveryTag, ARequeue );
end;

procedure TAMQPChannel.BasicReject(ADeliveryTag: UInt64; ARequeue: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_REJECT );
  Try
    Method.Field['delivery-tag'].AsLongLongUInt.Value := ADeliveryTag;
    Method.Field['requeue'     ].AsBoolean.Value      := ARequeue;
    WriteMethod( Method );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.BasicPublish(AExchange, ARoutingKey: String; AData: TStream);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_BASIC_PUBLISH );
  Try
    Method.Field['exchange'].AsShortString.Value    := AExchange;
    Method.Field['routing-key'].AsShortString.Value := ARoutingKey;
    WriteMethod(  Method );
    WriteContent( AMQP_CLASS_BASIC, AData );
    if FConfirmSelect then
      ReadMethod( [ AMQP_BASIC_ACK ] );
  Finally
    Method.Free;
  End;
end;

function TAMQPChannel.GetQueue: TAMQPQueue;
begin
  Result := FQueue;
end;

function TAMQPChannel.GetState: TAMQPChannelState;
begin
  If FConnection <> nil then
    Result := cOpen
  Else
    Result := cClosed;
end;

function TAMQPChannel.HasCompleteMessageInQueue(AQueue: TObjectList<TAMQPFrame>): Boolean;
var
  HeaderFrame: TAMQPFrame;
  Size, Received: UInt64;
  Index: Integer;
begin
  Result := False;
  if AQueue.Count >= 2 then
  Begin
  //DeliverFrame := AQueue[0]; <-- Dont need this frame here
    HeaderFrame  := AQueue[1];
    Size         := HeaderFrame.Payload.AsHeader.BodySize;
    Received     := 0;
    Index := 2;
    While (Index < AQueue.Count) and
          (AQueue[Index].Kind = fkBody) and
          (Received < Size) do
    Begin
      Received := Received + AQueue[Index].Payload.AsBody.Stream.Size;
      Inc( Index );
    End;
    Result := (Received >= Size);
  End;
end;

procedure TAMQPChannel.QueueBind(AQueueName, AExchangeName, ARoutingKey: String; ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_QUEUE_BIND );
  Try
    Method.Field['queue'].AsShortString.Value       := AQueueName;
    Method.Field['exchange'].AsShortString.Value    := AExchangeName;
    Method.Field['routing-key'].AsShortString.Value := ARoutingKey;
    Method.Field['no-wait' ].AsBoolean.Value        := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_QUEUE_BIND_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.QueueDeclare(AQueueName: String; APassive, ADurable, AExclusive, AAutoDelete, ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_QUEUE_DECLARE );
  Try
    Method.Field['queue'].AsShortString.Value    := AQueueName;
    Method.Field['passive' ].AsBoolean.Value     := APassive;
    Method.Field['durable' ].AsBoolean.Value     := ADurable;
    Method.Field['exclusive' ].AsBoolean.Value   := AExclusive;
    Method.Field['auto-delete' ].AsBoolean.Value := AAutoDelete;
    Method.Field['no-wait' ].AsBoolean.Value     := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_QUEUE_DECLARE_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.QueueDelete(AQueueName: String; AIfUnused, AIfEmpty, ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_QUEUE_DELETE );
  Try
    Method.Field['queue'].AsShortString.Value  := AQueueName;
    Method.Field['if-unused' ].AsBoolean.Value := AIfUnused;
    Method.Field['if-empty' ].AsBoolean.Value  := AIfEmpty;
    Method.Field['no-wait' ].AsBoolean.Value   := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_QUEUE_DELETE_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.QueuePurge(AQueueName: String; ANoWait: Boolean);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_QUEUE_PURGE );
  Try
    Method.Field['queue'].AsShortString.Value       := AQueueName;
    Method.Field['no-wait' ].AsBoolean.Value        := ANoWait;
    WriteMethod( Method );

    if not ANoWait then
      ReadMethod( AMQP_QUEUE_PURGE_OK );
  Finally
    Method.Free;
  End;
end;

procedure TAMQPChannel.QueueUnBind(AQueueName, AExchangeName, ARoutingKey: String);
var
  Method: TAMQPMethod;
begin
  Method := TAMQPMethod.CreateMethod( AMQP_QUEUE_UNBIND );
  Try
    Method.Field['queue'].AsShortString.Value       := AQueueName;
    Method.Field['exchange'].AsShortString.Value    := AExchangeName;
    Method.Field['routing-key'].AsShortString.Value := ARoutingKey;
    WriteMethod( Method );
    ReadMethod( AMQP_QUEUE_UNBIND_OK );
  Finally
    Method.Free;
  End;
end;

function TAMQPChannel.ReadMethod(AExpected: array of TAMQPMethodID): IAMQPFrame;
var
  MethodIsExpected: Boolean;
  Method: TAMQPMethodID;
begin
  CheckOpen;
  Repeat
    Result := FQueue.Get;
  Until Result.Kind <> fkHeartbeat;

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

procedure TAMQPChannel.ReceiveFrame(AFrame: TAMQPFrame);
begin
  if (AFrame.Payload is TAMQPMethod) and
     AFrame.Payload.AsMethod.IsMethod( AMQP_BASIC_DELIVER ) then
    Deliver( AFrame )
  Else if Assigned(FDeliverConsumer) then
  Begin
    FDeliverQueue.Add( AFrame );
    CheckDeliveryComplete;
  End
  Else
    FQueue.Put( AFrame );
end;

procedure TAMQPChannel.RemoveConsumer(AConsumerTag: String);
var
  Consumer: TConsumer;
begin
  Consumer := FindConsumer( AConsumerTag );
  if Consumer = nil then
    raise AMQPException.Create('Consumer not found');
  FConsumers.Remove( Consumer );
end;

procedure TAMQPChannel.WriteContent(AClassID: Word; AContent: TStream);
begin
  CheckOpen;
  FConnection.WriteContent( FID, AClassID, AContent );
end;

procedure TAMQPChannel.WriteMethod(AMethod: TAMQPMethod);
begin
  CheckOpen;
  FConnection.WriteMethod( FID, AMethod );
end;

{ TConsumer }

constructor TConsumer.Create(AQueueName, AConsumerTag: String; AMessageHandler: TConsumerMethod; AMessageQueue: TAMQPMessageQueue);
begin
  FQueueName      := AQueueName;
  FConsumerTag    := AConsumerTag;
  FMessageHandler := AMessageHandler;
  FMessageQueue   := AMessageQueue;
end;

destructor TConsumer.Destroy;
begin
  if FMessageQueue <> nil then
    FMessageQueue.Put( nil );
  inherited;
end;

procedure TConsumer.Receive(AMessage: TAMQPMessage);
var
  SendAck: Boolean;
begin
  if FMessageQueue <> nil then
    FMessageQueue.Put( AMessage )
  else
  Begin
    SendAck := True;
    Try
      MessageHandler( AMessage, SendAck );
      if SendAck then
        AMessage.Ack;
    Finally
      AMessage.Free;
    End;
  End;
end;

end.
