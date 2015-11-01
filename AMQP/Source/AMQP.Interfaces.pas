unit AMQP.Interfaces;

interface

Uses
  System.SysUtils, System.Classes, AMQP.Classes, AMQP.Method, AMQP.Message, AMQP.Frame;

Type
  TAMQPChannelState = ( cOpen, cClosed );

  TConsumerMethod = Reference to Procedure( AMQPMessage: TAMQPMessage; var SendAck: Boolean );

  IAMQPChannel = interface(IAMQPBaseChannel) ['{6620C29F-0354-4C66-A3AC-4D5F7BB7113A}']
    Function GetID    : Integer;
    Function GetQueue : TAMQPQueue;
    Function GetState : TAMQPChannelState;

    Property ID    : Integer           read GetID;
    Property Queue : TAMQPQueue        read GetQueue;
    Property State : TAMQPChannelState read GetState;

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
    //Procedure BasicAck( ADeliveryTag: UInt64; AMultiple: Boolean = False ); Overload;
    Procedure BasicConsume( AMessageHandler: TConsumerMethod; AQueueName, AConsumerTag: String; ANoLocal: Boolean = False;
                            ANoAck: Boolean = False; AExclusive: Boolean = False; ANoWait: Boolean = False ); Overload;
    Procedure BasicConsume( AMessageQueue: TAMQPMessageQueue; AQueueName, AConsumerTag: String; ANoLocal: Boolean = False;
                            ANoAck: Boolean = False; AExclusive: Boolean = False; ANoWait: Boolean = False ); Overload;
    Procedure BasicCancel( AConsumerTag: String; ANoWait: Boolean = False );
    Procedure BasicReject( AMessage: TAMQPMessage; ARequeue: Boolean = True ); overload;
    //Procedure BasicReject( ADeliveryTag: UInt64; ARequeue: Boolean = True ); overload;

    Procedure ConfirmSelect( ANoWait: Boolean = False );
    procedure ReceiveFrame( AFrame: TAMQPFrame );

    Procedure Close;
    Procedure ChannelClosed;
  end;

  IAMQPConnection = interface ['{4736645E-A4E1-4E3B-B1D5-BC218A6C6CCC}']
    Function IsOpen: Boolean;
    Procedure WriteFrame( AFrameType: Byte; AChannel: Word; APayload: TStream );
    procedure WriteMethod( AChannel: Word; AMethod: TAMQPMethod );
    procedure WriteContent( AChannel, AClassID: Word; AContent: TStream );
    Procedure HeartbeatReceived;
    Procedure Disconnect;
    Procedure ServerDisconnect( Msg: String );
    Procedure CloseChannel( AChannel: IAMQPChannel );
  end;

implementation

end.
