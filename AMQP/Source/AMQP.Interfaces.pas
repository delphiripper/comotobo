unit AMQP.Interfaces;

interface

Uses
  System.SysUtils, System.Classes, AMQP.Classes, AMQP.Method, AMQP.Message, AMQP.Frame, AMQP.IMessageProperties, AMQP.Arguments;

Type
  TAMQPChannelState = ( cOpen, cClosed );

  TConsumerMethod = Reference to Procedure( AMQPMessage: TAMQPMessage; var SendAck: Boolean );

  TExchangeType = ( etDirect, etTopic, etFanout, etHeaders );

Const
  ExchangeTypeStr : Array[TExchangeType] of string = ( 'direct', 'topic', 'fanout', 'headers' );

Type
  IAMQPChannel = interface(IAMQPBaseChannel) ['{6620C29F-0354-4C66-A3AC-4D5F7BB7113A}']
    Function GetID    : Integer;
    Function GetQueue : TAMQPQueue;
    Function GetState : TAMQPChannelState;

    Property ID    : Integer           read GetID;
    Property Queue : TAMQPQueue        read GetQueue;
    Property State : TAMQPChannelState read GetState;

    Procedure ExchangeDeclare( AExchangeName, AType: String; APassive: Boolean = False; ADurable: Boolean = True; ANoWait: Boolean = False ; Arguments: TArguments = [] ); overload;
    Procedure ExchangeDeclare( AExchangeName: String; AType: TExchangeType; APassive: Boolean = False; ADurable: Boolean = True; ANoWait: Boolean = False ; Arguments: TArguments = [] ); overload;
    Procedure ExchangeBind(ADestination, ASource, ARoutingKey: String; ANoWait: Boolean = False; Arguments: TArguments = []);
    Procedure ExchangeDelete( AExchangeName: String; AIfUnused: Boolean = True; ANoWait: Boolean = False );
    Procedure QueueDeclare( AQueueName: String; APassive: Boolean = False; ADurable: Boolean = True; AExclusive: Boolean = False;
                            AAutoDelete: Boolean = False; ANoWait: Boolean = False; Arguments: TArguments = [] );
    Procedure QueueBind( AQueueName, AExchangeName, ARoutingKey: String; ANoWait: Boolean = False ; Arguments: TArguments = [] );
    Procedure QueuePurge( AQueueName: String; ANoWait: Boolean = False );
    Procedure QueueDelete( AQueueName: String; AIfUnused: Boolean = True; AIfEmpty: Boolean = True; ANoWait: Boolean = False );
    Procedure QueueUnBind( AQueueName, AExchangeName, ARoutingKey: String; Arguments: TArguments = [] );

    Procedure BasicPublish( AExchange, ARoutingKey: String; AData: TStream ); Overload;
    Procedure BasicPublish( AExchange, ARoutingKey: String; AData: TStream; AMandatory: Boolean ); Overload;
    Procedure BasicPublish( AExchange, ARoutingKey: String; AData: TStream; AMandatory: Boolean; AMessageProperties: IAMQPMessageProperties ); Overload;
    Procedure BasicPublish( AExchange, ARoutingKey: String; Const AData: String; AMandatory: Boolean = False ); Overload;

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
    Function DefaultMessageProperties: IAMQPMessageProperties;
    //Procedure WriteFrame( FrameType: Byte; Channel: Word; Payload: TStream; Size: Integer );
    procedure WriteMethod( Channel: Word; Method: TAMQPMethod );
    procedure WriteContent( Channel, ClassID: Word; Content: TStream; MessageProperties: IAMQPMessageProperties );
    Procedure HeartbeatReceived;
    Procedure InternalDisconnect( CloseConnection: Boolean );
    Procedure ServerDisconnect( Msg: String );
    Procedure CloseChannel( Channel: IAMQPChannel );
  end;

implementation

end.
