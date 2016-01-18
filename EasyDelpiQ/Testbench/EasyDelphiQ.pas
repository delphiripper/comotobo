unit EasyDelphiQ;

interface

Uses
  System.Classes, AMQP.Connection, AMQP.Interfaces;

Type
  TSubscriptionResult = Record
  Private
    FSubscriptionID: String;
  Public
    Exchange: String;
    Queue: String;
    //Procedure Cancel;
  End;

  TAction<T> = Reference to Procedure( Input: T );

  IBus = Interface ['{39AF7090-0900-478B-8BCA-D416434DE12F}']
    Procedure Publish( Msg: TObject; Topic: String = '' );
    //Function Subscribe<T>( SubscriptionID: String; OnMessage: TAction<T> ): TSubscriptionResult;
    //Function Get<T>: T;
  End;

  TSerializer   = Reference to Procedure( Obj: TObject; Stream: TStream );
  TDeSerializer = Reference to Procedure( Stream: TStream; Obj: TObject );

  TMessageHandler<T:Class,constructor> = Reference to Procedure( var Msg: T );

  //TBus = class(TInterfacedObject, IBus)
  TBus = class
  Private
    FConnection: TAMQPConnection;
    FDefaultChannel: IAMQPChannel;
    Serialize: TSerializer;
    DeSerialize: TDeSerializer;
  Public
    Property Connection: TAMQPConnection read FConnection;
    Procedure Publish( Msg: TObject; Topic: String = '' );
    Function Get<T:Class,constructor>(SubscriberID: String; Topic: String = ''): T;
    Function Subscribe<T:Class,constructor>(SubscriberID: String; MessageHandler: TMessageHandler<T>): TSubscriptionResult; Overload;
    Function Subscribe<T:Class,constructor>(SubscriberID: String; Topic: String; MessageHandler: TMessageHandler<T>): TSubscriptionResult; Overload;
    Procedure Connect;
    Constructor Create;
    Destructor Destroy; override;
  end;

  RabbitHutch = class
  Private
  Public
    Class Function CreateBus( Host: String; Username: String; Password: String ): TBus; overload;
  end;

implementation

Uses
  System.SysUtils, System.TypInfo, EasyDelphiQ.DTO, JSON, AMQP.Message;

{ TBus }

procedure TBus.Connect;
begin
  FConnection.Connect;
  FDefaultChannel := FConnection.OpenChannel;
end;

constructor TBus.Create;
begin
  FDefaultChannel := nil;
  FConnection := TAMQPConnection.Create;
  Serialize := Procedure( Obj: TObject; Stream: TStream )
               var
                 Str: String;
                 StrStream: TStringStream;
               Begin
                 Str := TJSONSerializer.Serialize( Obj );
                 StrStream := TStringStream.Create( Str, TEncoding.UTF8 );
                 Try
                   Stream.CopyFrom( StrStream, 0 );
                   Stream.Position := 0;
                 Finally
                   StrStream.Free;
                 End;
               End;
  DeSerialize := Procedure( Stream: TStream; Obj: TObject  )
                 var
                   StrStream: TStringStream;
                 Begin
                   StrStream := TStringStream.Create( '', TEncoding.UTF8 );
                   Try
                     StrStream.CopyFrom( Stream, 0 );
                     TJSONParser.Parse( StrStream.DataString, Obj );
                   Finally
                     StrStream.Free;
                   End;
                 End;
end;

destructor TBus.Destroy;
begin
  FDefaultChannel := nil;
  FConnection.Free;
  inherited;
end;

function TBus.Get<T>(SubscriberID: String; Topic: String = ''): T;
var
  Msg: TAMQPMessage;
  QueueName: string;
  Stream: TMemoryStream;
  Obj: T;
begin
  Result := nil;
  Msg := nil;
  Obj := T.Create;
  Try
    QueueName := GetQueueeName( Obj, SubscriberID );
    //Todo: Make map with [Obj.class,QueueName]
    //Todo: If Obj.class is in map then dont declare exchange
    FDefaultChannel.QueueDeclare( QueueName );
    FDefaultChannel.QueueBind( QueueName, GetExchangeName(Obj), Topic );

    Msg := FDefaultChannel.BasicGet( GetQueueeName( Obj, SubscriberID ) );
    if Msg <> nil then
    Begin
      DeSerialize( Msg.Body, Obj );
      Result := Obj;
      Obj := nil;
      Msg.Ack;
    End;
  Finally
    Msg.Free;
    Obj.Free;
  End;
end;

procedure TBus.Publish(Msg: TObject; Topic: String = '');
var
  ExchangeName: string;
  Stream: TMemoryStream;
begin
  ExchangeName := GetExchangeName( Msg );
  //Todo: Make map with [Obj.class,ExchangeName]
  //Todo: If Obj.class is in map then dont declare exchange
  FDefaultChannel.ExchangeDeclare( ExchangeName, etTopic );

  Stream := TMemoryStream.Create;
  Try
    Serialize( Msg, Stream );
    FDefaultChannel.BasicPublish( ExchangeName, Topic, Stream );
  Finally
    Stream.Free;
  End;
end;

function TBus.Subscribe<T>(SubscriberID: String; MessageHandler: TMessageHandler<T>): TSubscriptionResult;
begin
  Result := Subscribe<T>( SubscriberID, '', MessageHandler );
end;

function TBus.Subscribe<T>(SubscriberID, Topic: String; MessageHandler: TMessageHandler<T>): TSubscriptionResult;
var
  QueueName: String;
  Handler: TConsumerMethod;
  Obj: T;
begin
  Obj := T.Create;
  Try
    QueueName := GetQueueeName( Obj, SubscriberID );
    //Todo: Make map with [Obj.class,QueueName]
    //Todo: If Obj.class is in map then dont declare exchange
    FDefaultChannel.QueueDeclare( QueueName );
    FDefaultChannel.QueueBind( QueueName, GetExchangeName(Obj), Topic );
  Finally
    Obj.Free;
  End;
  Handler := Procedure( AMQPMessage: TAMQPMessage; var SendAck: Boolean )
             var
               Msg: T;
             Begin
               Msg := T.Create;
               Try
                 DeSerialize( AMQPMessage.Body, Msg );
                 MessageHandler( Msg );
                 SendAck := True;
               finally
                 Msg.Free;
               end;
             End;
  FDefaultChannel.BasicConsume( Handler, QueueName, SubscriberID );
end;

{ RabbitHutch }

class function RabbitHutch.CreateBus(Host, Username, Password: String): TBus;
var
  Bus: TBus;
begin
  Bus := TBus.Create;
  Try
    Bus.Connection.Host := Host;
    Bus.Connection.Username := Username;
    Bus.Connection.Password := Password;
    Bus.Connect;
    Result := Bus;
    Bus := nil;
  Finally
    Bus.Free;
  End;
end;

end.
