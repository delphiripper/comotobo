unit EasyDelphiQ;

interface

Uses
  System.Classes, System.Generics.Collections, EasyDelphiQ.Interfaces, AMQP.Connection, AMQP.Interfaces;

Type
  TSerializer   = Reference to Procedure( Obj: TObject; Stream: TStream );
  TDeSerializer = Reference to Procedure( Stream: TStream; Obj: TObject );

  TMessageHandler<T:Class,constructor> = Reference to Procedure( var Msg: T );

  TStartSubscription = Reference to Procedure;

  //TBus = class(TInterfacedObject, IBus)
  TBus = class
  Private
    FConnection: TAMQPConnection;
    FDefaultChannel: IAMQPChannel;
    FSerializer: TSerializer;
    FDeSerializer: TDeSerializer;
    FQueues: TDictionary<String,IQueue>;
    FExchanges: TDictionary<String,IExchange>;
    FSubscriptions: TList<TStartSubscription>;
    Function GetDefaultChannel: IAMQPChannel;
    Procedure CheckConnection;
    Procedure Reconnect;
  Public
    Property Connection: TAMQPConnection read FConnection;

    Function MakeQueue<T:Class,constructor>(SubscriberID: String = ''; Topic: String = ''): IQueue;
    Function MakeExchange(Msg: TObject; ExchangeType: TExchangeType): IExchange;

    Procedure Publish( Msg: TObject; Topic: String = '' );

    Function Get<T:Class,constructor>(SubscriberID: String; Topic: String = ''): T;
    Function Subscribe<T:Class,constructor>(SubscriberID: String; MessageHandler: TMessageHandler<T>): ISubscriptionResult; Overload;
    Function Subscribe<T:Class,constructor>(SubscriberID: String; Topic: String; MessageHandler: TMessageHandler<T>): ISubscriptionResult; Overload;

    Procedure TestReconnect;

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
  System.SysUtils, System.TypInfo, EasyDelphiQ.Classes, EasyDelphiQ.DTO, JSON, AMQP.Message;

{ TBus }

procedure TBus.CheckConnection;
begin
  if not FConnection.IsOpen then
  Begin
    FConnection.Connect;
    FDefaultChannel := FConnection.OpenChannel;
    If (FQueues.Count > 0) or (FExchanges.Count > 0) then
      Reconnect;
  End;
end;

constructor TBus.Create;
begin
  FDefaultChannel := nil;
  FQueues := TDictionary<String,IQueue>.Create;
  FExchanges := TDictionary<String,IExchange>.Create;
  FConnection := TAMQPConnection.Create;
  FSubscriptions := TList<TStartSubscription>.Create;
  FSerializer := Procedure( Obj: TObject; Stream: TStream )
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
  FDeSerializer := Procedure( Stream: TStream; Obj: TObject  )
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
  FQueues.Free;
  FExchanges.Free;
  FDefaultChannel := nil;
  FConnection.Free;
  FSubscriptions.Free;
  inherited;
end;

function TBus.Get<T>(SubscriberID: String; Topic: String = ''): T;
var
  Msg: TAMQPMessage;
  Queue: IQueue;
  Stream: TMemoryStream;
  Obj: T;
begin
  Result := nil;
  Msg    := nil;
  Queue  := MakeQueue<T>(SubscriberID, Topic);
  Obj    := T.Create;
  Try
    Msg := FDefaultChannel.BasicGet( Queue.Name );
    if Msg <> nil then
    Begin
      FDeSerializer( Msg.Body, Obj );
      Result := Obj;
      Obj := nil;
      Msg.Ack;
    End;
  Finally
    Msg.Free;
    Obj.Free;
  End;
end;

function TBus.GetDefaultChannel: IAMQPChannel;
begin
  Result := FDefaultChannel;
end;

function TBus.MakeExchange(Msg: TObject; ExchangeType: TExchangeType): IExchange;
var
  ExchangeName: string;
begin
  CheckConnection;
  ExchangeName := GetExchangeName( Msg );
  if not FExchanges.TryGetValue( ExchangeName, Result ) then
  Begin
    Result := TExchange.Create( FDefaultChannel, ExchangeName, ExchangeType );
    FDefaultChannel.ExchangeDeclare( Result.Name, Result.ExchangeType );
    FExchanges.Add( ExchangeName, Result );
  End;
end;

function TBus.MakeQueue<T>(SubscriberID: String = ''; Topic: String = ''): IQueue;
var
  Msg: T;
  Key: string;
  QueueeName: String;
  ExchangeName: String;
begin
  CheckConnection;
  Result := nil;
  Msg := T.Create;
  Try
    QueueeName   := GetQueueeName( Msg, SubscriberID );
    ExchangeName := GetExchangeName( Msg );
    Key          := QueueeName + '_' + Topic + '_' + SubscriberID + '_' + ExchangeName + '_' + Msg.ClassName;
    If not FQueues.TryGetValue( Key, Result ) then
    Begin
      Result := TQueue.Create( FDefaultChannel, QueueeName, Topic, SubscriberID, ExchangeName, Msg.ClassName );
      FDefaultChannel.QueueDeclare( Result.Name );
      FDefaultChannel.QueueBind( Result.Name, Result.Exchange, Result.Topic );
      FQueues.Add( Key, Result );
    End;
  Finally
    Msg.Free;
  End;
end;

procedure TBus.Publish(Msg: TObject; Topic: String = '');
var
  Exchange: IExchange;
  Stream: TMemoryStream;
begin
  Exchange := MakeExchange( Msg, etTopic );
  Stream := TMemoryStream.Create;
  Try
    FSerializer( Msg, Stream );
    FDefaultChannel.BasicPublish( Exchange.Name, Topic, Stream );
  Finally
    Stream.Free;
  End;
end;

procedure TBus.Reconnect;
var
  ExchangePair: TPair<String, IExchange>;
  QueuePair: TPair<String, IQueue>;
  Method: TStartSubscription;
begin
  for ExchangePair in FExchanges do
  Begin
    ExchangePair.Value.Reconnect( FDefaultChannel );
    FDefaultChannel.ExchangeDeclare( ExchangePair.Value.Name, ExchangePair.Value.ExchangeType );
  End;
  for QueuePair in FQueues do
  Begin
    QueuePair.Value.Reconnect( FDefaultChannel );
    FDefaultChannel.QueueDeclare( QueuePair.Value.Name );
    FDefaultChannel.QueueBind( QueuePair.Value.Name, QueuePair.Value.Exchange, QueuePair.Value.Topic );
  End;
  for Method in FSubscriptions do
    Method();
end;

function TBus.Subscribe<T>(SubscriberID: String; MessageHandler: TMessageHandler<T>): ISubscriptionResult;
begin
  Result := Subscribe<T>( SubscriberID, '', MessageHandler );
end;

function TBus.Subscribe<T>(SubscriberID, Topic: String; MessageHandler: TMessageHandler<T>): ISubscriptionResult;
var
  Queue: IQueue;
  QueueName: String;
  Method: TStartSubscription;
begin
  Queue  := MakeQueue<T>(SubscriberID, Topic);
  QueueName := Queue.Name;
  Result := TSubscriptionResult.Create( FDefaultChannel, Queue );
  Method := Procedure
            Begin
              GetDefaultChannel.BasicConsume(  Procedure( AMQPMessage: TAMQPMessage; var SendAck: Boolean )
                                               var
                                                 Msg: T;
                                               Begin
                                                 Msg := T.Create;
                                                 Try
                                                   FDeSerializer( AMQPMessage.Body, Msg );
                                                   MessageHandler( Msg );
                                                   SendAck := True;
                                                 finally
                                                   Msg.Free;
                                                 end;
                                               End,
                                               QueueName,
                                               SubscriberID );
            End;
  FSubscriptions.Add( Method );
  Method();
end;

procedure TBus.TestReconnect;
begin
  FConnection.Disconnect;
  CheckConnection;
  //Reconnect;
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
    Result := Bus;
    Bus := nil;
  Finally
    Bus.Free;
  End;
end;

end.
