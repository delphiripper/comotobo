unit EasyDelphiQ;

interface

/// EasyNetQ compatibility:
///   The default Exchange name used is {Namespace/UnitName}.{ClassName}:{AssemblyName}
///
///
///

Uses
  System.Classes, System.Generics.Collections, VCL.ExtCtrls, EasyDelphiQ.Interfaces, AMQP.Connection, AMQP.Interfaces, AMQP.Arguments,
  EasyDelphiQ.DTO;

Type
  TSerializer   = Reference to Procedure( Obj: TObject; Stream: TStream );
  TDeSerializer = Reference to Procedure( Stream: TStream; Obj: TObject );

  TMessageHandler<T:Class,constructor> = Reference to Procedure( var Msg: T );

  TStartSubscription = Reference to Procedure;

  TOnDataReceivedEvent = Procedure( Sender: TObject; Const DataStr: String ) of object;

  //TBus = class(TInterfacedObject, IBus)
  TBus = class
  Private
    FConnectTimer: TTimer;
    FConnection: TAMQPConnection;
    FWasConnected: Boolean;
    FDefaultChannel: IAMQPChannel;
    FErrorChannel: IAMQPChannel;
    FSerializer: TSerializer;
    FDeSerializer: TDeSerializer;
    FQueues: TDictionary<String,IQueue>;
    FExchanges: TDictionary<String,IExchange>;
    FSubscriptions: TList<TStartSubscription>;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnDataReceived: TOnDataReceivedEvent;
    Function GetClassInformation<T: Class,constructor>: TClassInformation;
    Function GetDefaultChannel: IAMQPChannel;
    Procedure ConnectTimerOnTTimer(Sende: TObject);
    Procedure CheckConnection;
    Procedure Reconnect;
    Procedure DoConnected;
    Procedure DoDisconnected;
    Procedure ReceiveData( Const DataStr: String );
    Property Connection: TAMQPConnection read FConnection;
  Public
    Property OnConnected    : TNotifyEvent read FOnConnected    write FOnConnected;
    Property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;
    Property OnDataReceived : TOnDataReceivedEvent read FOnDataReceived write FOnDataReceived;

    Function MakeQueue(QueueName, ExchangeName, SubscriberID, Topic, DTOClassName: String; Arguments: TArguments): IQueue; Overload;
    Function MakeQueue<T:Class,constructor>(SubscriberID: String = ''; Topic: String = ''; Arguments: TArguments = []): IQueue; Overload;
    Function MakeExchange(Exchange: String; ExchangeType: TExchangeType): IExchange;

    Procedure Publish( Msg: TObject; Topic: String = '' );

    Function Get<T:Class,constructor>(SubscriberID: String; Topic: String = ''): T;

    /// <summary>
    ///   Messges are automatically acknowledged if no exception occurs in MessageHandler
    /// </summary>
    Function Subscribe<T:Class,constructor>( SubscriberID: String;
                                             MessageHandler: TMessageHandler<T>;
                                             Arguments: TArguments = [] ): ISubscriptionResult; Overload;
    /// <summary>
    ///   Messges are automatically acknowledged if no exception occurs in MessageHandler
    /// </summary>
    Function Subscribe<T:Class,constructor>( SubscriberID: String;
                                             Topic: String;
                                             MessageHandler: TMessageHandler<T>;
                                             Arguments: TArguments = [] ): ISubscriptionResult; Overload;
    /// <summary>
    ///   Messges are automatically acknowledged if no exception occurs in MessageHandler
    /// </summary>
    Function Subscribe<T:Class,constructor>( Queue: String;
                                             SubscriberID: String;
                                             Topic: String;
                                             MessageHandler: TMessageHandler<T>;
                                             Arguments: TArguments = [] ): ISubscriptionResult; Overload;
    /// <summary>
    ///   Messges are automatically acknowledged if no exception occurs in MessageHandler
    /// </summary>
    Function Subscribe<T:Class,constructor>( Queue: String;
                                             Exchange: String;
                                             SubscriberID: String;
                                             Topic: String;
                                             MessageHandler: TMessageHandler<T>;
                                             Arguments: TArguments = [] ): ISubscriptionResult; Overload;

    Constructor Create;
    Destructor Destroy; override;
  end;

  RabbitHutch = class
  Private
  Public
    Class Function CreateBus( Host: String; Username: String; Password: String ): TBus; overload;
    /// <summary>
    ///
    /// host=#{RabbitMQHost};username=#{RabbitMQUserName};password=#{RabbitMQPassword}
    /// </summary>
    /// <param name="ConnectionString">
    ///  A semicolon separated string of name=value pairs. Values must not be enclosed in quotes
    ///  Possible names are:
    ///    Host: IP or hostname of the RabbitMQ server
    ///    Port: Port to use for connection (default: 5672)
    ///    Username: RabbitMQ user name
    ///    Password: Password for RabbitMQ user
    ///    VirtualHost: The virtual host on the RabbitMQ server (default: '/')
    ///    RequestedHeartbeat: The requested interval for heartbeats from the RabbitMQ server
    ///    Product: Application name
    ///    Platform: Host name
    ///    Application: Executable file
    ///  Example: 'host=some_server;username=rabbit;password=rabbitsfoot;port=5672'
    /// </param>
    Class Function CreateBus( ConnectionString: String ): TBus; overload;
  end;

implementation

Uses
  System.SysUtils, System.TypInfo, EasyDelphiQ.Classes, DJSON, AMQP.Message, AMQP.IMessageProperties,
   AMQP.MessageProperties, AMQP.Types;

{ TBus }

procedure TBus.CheckConnection;
begin
  if not FConnection.IsOpen then
  Begin
    If FWasConnected then
    Begin
      FWasConnected := False;
      DoDisconnected;
    End;

    Try
      FConnection.Connect;
      FWasConnected := True;
      FConnectTimer.Enabled := True;
      FDefaultChannel := FConnection.OpenChannel;
      FErrorChannel := FConnection.OpenChannel;
      If (FQueues.Count > 0) or (FExchanges.Count > 0) then
        Reconnect;
      DoConnected;
    Except
      On E: Exception do
        raise EEasyDelphiQConnectionFailed.Create('Connect failed: ' + E.Message);
    End;
  End;
end;

procedure TBus.ConnectTimerOnTTimer(Sende: TObject);
begin
  Try
    CheckConnection;
  Except
    On E: Exception do; //Ignore reconnect errors
  End;
end;

constructor TBus.Create;
begin
  FDefaultChannel := nil;
  FOnConnected := nil;
  FOnDisconnected := nil;
  FOnDataReceived := nil;
  FWasConnected := False;
  FConnectTimer := TTimer.Create(nil);
  FConnectTimer.OnTimer := ConnectTimerOnTTimer;
  FConnectTimer.Interval := 5000;
  FConnectTimer.Enabled := False;
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
                     JSON: TJSON_Element;
                     DataStr: String;
                   Begin
                     JSON := nil;
                     StrStream := TStringStream.Create( '', TEncoding.UTF8 );
                     Try
                       StrStream.CopyFrom( Stream, 0 );
                       DataStr := StrStream.DataString;
                       Self.ReceiveData( DataStr );
                       JSON := TJSON.ParseText( DataStr );
                       //EasyNetQ encapsulation:
                       if JSON.IsObject and
                          (JSON.AsObject.FindProperty('Data') <> nil) and
                          (JSON.AsObject.FindProperty('CorrelationId') <> nil) then
                         TJSONParser.Parse( JSON.AsObject.FindProperty('Data').Value, Obj )
                       else
                         TJSONParser.Parse( JSON, Obj );
                     Finally
                       StrStream.Free;
                       JSON.Free;
                     End;
                   End;
end;

destructor TBus.Destroy;
begin
  FQueues.Free;
  FExchanges.Free;
  FDefaultChannel := nil;
  FErrorChannel := nil;
  FConnection.Free;
  FSubscriptions.Free;
  FConnectTimer.Free;
  inherited;
end;

procedure TBus.DoConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected( Self );
end;

procedure TBus.DoDisconnected;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected( Self );
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

function TBus.GetClassInformation<T>: TClassInformation;
var
  Obj: T;
begin
  Obj := T.Create;
  Try
    Result := TClassInformation.Create( Obj );
  Finally
    Obj.Free;
  End;
end;

function TBus.GetDefaultChannel: IAMQPChannel;
begin
  Result := FDefaultChannel;
end;

function TBus.MakeExchange(Exchange: String; ExchangeType: TExchangeType): IExchange;
begin
  CheckConnection;
  if not FExchanges.TryGetValue( Exchange, Result ) then
  Begin
    Result := TExchange.Create( FDefaultChannel, Exchange, ExchangeType );
    FDefaultChannel.ExchangeDeclare( Result.Name, Result.ExchangeType );
    FErrorChannel.ExchangeDeclare( Result.Name+'_error', etFanout);
    FExchanges.Add( Exchange, Result );
  End;
end;

function TBus.MakeQueue(QueueName, ExchangeName, SubscriberID, Topic, DTOClassName: String; Arguments: TArguments): IQueue;
var
  Key: string;
begin
  CheckConnection;
  Result := nil;
  Key    := QueueName + '_' + Topic + '_' + SubscriberID + '_' + ExchangeName;
  If not FQueues.TryGetValue( Key, Result ) then
  Begin
    Result := TQueue.Create( FDefaultChannel, QueueName, Topic, SubscriberID, ExchangeName, DTOClassName, Arguments );
    FDefaultChannel.QueueDeclare( Result.Name, False, True, False, False, False, Arguments );
    FDefaultChannel.QueueBind( Result.Name, Result.Exchange, Result.Topic );
    FErrorChannel.QueueDeclare( Result.Name+'_error', False, True, False, False, False, nil );
    FErrorChannel.QueueBind( Result.Name+'_error', Result.Exchange+'_error', Result.Topic );

    FQueues.Add( Key, Result );
  End;
end;

function TBus.MakeQueue<T>(SubscriberID: String = ''; Topic: String = ''; Arguments: TArguments = []): IQueue;
var
  ClassInfo: TClassInformation;
begin
  ClassInfo := GetClassInformation<T>;
  Result := MakeQueue( ClassInfo.GetQueueName(SubscriberID),
                       ClassInfo.GetExchangeName,
                       SubscriberID,
                       Topic,
                       ClassInfo.GetClassName,
                       Arguments );
end;

procedure TBus.Publish(Msg: TObject; Topic: String = '');
var
  Exchange: IExchange;
  Stream: TMemoryStream;
  ExchangeName: String;
  ClassInfo: TClassInformation;
  MessageProperties: IAMQPMessageProperties;
begin
  ClassInfo         := TClassInformation.Create( Msg );
  ExchangeName      := ClassInfo.GetExchangeName;
  Exchange          := MakeExchange( ExchangeName, etTopic );
  Stream            := TMemoryStream.Create;
  MessageProperties := FConnection.DefaultMessageProperties;
  Try
    FSerializer( Msg, Stream );
    MessageProperties.&Type.Value := ClassInfo.GetFullyQualifiedClassName;
    MessageProperties.ContentType.Value := 'application/json';
    FDefaultChannel.BasicPublish( Exchange.Name, Topic, Stream, False, MessageProperties );
  Finally
    Stream.Free;
  End;
end;

procedure TBus.ReceiveData(const DataStr: String);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived( Self, DataStr );
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
    FErrorChannel.ExchangeDeclare( ExchangePair.Value.Name+'_error', etFanout);
  End;
  for QueuePair in FQueues do
  Begin
    QueuePair.Value.Reconnect( FDefaultChannel );
    FDefaultChannel.QueueDeclare( QueuePair.Value.Name, False, True, False, False, False, QueuePair.Value.Arguments );
    FDefaultChannel.QueueBind( QueuePair.Value.Name, QueuePair.Value.Exchange, QueuePair.Value.Topic );
    FErrorChannel.QueueDeclare( QueuePair.Value.Name+'_error');
    FErrorChannel.QueueBind( QueuePair.Value.Name+'_error', QueuePair.Value.Exchange+'_error', QueuePair.Value.Topic);

  End;
  for Method in FSubscriptions do
    Method();
end;

function TBus.Subscribe<T>(Queue, SubscriberID, Topic: String; MessageHandler: TMessageHandler<T>; Arguments: TArguments): ISubscriptionResult;
begin
  Result := Subscribe<T>( Queue, '', SubscriberID, Topic, MessageHandler, Arguments );
end;

function TBus.Subscribe<T>(Queue, Exchange, SubscriberID, Topic: String; MessageHandler: TMessageHandler<T>; Arguments: TArguments): ISubscriptionResult;
var
  QueueIntf: IQueue;
  QueueName: String;
  Method: TStartSubscription;
  ClassInfo: TClassInformation;
begin
  ClassInfo := GetClassInformation<T>;
  if Exchange = '' then
    Exchange := ClassInfo.GetExchangeName;
  if Queue = '' then
    Queue := ClassInfo.GetQueueName( SubscriberID );
  QueueIntf := MakeQueue(Queue, Exchange, SubscriberID, Topic, ClassInfo.GetClassName, Arguments );
  Result := TSubscriptionResult.Create( FDefaultChannel, QueueIntf );
  Method := Procedure
            Begin
              GetDefaultChannel.BasicConsume(  Procedure( AMQPMessage: TAMQPMessage; var SendAck: Boolean )
                                               var
                                                 Msg: T;
                                                 Prop: IAMQPMessageProperties;
                                               Begin
                                                 Msg := T.Create;
                                                 Try
                                                  try
                                                   FDeSerializer( AMQPMessage.Body, Msg );
                                                   MessageHandler( Msg );
                                                  except
                                                   on E: Exception do
                                                    begin
                                                      Prop := AMQPMessage.Header.PropertyList;
                                                      Prop.ApplicationHeaders.Add('Exception',
                                                        TFieldTable.Create
                                                          .Add('Class', E.ClassName)
                                                          .Add('Message', E.Message));
                                                      FErrorChannel.BasicPublish(AMQPMessage.Exchange+'_error',
                                                        AMQPMessage.RoutingKey, AMQPMessage.Body, False, Prop);
                                                    end;
                                                  end;
                                                  SendAck := True;
                                                 finally
                                                   Msg.Free;
                                                 end;
                                               End,
                                               Queue,
                                               SubscriberID );
            End;
  FSubscriptions.Add( Method );
  Method();
end;

function TBus.Subscribe<T>(SubscriberID: String; MessageHandler: TMessageHandler<T>; Arguments: TArguments): ISubscriptionResult;
begin
  Result := Subscribe<T>( SubscriberID, '', MessageHandler, Arguments );
end;

function TBus.Subscribe<T>(SubscriberID, Topic: String; MessageHandler: TMessageHandler<T>; Arguments: TArguments): ISubscriptionResult;
var
  ClassInfo: TClassInformation;
begin
  ClassInfo := GetClassInformation<T>;
  Result := Subscribe<T>( ClassInfo.GetQueueName(SubscriberID), ClassInfo.GetExchangeName, SubscriberID, Topic, MessageHandler, Arguments );
end;

{ RabbitHutch }

class function RabbitHutch.CreateBus(Host, Username, Password: String): TBus;
var
  Bus: TBus;
begin
  Bus := TBus.Create;
  Try
    Bus.Connection.ClientAPI := 'EasyDelphiQ';
    Bus.Connection.Host := Host;
    Bus.Connection.Username := Username;
    Bus.Connection.Password := Password;
    Result := Bus;
    Bus := nil;
  Finally
    Bus.Free;
  End;
end;

class function RabbitHutch.CreateBus(ConnectionString: String): TBus;

  Function UseDefault( Const Value, DefaultValue: String ): String;
  Begin
    Result := Value;
    if Result = '' then
      Result := DefaultValue;
  End;

var
  Bus: TBus;
  Strings: TStringList;
begin
  Bus := nil;
  Strings := TStringList.Create;
  Try
    Strings.StrictDelimiter := True;
    Strings.Delimiter := ';';
    Strings.DelimitedText := ConnectionString;

    Bus := TBus.Create;
    Bus.Connection.ClientAPI     := 'EasyDelphiQ';
    Bus.Connection.Host          := Strings.Values[ 'Host' ];
    Bus.Connection.Username      := Strings.Values[ 'username' ];
    Bus.Connection.Password      := Strings.Values[ 'password' ];
    Bus.Connection.VirtualHost   := UseDefault( Strings.Values[ 'VirtualHost' ], '/' );
    Bus.Connection.HeartbeatSecs := UseDefault( Strings.Values[ 'RequestedHeartbeat' ], '180' ).ToInteger;
    Bus.Connection.Port          := UseDefault( Strings.Values[ 'Port' ], '5672' ).ToInteger;
    if Strings.Values[ 'Platform' ] <> '' then
      Bus.Connection.PlatformID := Strings.Values[ 'Platform' ];
    if Strings.Values[ 'Product' ] <> '' then
      Bus.Connection.ProductID := Strings.Values[ 'Product' ];
    if Strings.Values[ 'Application' ] <> '' then
      Bus.Connection.ApplicationID := Strings.Values[ 'Application' ];

    Result := Bus;
    Bus := nil;
  Finally
    Bus.Free;
    Strings.Free;
  End;
end;

end.
