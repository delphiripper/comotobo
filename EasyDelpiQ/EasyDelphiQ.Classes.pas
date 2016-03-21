unit EasyDelphiQ.Classes;

interface

Uses
  AMQP.Interfaces, EasyDelphiQ.Interfaces;

Type
  TQueue = class(TInterfacedObject, IQueue)
  Private
    FChannel: IAMQPChannel;
    FName: String;
    FTopic: String;
    FSubscriberID: String;
    FExchange: String;
    FClassName: String;
  Public
    Function Name: String;
    Function Topic: String;
    Function SubscriberID: String;
    Function Exchange: String;
    Function ClassName: String;
    Procedure Reconnect( Channel: IAMQPChannel );
    Constructor Create( AChannel: IAMQPChannel; AName: String; ATopic: String; ASubscriberID: String; AExchange: String;
                        AClassName: String );
  End;

  TExchange = class(TInterfacedObject, IExchange)
  Private
    FChannel: IAMQPChannel;
    FName: String;
    FExchangeType: TExchangeType;
  Public
    Function Name: String;
    Function ExchangeType: TExchangeType;
    Procedure Reconnect( Channel: IAMQPChannel );
    Constructor Create( AChannel: IAMQPChannel; AName: String; AExchangeType: TExchangeType );
  End;

  TSubscriptionResult = Class(TInterfacedObject, ISubscriptionResult)
  Private
    FChannel: IAMQPChannel;
    FQueue: IQueue;
  Public
    Function Queue: IQueue;
    Procedure Cancel;
    Constructor Create( AChannel: IAMQPChannel; AQueue: IQueue );
  End;

implementation

{ TQueue }

function TQueue.ClassName: String;
begin
  Result := FClassName;
end;

constructor TQueue.Create(AChannel: IAMQPChannel; AName, ATopic, ASubscriberID, AExchange, AClassName: String);
begin
  FChannel      := AChannel;
  FName         := AName;
  FTopic        := ATopic;
  FSubscriberID := ASubscriberID;
  FExchange     := AExchange;
  FClassName    := AClassName;
end;

function TQueue.Exchange: String;
begin
  Result := FExchange;
end;

function TQueue.Name: String;
begin
  Result := FName;
end;

procedure TQueue.Reconnect(Channel: IAMQPChannel);
begin
  FChannel := nil;
  FChannel := Channel;
end;

function TQueue.SubscriberID: String;
begin
  Result := FSubscriberID;
end;

function TQueue.Topic: String;
begin
  Result := FTopic;
end;

{ TExchange }

constructor TExchange.Create(AChannel: IAMQPChannel; AName: String; AExchangeType: TExchangeType);
begin
  FChannel      := AChannel;
  FName         := AName;
  FExchangeType := AExchangeType;
end;

function TExchange.ExchangeType: TExchangeType;
begin
  Result := FExchangeType;
end;

function TExchange.Name: String;
begin
  Result := FName;
end;

procedure TExchange.Reconnect(Channel: IAMQPChannel);
begin
  FChannel := nil;
  FChannel := Channel;
end;

{ TSubscriptionResult }

procedure TSubscriptionResult.Cancel;
begin
  FChannel.BasicCancel( FQueue.SubscriberID );
end;

constructor TSubscriptionResult.Create(AChannel: IAMQPChannel; AQueue: IQueue);
begin
  FChannel := AChannel;
  FQueue   := AQueue;
end;

function TSubscriptionResult.Queue: IQueue;
begin
  Result := FQueue;
end;

end.
