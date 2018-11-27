unit EasyDelphiQ.Interfaces;

interface

Uses
  AMQP.Interfaces, AMQP.Arguments;

Type
  IQueue = Interface ['{B6686F6E-3772-47C8-A30C-6FC26AB12084}']
    Function Name: String;
    Function Topic: String;
    Function SubscriberID: String;
    Function Exchange: String;
    Function ClassName: String;
    function Arguments: TArguments;
    Procedure Reconnect( Channel: IAMQPChannel );
  End;

  IExchange = Interface ['{58CB0E85-5E41-475D-BBCE-C7DCFA401AA9}']
    Function Name: String;
    Function ExchangeType: TExchangeType;
    Procedure Reconnect( Channel: IAMQPChannel );
  End;

  ISubscriptionResult = Interface ['{6B4F0D36-EE7C-47EC-92B9-4A7222476054}']
    Function Queue: IQueue;
    Procedure Cancel;
  End;


implementation

end.
