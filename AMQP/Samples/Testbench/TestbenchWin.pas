unit TestbenchWin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.SyncObjs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  AMQP.Connection, AMQP.Interfaces, AMQP.Classes;

type
  TConsumerThread = Class(TThread)
  Strict Private
    FQueue: TAMQPMessageQueue;
    Procedure WriteLine( Text: String; Sync: Boolean );
  Protected
    Procedure Execute; Override;
    Constructor Create( AQueue: TAMQPMessageQueue ); Reintroduce;
  End;

  TTestbenchForm = class(TForm)
    MemoMessages: TMemo;
    ButtonConnect: TButton;
    MemoSent: TMemo;
    MemoReceived: TMemo;
    MemoSentBytes: TMemo;
    MemoReceivedBytes: TMemo;
    ButtonDisconnect: TButton;
    ButtonPublishRed: TButton;
    ButtonOpenChannel: TButton;
    ButtonCloseChannel: TButton;
    ButtonExchangeDeclare: TButton;
    ButtonExchangeDelete: TButton;
    ButtonQueueDeclare: TButton;
    ButtonQueueDelete: TButton;
    ButtonQueueBind: TButton;
    ButtonQueueUnbind: TButton;
    ButtonPublishBlue: TButton;
    ButtonGetRed: TButton;
    ButtonGetBlue: TButton;
    ButtonConfirmSelect: TButton;
    ButtonConsumeBlue: TButton;
    Panel1: TPanel;
    LabelStatus: TLabel;
    Timer1: TTimer;
    ButtonPurgeRed: TButton;
    ButtonCancelBlue: TButton;
    ButtonReject: TButton;
    ButtonThreadConsume: TButton;
    ButtonCancelRed: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonPublishRedClick(Sender: TObject);
    procedure ButtonOpenChannelClick(Sender: TObject);
    procedure ButtonCloseChannelClick(Sender: TObject);
    procedure ButtonExchangeDeclareClick(Sender: TObject);
    procedure ButtonExchangeDeleteClick(Sender: TObject);
    procedure ButtonQueueDeclareClick(Sender: TObject);
    procedure ButtonQueueDeleteClick(Sender: TObject);
    procedure ButtonQueueBindClick(Sender: TObject);
    procedure ButtonQueueUnbindClick(Sender: TObject);
    procedure ButtonPublishBlueClick(Sender: TObject);
    procedure ButtonGetRedClick(Sender: TObject);
    procedure ButtonGetBlueClick(Sender: TObject);
    procedure ButtonConfirmSelectClick(Sender: TObject);
    procedure ButtonConsumeBlueClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonPurgeRedClick(Sender: TObject);
    procedure ButtonCancelBlueClick(Sender: TObject);
    procedure ButtonRejectClick(Sender: TObject);
    procedure ButtonThreadConsumeClick(Sender: TObject);
    procedure ButtonCancelRedClick(Sender: TObject);
  private
    AMQP: TAMQPConnection;
    Channel: IAMQPChannel;
    RecvWireDebug: TStringList;
    SentWireDebug: TStringList;
    RecvMethodDebug: TStringList;
    SentMethodDebug: TStringList;
    DebugLock: TCriticalSection;
    Thread: TConsumerThread;

    Procedure ConsumeDebug( AMemo: TMemo; AStrings: TStrings );
    Procedure AddAndScrollToBottom( AMemo: TMemo; AStrings: TStrings );

    Procedure AMQPWireEvent( Sender: TObject; SendRecv: TSendRecv; AStrings: TStrings );
    Procedure AMQPDebugEvent( Sender: TObject; SendRecv: TSendRecv; AStrings: TStrings );
  public
  end;

var
  TestbenchForm: TTestbenchForm;

implementation

Uses
  AMQP.Message, AMQP.StreamHelper;

{$R *.dfm}

procedure TTestbenchForm.AddAndScrollToBottom(AMemo: TMemo; AStrings: TStrings);
begin
  AMemo.Lines.AddStrings( AStrings );
  SendMessage( AMemo.Handle, EM_LINESCROLL, 0, AMemo.Lines.Count);
end;

procedure TTestbenchForm.AMQPDebugEvent(Sender: TObject; SendRecv: TSendRecv; AStrings: TStrings);
begin
  DebugLock.Enter;
  Try
    case SendRecv of
      srSend    : SentMethodDebug.AddStrings( AStrings );
      srReceive : RecvMethodDebug.AddStrings( AStrings );
    end;
  Finally
    DebugLock.Release;
  End;
end;

procedure TTestbenchForm.AMQPWireEvent(Sender: TObject; SendRecv: TSendRecv; AStrings: TStrings);
begin
  DebugLock.Enter;
  Try
    case SendRecv of
      srSend    : SentWireDebug.AddStrings( AStrings );
      srReceive : RecvWireDebug.AddStrings( AStrings );
    end;
  Finally
    DebugLock.Release;
  End;
end;

procedure TTestbenchForm.ButtonGetBlueClick(Sender: TObject);
var
  Msg: TAMQPMessage;
begin
  Msg := Channel.BasicGet( 'blue' );
  Try
    if Msg <> nil then
    Begin
      MemoMessages.Lines.Add( 'Message: ' + Msg.Body.AsString[ TEncoding.ASCII ] );
      Msg.Ack;
    End
    Else
      MemoReceived.Lines.Add( 'No message' );
  Finally
    Msg.Free;
  End;
end;

procedure TTestbenchForm.ButtonQueueUnbindClick(Sender: TObject);
begin
  Channel.QueueUnBind( 'red',  'Colors', 'color.red' );
  Channel.QueueUnBind( 'blue', 'Colors', 'color.blue' );
end;

procedure TTestbenchForm.ButtonRejectClick(Sender: TObject);
var
  Msg: TAMQPMessage;
begin
  Msg := Channel.BasicGet( 'red' );
  Try
    if Msg <> nil then
    Begin
      MemoMessages.Lines.Add( 'Message: ' + Msg.Body.AsString[ TEncoding.ASCII ] );
      Msg.Reject;
    End
    Else
      MemoReceived.Lines.Add( 'No message' );
  Finally
    Msg.Free;
  End;
end;

procedure TTestbenchForm.ConsumeDebug(AMemo: TMemo; AStrings: TStrings);
begin
  If AStrings.Count > 0 then
  Begin
    AddAndScrollToBottom( AMemo, AStrings );
    AStrings.Clear;
  End;
end;

procedure TTestbenchForm.ButtonQueueBindClick(Sender: TObject);
begin
  Channel.QueueBind( 'red',  'Colors', 'color.red' );
  Channel.QueueBind( 'blue', 'Colors', 'color.blue' );
end;

procedure TTestbenchForm.ButtonQueueDeclareClick(Sender: TObject);
begin
  Channel.QueueDeclare( 'red' );
  Channel.QueueDeclare( 'blue' );
end;

procedure TTestbenchForm.ButtonQueueDeleteClick(Sender: TObject);
begin
  Channel.QueueDelete( 'red',  False, False );
  Channel.QueueDelete( 'blue', False, False );
end;

procedure TTestbenchForm.ButtonExchangeDeclareClick(Sender: TObject);
begin
  Channel.ExchangeDeclare( 'Colors', etTopic );
end;

procedure TTestbenchForm.ButtonExchangeDeleteClick(Sender: TObject);
begin
  Channel.ExchangeDelete( 'Colors' );
end;

procedure TTestbenchForm.ButtonGetRedClick(Sender: TObject);
var
  Msg: TAMQPMessage;
begin
  Msg := Channel.BasicGet( 'red' );
  Try
    if Msg <> nil then
    Begin
      MemoMessages.Lines.Add( 'Message: ' + Msg.Body.AsString[ TEncoding.ASCII ] );
      Msg.Ack;
    End
    Else
      MemoReceived.Lines.Add( 'No message' );
  Finally
    Msg.Free;
  End;
end;

procedure TTestbenchForm.ButtonDisconnectClick(Sender: TObject);
begin
  Try
    AMQP.Disconnect;
  Finally
    Channel := nil;
  End;
end;

procedure TTestbenchForm.ButtonOpenChannelClick(Sender: TObject);
begin
  Channel := AMQP.OpenChannel;
end;

procedure TTestbenchForm.ButtonPublishBlueClick(Sender: TObject);
begin
  if Channel = nil then
    raise Exception.Create('Channel not open');
  Channel.BasicPublish( 'Colors', 'color.blue', 'I see cyan in your eyes!' );
end;

procedure TTestbenchForm.ButtonPublishRedClick(Sender: TObject);
begin
  if Channel = nil then
    raise Exception.Create('Channel not open');
  Channel.BasicPublish( 'Colors', 'color.red', 'Magenta is the word!' );
end;

procedure TTestbenchForm.ButtonPurgeRedClick(Sender: TObject);
begin
  Channel.QueuePurge( 'red' );
end;

procedure TTestbenchForm.ButtonCancelRedClick(Sender: TObject);
begin
  Channel.BasicCancel( 'ConsumeRed' );
end;

procedure TTestbenchForm.ButtonCancelBlueClick(Sender: TObject);
begin
  Channel.BasicCancel( 'ConsumeBlue' );
end;

procedure TTestbenchForm.ButtonThreadConsumeClick(Sender: TObject);
var
  Queue: TAMQPMessageQueue;
begin
  if Channel = nil then
    raise Exception.Create('Channel not open');
  Queue := TAMQPMessageQueue.Create;
  Channel.BasicConsume( Queue, 'red', 'ConsumeRed' );
  Thread.Free;
  Thread := nil;
  Thread := TConsumerThread.Create( Queue );
end;

procedure TTestbenchForm.ButtonCloseChannelClick(Sender: TObject);
begin
  Try
    Channel.Close;
  Finally
    Channel := nil;
  End;
end;

procedure TTestbenchForm.ButtonConfirmSelectClick(Sender: TObject);
begin
  Channel.ConfirmSelect;
end;

procedure TTestbenchForm.ButtonConnectClick(Sender: TObject);
begin
  AMQP.Host          := 'localhost';
  AMQP.Port          := 5672;
  AMQP.VirtualHost   := '/';
  AMQP.Username      := 'TestUser';
  AMQP.Password      := 'password';
  AMQP.ApplicationID := 'Testbench';
  AMQP.Connect;
  MemoMessages.Lines.Add( 'AMQP.ClassName: ' + AMQP.ClassName );
  MemoMessages.Lines.Add( 'AMQP.UnitName:  ' + AMQP.UnitName );
  MemoMessages.Lines.Add( 'AMQP.UnitScope: ' + AMQP.UnitScope );
end;

procedure TTestbenchForm.ButtonConsumeBlueClick(Sender: TObject);
begin
  if Channel = nil then
    raise Exception.Create('Channel not open');
  Channel.BasicConsume( Procedure( Msg: TAMQPMessage; var SendAck: Boolean )
                        var
                          Txt : String;
                        Begin
                          Txt := Msg.Body.AsString[ TEncoding.ASCII ];
                          TThread.Synchronize( nil, Procedure
                                                    Begin
                                                      MemoMessages.Lines.Add( Txt );
                                                    End );
                        End,
                        'blue', 'ConsumeBlue' );
end;

procedure TTestbenchForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  RecvWireDebug   := TStringList.Create;
  SentWireDebug   := TStringList.Create;
  RecvMethodDebug := TStringList.Create;
  SentMethodDebug := TStringList.Create;
  DebugLock   := TCriticalSection.Create;
  AMQP := TAMQPConnection.Create;
  AMQP.HeartbeatSecs := 120;
  AMQP.OnWireDebug   := AMQPWireEvent;
  AMQP.OnDebug       := AMQPDebugEvent;
  Channel := nil;
  Thread := nil;
end;

procedure TTestbenchForm.FormDestroy(Sender: TObject);
begin
  Channel := nil;
  AMQP.Free;
  Thread.Free;
  DebugLock.Free;
  RecvWireDebug.Free;
  SentWireDebug.Free;
  RecvMethodDebug.Free;
  SentMethodDebug.Free;
end;

procedure TTestbenchForm.Timer1Timer(Sender: TObject);
var
  Status: TStringList;
begin
  DebugLock.Enter;
  Try
    ConsumeDebug( MemoSentBytes,     SentWireDebug );
    ConsumeDebug( MemoReceivedBytes, RecvWireDebug );
    ConsumeDebug( MemoSent,          SentMethodDebug );
    ConsumeDebug( MemoReceived,      RecvMethodDebug );
  Finally
    DebugLock.Release;
  End;

  Status := TStringList.Create;
  Try
    Status.Values[ 'LastHeartbeat' ] := DateTimeToStr(AMQP.LastHeartbeat);
    Status.Values[ 'IsOpen' ]        := BoolToStr( AMQP.IsOpen, True );
    LabelStatus.Caption := Status.Text;
  Finally
    Status.Free;
  End;
end;

{ TConsumerThread }

constructor TConsumerThread.Create(AQueue: TAMQPMessageQueue);
begin
  FQueue := AQueue;
  inherited Create;
end;

procedure TConsumerThread.Execute;
var
  Msg: TAMQPMessage;
begin
  WriteLine( 'Thread starting', False );
  NameThreadForDebugging( 'ConsumerThread' );
  Repeat
    Msg := FQueue.Get;
    if Msg = nil then
      Terminate;
    if not Terminated then
    Begin
      WriteLine( 'Thread message: ' + Msg.Body.AsString[ TEncoding.ASCII ], False );
      Msg.Ack;
      Msg.Free;
    End;
  Until Terminated;
  FQueue.Free;
  WriteLine( 'Thread stopped', True );
end;

procedure TConsumerThread.WriteLine( Text: String; Sync: Boolean );
begin
  if Sync then
    Synchronize( Procedure
                 Begin
                   TestbenchForm.MemoMessages.Lines.Add( Text );
                 End )
  else
    Queue( Procedure
           Begin
             TestbenchForm.MemoMessages.Lines.Add( Text );
           End );
end;

end.
