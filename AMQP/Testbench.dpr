program Testbench;

uses
  Vcl.Forms,
  TestbenchWin in 'TestbenchWin.pas' {TestbenchForm},
  AMQP.Method in 'AMQP.Method.pas',
  AMQP.Types in 'AMQP.Types.pas',
  AMQP.Message in 'AMQP.Message.pas',
  AMQP.Protocol in 'AMQP.Protocol.pas',
  AMQP.Connection in 'AMQP.Connection.pas',
  AMQP.Frame in 'AMQP.Frame.pas',
  AMQP.Helper in 'AMQP.Helper.pas',
  AMQP.Payload in 'AMQP.Payload.pas',
  AMQP.MessageProperties in 'AMQP.MessageProperties.pas',
  AMQP.Header in 'AMQP.Header.pas',
  AMQP.StreamHelper in 'AMQP.StreamHelper.pas',
  AMQP.Channel in 'AMQP.Channel.pas',
  AMQP.Classes in 'AMQP.Classes.pas',
  AMQP.Interfaces in 'AMQP.Interfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTestbenchForm, TestbenchForm);
  Application.Run;
end.
