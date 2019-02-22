unit AMQP.Workers;
{$IfDef FPC}
{$mode delphi}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, AMQP.Classes, AMQP.Interfaces;

type

  { TAMQPWorker }

  TAMQPWorker<T> = class(TThread)
  private
    FInQueue: TBlockingQueue<T>;
    FChannel: IAMQPChannel;
  protected
    function IsMsgNull(AMsg: T): boolean; virtual; abstract;
    procedure DoMessage(AMsg: T); virtual; abstract;
    procedure Execute; override;
    procedure BeforeConstruction; virtual;
  public
    constructor Create(AInQueue: TBlockingQueue<T>; AChannel: IAMQPChannel = nil); reintroduce; virtual;
    property Channel: IAMQPChannel read FChannel;
    property InQueue: TBlockingQueue<T> read FInQueue;
  end;

implementation

{ TAMQPWorker }

constructor TAMQPWorker<T>.Create(AInQueue: TBlockingQueue<T>; AChannel: IAMQPChannel = nil);
begin
 FInQueue := AInQueue;
 FChannel := AChannel;
 BeforeConstruction;
 inherited Create(False);
end;

procedure TAMQPWorker<T>.BeforeConstruction;
begin

end;

procedure TAMQPWorker<T>.Execute;
var Msg: T;
begin
  repeat
    Sleep(5);
    while FInQueue.Count > 0 do
    begin
     Msg := FInQueue.Get();
     if IsMsgNull(Msg) then
     begin
      Terminate;
      Break;
     end;
     DoMessage(Msg);
    end;
  until Terminated;
end;

end.

