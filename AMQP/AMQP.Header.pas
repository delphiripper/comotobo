unit AMQP.Header;

interface

Uses
  System.Classes, AMQP.Payload, AMQP.MessageProperties;

Type
  TAMQPHeader = Class(TAMQPPayload)
  strict private
    FClassID      : UInt16;
    FWeight       : UInt16;
    FBodySize     : UInt64;
    FPropertyList : TAMQPMessageProperties;
  Public
    Property ClassID      : UInt16                 read FClassID;
    Property Weight       : UInt16                 read FWeight;
    Property BodySize     : UInt64                 read FBodySize;
    Property PropertyList : TAMQPMessageProperties read FPropertyList;
    Procedure Assign( AHeader: TAMQPHeader );
    Procedure LoadFromStream( AStream: TStream ); Override;
    Constructor Create; Override;
    Destructor Destroy; Override;
  End;

implementation

Uses
  AMQP.StreamHelper;

{ TAMQPHeader }

procedure TAMQPHeader.Assign(AHeader: TAMQPHeader);
begin
  FClassID       := AHeader.ClassID;
  FWeight        := AHeader.Weight;
  FBodySize      := AHeader.BodySize;
  FPropertyList.Assign( AHeader.PropertyList );
end;

constructor TAMQPHeader.Create;
begin
  inherited;
  Name           := 'Header';
  FClassID       := 0;
  FWeight        := 0;
  FBodySize      := 0;
  FPropertyList  := TAMQPMessageProperties.Create;
end;

destructor TAMQPHeader.Destroy;
begin
  FPropertyList.Free;
  inherited;
end;

procedure TAMQPHeader.LoadFromStream(AStream: TStream);
begin
  AStream.ReadUInt16( FClassID );
  AStream.ReadUInt16( FWeight );
  AStream.ReadUInt64( FBodySize );
  FPropertyList.LoadFromStream( AStream );
end;

end.
