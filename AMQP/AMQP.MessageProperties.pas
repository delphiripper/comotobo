unit AMQP.MessageProperties;

interface

Uses
  System.Classes, AMQP.Types;

Type
  TAMQPMessageProperties = class
  Strict Private
    FContentType        : TShortString;    //MIME content type, e.g. 'application/json'
    FContentEncoding    : TShortString;    //MIME content encoding, e.g. 'utf8'
    FApplicationHeaders : TFieldTable;     //message header field table
    FDeliveryMode       : TShortShortUInt; //nonpersistent (1) or persistent (2)
    FPriority           : TShortShortUInt; //message priority, 0 to 9
    FCorrelationID      : TShortString;    //application correlation identifier
    FReplyTo            : TShortString;    //address to reply to
    FExpiration         : TShortString;    //message expiration specification
    FMessageID          : TShortString;    //application message identifier
    FTimestamp          : TTimestamp;      //message timestamp
    FType               : TShortString;    //message type name
    FUserID             : TShortString;    //creating user id
    FAppID              : TShortString;    //creating application id
    FReserved           : TShortString;    //reserved, must be empty
  Public
    Property ContentType        : TShortString    read FContentType;
    Property ContentEncoding    : TShortString    read FContentEncoding;
    Property ApplicationHeaders : TFieldTable     read FApplicationHeaders;
    Property DeliveryMode       : TShortShortUInt read FDeliveryMode;
    Property Priority           : TShortShortUInt read FPriority;
    Property CorrelationID      : TShortString    read FCorrelationID;
    Property ReplyTo            : TShortString    read FReplyTo;
    Property Expiration         : TShortString    read FExpiration;
    Property MessageID          : TShortString    read FMessageID;
    Property Timestamp          : TTimestamp      read FTimestamp;
    Property &Type              : TShortString    read FType;
    Property UserID             : TShortString    read FUserID;
    Property AppID              : TShortString    read FAppID;
    Property Reserved           : TShortString    read FReserved; //ClusterID ?
    Procedure SaveToStream( AStream: TStream );
    Procedure LoadFromStream( AStream: TStream );
    Procedure Assign( AMessageProperties: TAMQPMessageProperties );
    Constructor Create;
    Destructor Destroy; Override;
  end;

implementation

Uses
  AMQP.StreamHelper;

{ TAMQPMessageProperties }

procedure TAMQPMessageProperties.Assign(AMessageProperties: TAMQPMessageProperties);
begin
  FContentType.Value     := AMessageProperties.ContentType.Value;
  FContentEncoding.Value := AMessageProperties.ContentEncoding.Value;
  FApplicationHeaders.Assign( AMessageProperties.ApplicationHeaders );
  FDeliveryMode.Value    := AMessageProperties.DeliveryMode.Value;
  FPriority.Value        := AMessageProperties.Priority.Value;
  FCorrelationID.Value   := AMessageProperties.CorrelationID.Value;
  FReplyTo.Value         := AMessageProperties.ReplyTo.Value;
  FExpiration.Value      := AMessageProperties.Expiration.Value;
  FMessageID.Value       := AMessageProperties.MessageID.Value;
  FTimestamp.Value       := AMessageProperties.Timestamp.Value;
  FType.Value            := AMessageProperties.&Type.Value;
  FUserID.Value          := AMessageProperties.UserID.Value;
  FAppID.Value           := AMessageProperties.AppID.Value;
  FReserved.Value        := AMessageProperties.Reserved.Value;
end;

constructor TAMQPMessageProperties.Create;
begin
  FContentType        := TShortString.Create( 'text/plain' );
  FContentEncoding    := TShortString.Create( 'utf8' );
  FApplicationHeaders := TFieldTable.Create;
  FDeliveryMode       := TShortShortUInt.Create( 2 );
  FPriority           := TShortShortUInt.Create( 1 );
  FCorrelationID      := TShortString.Create( '' );
  FReplyTo            := TShortString.Create( '' );
  FExpiration         := TShortString.Create( '' );
  FMessageID          := TShortString.Create( '' );
  FTimestamp          := TLongLongUInt.Create( 0 );
  FType               := TShortString.Create( '' );
  FUserID             := TShortString.Create( '' );
  FAppID              := TShortString.Create( 'Delphi' );
  FReserved           := TShortString.Create( '' );
end;

destructor TAMQPMessageProperties.Destroy;
begin
  FContentType.Free;
  FContentEncoding.Free;
  FApplicationHeaders.Free;
  FDeliveryMode.Free;
  FPriority.Free;
  FCorrelationID.Free;
  FReplyTo.Free;
  FExpiration.Free;
  FMessageID.Free;
  FTimestamp.Free;
  FType.Free;
  FUserID.Free;
  FAppID.Free;
  FReserved.Free;
  inherited;
end;

procedure TAMQPMessageProperties.LoadFromStream(AStream: TStream);
var
  Flags: UInt16;
begin
  AStream.ReadUInt16( Flags );
  if Flags and $8000 = $8000 then FContentType.LoadFromStream( AStream );
  if Flags and $4000 = $4000 then FContentEncoding.LoadFromStream( AStream );
  if Flags and $2000 = $2000 then FApplicationHeaders.LoadFromStream( AStream );
  if Flags and $1000 = $1000 then FDeliveryMode.LoadFromStream( AStream );
  if Flags and $0800 = $0800 then FPriority.LoadFromStream( AStream );
  if Flags and $0400 = $0400 then FCorrelationID.LoadFromStream( AStream );
  if Flags and $0200 = $0200 then FReplyTo.LoadFromStream( AStream );
  if Flags and $0100 = $0100 then FExpiration.LoadFromStream( AStream );
  if Flags and $0080 = $0080 then FMessageID.LoadFromStream( AStream );
  if Flags and $0040 = $0040 then FTimestamp.LoadFromStream( AStream );
  if Flags and $0020 = $0020 then FType.LoadFromStream( AStream );
  if Flags and $0010 = $0010 then FUserID.LoadFromStream( AStream );
  if Flags and $0008 = $0008 then FAppID.LoadFromStream( AStream );
  if Flags and $0004 = $0004 then FReserved.LoadFromStream( AStream );
end;

procedure TAMQPMessageProperties.SaveToStream(AStream: TStream);
var
  PropertyFlags: UInt16;
  Data: TMemoryStream;

  Procedure SaveField( ASaveField: Boolean; ABit: Byte; AValue: TAMQPValue );
  Begin
    if ASaveField then
    Begin
      PropertyFlags := PropertyFlags + 1 shl ABit;
      AValue.SaveToStream( Data );
    End;
  End;

begin
  PropertyFlags := 0;
  Data := TMemoryStream.Create;
  Try
    SaveField( ContentType.Value        <> '', 15, ContentType );
    SaveField( ContentEncoding.Value    <> '', 14, ContentEncoding );
    SaveField( ApplicationHeaders.Count <>  0, 13, ApplicationHeaders );
    SaveField( DeliveryMode.Value       <>  0, 12, DeliveryMode );
    SaveField( Priority.Value           <>  0, 11, Priority );
    SaveField( CorrelationID.Value      <> '', 10, CorrelationID );
    SaveField( ReplyTo.Value            <> '',  9, ReplyTo );
    SaveField( Expiration.Value         <> '',  8, Expiration );
    SaveField( MessageID.Value          <> '',  7, MessageID );
    SaveField( Timestamp.Value          <>  0,  6, Timestamp );
    SaveField( FType.Value              <> '',  5, FType );
    SaveField( UserID.Value             <> '',  4, UserID );
    SaveField( AppID.Value              <> '',  3, AppID );
    SaveField( Reserved.Value           <> '',  2, Reserved );

    AStream.WriteUInt16( PropertyFlags );
    AStream.CopyFrom( Data, 0 );
  Finally
    Data.Free;
  End;
end;

end.
