unit AMQP.Arguments;

interface

Const
  AMQP_TTL_SECOND = 1000;
  AMQP_TTL_MINUTE = AMQP_TTL_SECOND * 60;
  AMQP_TTL_HOUR   = AMQP_TTL_MINUTE * 60;
  AMQP_TTL_DAY    = AMQP_TTL_HOUR   * 24;

Type
  TArgument = Record
    Name: String;
    Value: Variant;
  End;

  TArguments = Array of TArgument;

  TArgumentHelper = Record Helper for TArguments
    Function Add( Name: String; Value: Variant ): TArguments;
    Function SetMessageTTL( TimeToLiveMS: Int64 ): TArguments;
  End;

Function MakeArguments: TArguments; overload;
Function MakeArguments( Name: String; Value: Variant ): TArguments; overload;

implementation

Function MakeArguments( Name: String; Value: Variant ): TArguments;
var
  Arg: TArgument;
Begin
  Arg.Name  := Name;
  Arg.Value := Value;
  Result := [ Arg ];
End;

Function MakeArguments: TArguments;
Begin
  Result := [];
End;

{ TArgumentHelper }

function TArgumentHelper.Add(Name: String; Value: Variant): TArguments;
begin
  Result := self + MakeArguments( Name, Value );
end;

function TArgumentHelper.SetMessageTTL(TimeToLiveMS: Int64): TArguments;
begin
  Result := MakeArguments( 'x-message-ttl', TimeToLiveMS );
end;

end.
