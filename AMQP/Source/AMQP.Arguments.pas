unit AMQP.Arguments;

interface

Type
  TArgument = Record
    Name: String;
    Value: Variant;
  End;

  TArguments = Array of TArgument;

  TArgumentHelper = Record Helper for TArguments
    Function Add( Name: String; Value: Variant ): TArguments;
    Function SetMessageTTL( TimeToLive: UInt32 ): TArguments;
  End;

Function Arguments: TArguments;
Function MakeArguments( Name: String; Value: Variant ): TArguments;
//Function Arguments( Name: String; Value: Variant ): TArguments;
//Arguments( 'x-message-ttl', 30000 )

implementation

//Function Arguments( Name: String; Value: Variant ): TArguments;
//var
//  Arg: TArgument;
//Begin
//  Arg.Name  := Name;
//  Arg.Value := Value;
//  Result := [ Arg ];
//End;

Function MakeArguments( Name: String; Value: Variant ): TArguments;
var
  Arg: TArgument;
Begin
  Arg.Name  := Name;
  Arg.Value := Value;
  Result := [ Arg ];
End;

Function Arguments: TArguments;
Begin
  Result := [];
End;

{ TArgumentHelper }

function TArgumentHelper.Add(Name: String; Value: Variant): TArguments;
begin
  Result := self + MakeArguments( Name, Value );
  //Copy( self, 1, Length(Self) );
end;

function TArgumentHelper.SetMessageTTL(TimeToLive: UInt32): TArguments;
begin
  Result := MakeArguments( 'x-message-ttl', 30000 );
end;

begin
//  Args := Arguments( 'Test', 33 ).Add( 'x-ttl', 600 );
end.
