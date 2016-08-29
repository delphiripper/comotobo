unit AMQP.StreamHelper;

interface

Uses
  System.SysUtils, System.Classes, IdGlobal;

Type
  TStreamHelper = Class helper for TStream
  Private
    function GetAsString(AEncoding: TEncoding): String;
    procedure SetAsString(AEncoding: TEncoding; const Value: String);
  Public
    Procedure WriteMSB( var Buffer; Count: Cardinal );
    Procedure WriteOctet( B: Byte );
    Procedure WriteUInt8( I: UInt8 );
    Procedure WriteUInt16( I: UInt16 );
    Procedure WriteUInt32( I: UInt32 );
    Procedure WriteUInt64( I: UInt64 );
    Procedure WriteInt8( I: Int8 );
    Procedure WriteInt16( I: Int16 );
    Procedure WriteInt32( I: Int32 );
    Procedure WriteInt64( I: Int64 );
    Procedure WriteShortStr( S: String );
    Procedure ReadMSB( var Buffer; Count: Integer );
    Procedure ReadOctet( var B: Byte );
    Procedure ReadUInt8( var I: UInt8 );
    Procedure ReadUInt16( var I: UInt16 );
    Procedure ReadUInt32( var I: UInt32 );
    Procedure ReadUInt64( var I: UInt64 );
    Procedure ReadInt8( var I: Int8 );
    Procedure ReadInt16( var I: Int16 );
    Procedure ReadInt32( var I: Int32 );
    Procedure ReadInt64( var I: Int64 );
    Procedure ReadShortStr( var S: String );
    Procedure ReadLongStr( var S: String );
    Function AsBytes: TIdBytes;
    Property AsString[ Encoding: TEncoding ]: String read GetAsString write SetAsString;
  End;

implementation

{ TStreamHelper }

function TStreamHelper.AsBytes: TIdBytes;
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  Position := 0;
  SetLength( Result, Size );
  ReadBuffer( Result[0], Size );
  Position := OldPosition;
end;

function TStreamHelper.GetAsString(AEncoding: TEncoding): String;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create( '', AEncoding );
  Try
    StringStream.CopyFrom( Self, -1 );
    Result := StringStream.DataString;
  Finally
    StringStream.Free;
  End;
end;

procedure TStreamHelper.ReadInt16(var I: Int16);
begin
  ReadMSB( I, 2 );
end;

procedure TStreamHelper.ReadInt32(var I: Int32);
begin
  ReadMSB( I, 4 );
end;

procedure TStreamHelper.ReadInt64(var I: Int64);
begin
  ReadMSB( I, 8 );
end;

procedure TStreamHelper.ReadInt8(var I: Int8);
begin
  Read( I, 1 );
end;

procedure TStreamHelper.ReadLongStr(var S: String);
var
  Str: AnsiString;
  Len: UInt32;
begin
  ReadUInt32( Len );
  SetLength( Str, Len );
  if Len > 0 then
    Read( Str[1], Len );
  S := String( Str );
end;

Procedure TStreamHelper.ReadMSB(var Buffer; Count: Integer);
var
  Cnt: Integer;
begin
  For Cnt := 0 to Count-1 do
    Read( PByteArray(@Buffer)[Count - Cnt - 1], 1 );
end;

procedure TStreamHelper.ReadOctet(var B: Byte);
begin
  Read( B, 1 );
end;

procedure TStreamHelper.ReadShortStr(var S: String);
var
  Str: AnsiString;
  Len: Byte;
begin
  ReadUInt8( Len );
  SetLength( Str, Len );
  if Len > 0 then
    Read( Str[1], Len );
  S := String( Str );
end;

procedure TStreamHelper.ReadUInt16(var I: UInt16);
begin
  ReadMSB( I, 2 );
end;

procedure TStreamHelper.ReadUInt32(var I: UInt32);
begin
  ReadMSB( I, 4 );
end;

procedure TStreamHelper.ReadUInt64(var I: UInt64);
begin
  ReadMSB( I, 8 );
end;

procedure TStreamHelper.ReadUInt8(var I: UInt8);
begin
  Read( I, 1 );
end;

procedure TStreamHelper.SetAsString(AEncoding: TEncoding; const Value: String);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create( Value, AEncoding );
  Try
    Self.CopyFrom( StringStream, -1 );
  Finally
    StringStream.Free;
  End;
end;

procedure TStreamHelper.WriteInt16(I: Int16);
begin
  WriteMSB( I, 2 );
end;

procedure TStreamHelper.WriteInt32(I: Int32);
begin
  WriteMSB( I, 4 );
end;

procedure TStreamHelper.WriteInt64(I: Int64);
begin
  WriteMSB( I, 8 );
end;

procedure TStreamHelper.WriteInt8(I: Int8);
begin
  WriteMSB( I, 1 );
end;

procedure TStreamHelper.WriteMSB(var Buffer; Count: Cardinal);
var
  Index: Integer;
begin
  For Index := Count-1 downto 0 do
    WriteData( PByteArray(@Buffer)[Index], 1 );
end;

procedure TStreamHelper.WriteOctet(B: Byte);
begin
  WriteMSB( B, 1 );
end;

procedure TStreamHelper.WriteShortStr(S: String);
var
  Char: AnsiChar;
  Str: AnsiString;
  B: Byte;
begin
  B   := S.Length;
  Str := AnsiString( S );
  WriteData( B );
  for Char in Str do
    WriteData( Char );
end;

procedure TStreamHelper.WriteUInt64(I: UInt64);
begin
  WriteMSB( I, 8 );
end;

procedure TStreamHelper.WriteUInt16(I: UInt16);
begin
  WriteMSB( I, 2 );
end;

procedure TStreamHelper.WriteUInt32(I: UInt32);
begin
  WriteMSB( I, 4 );
end;

procedure TStreamHelper.WriteUInt8(I: UInt8);
begin
  WriteMSB( I, 1 );
end;

end.
