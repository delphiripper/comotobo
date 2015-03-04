unit TestObjects;

interface

Uses
  System.Generics.Collections, System.Classes;

Type
  TEnum = (First, Middle, Last);

  TSimple = Class
  private
    FInt: Integer;
    FC: Char;
    FDay: TDate;
    FStr: String;
    FTimestamp: TDateTime;
    FNumber: Double;
    FBool: Boolean;
    FInt64: Int64;
    FWord: Word;
    FByte: Byte;
    FEnum: TEnum;
  Public
    Property Str: String read FStr write FStr;
    Property C: Char read FC write FC;
    Property Int: Integer read FInt write FInt;
    Property Bool: Boolean read FBool write FBool;
    Property Number: Double read FNumber write FNumber;
    Property Day: TDate read FDay write FDay;
    Property Timestamp: TDateTime read FTimestamp write FTimestamp;
    Property Enum: TEnum read FEnum write FEnum;
    Property AByte: Byte read FByte write FByte;
    Property AWord: Word read FWord write FWord;
    Property AInt64: Int64 read FInt64 write FInt64;
    Constructor Create;
  End;

  TSimpleLists = Class
  private
    FInt       : TList<Integer>;
    FC         : TList<Char>;
    FDay       : TList<TDate>;
    FStr       : TList<String>;
    FTimestamp : TList<TDateTime>;
    FNumber    : TList<Double>;
    FBool      : TList<Boolean>;
    FStrList   : TStringList;
  Public
    Property Str       : TList<String>    read FStr;
    Property StrList   : TStringList      read FStrList;
    Property C         : TList<Char>      read FC;
    Property Int       : TList<Integer>   read FInt;
    Property Bool      : TList<Boolean>   read FBool;
    Property Number    : TList<Double>    read FNumber;
    Property Day       : TList<TDate>     read FDay;
    Property Timestamp : TList<TDateTime> read FTimestamp;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  TSimpleContainer = Class
  private
    FName: String;
    FObjects: TObjectList<TSimple>;
  Public
    Property Name: String read FName write FName;
    Property Objects: TObjectList<TSimple> read FObjects write FObjects;
    Constructor Create;
    Destructor Destroy; Override;
  End;

implementation

Uses
  System.SysUtils, System.DateUtils;

{ TSimple }

constructor TSimple.Create;
begin
  FInt := 0;
  FC := 'A';
  FDay := EncodeDate( 2014, 1, 1 );
  FTimestamp := EncodeDateTime( 2014, 1, 1, 0, 0, 0, 0 );
  FStr := '';
  FNumber := 0;
  FBool := False;
  FInt64 := 0;
  FWord := 0;
  FByte := 0;
  FEnum := First;
end;

{ TSimpleLists }

constructor TSimpleLists.Create;
begin
  FInt       := TList<Integer>.Create;
  FC         := TList<Char>.Create;
  FDay       := TList<TDate>.Create;
  FStr       := TList<String>.Create;
  FStrList   := TStringList.Create;
  FTimestamp := TList<TDateTime>.Create;
  FNumber    := TList<Double>.Create;
  FBool      := TList<Boolean>.Create;
end;

destructor TSimpleLists.Destroy;
begin
  FInt.Free;
  FC.Free;
  FDay.Free;
  FStr.Free;
  FStrList.Free;
  FTimestamp.Free;
  FNumber.Free;
  FBool.Free;
  inherited;
end;

{ TSimpleContainer }

constructor TSimpleContainer.Create;
begin
  FName := '';
  FObjects := TObjectList<TSimple>.Create;
end;

destructor TSimpleContainer.Destroy;
begin
  FObjects.Free;
  inherited;
end;

end.
