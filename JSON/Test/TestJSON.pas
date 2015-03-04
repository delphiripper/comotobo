unit TestJSON;

interface

uses
  TestFramework, System.SysUtils, System.Generics.Collections, System.Rtti, JSON, TestObjects;

type
  TestTJSONElements = class(TTestCase)
  strict private
    FJSON: TJSON_Element;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNull;
    procedure TestNumber;
    procedure TestNumberAsInteger;
    procedure TestString;
    procedure TestBoolean;
    procedure TestArrayEmpty;
    procedure TestArrayOfBoolean;
    procedure TestArrayOfInteger;
    procedure TestArrayOfFloat;
    procedure TestArrayOfString;
    procedure TestArrayOfObject;
    procedure TestObject;
    procedure TestPair;
  end;

  TestTJSON = class(TTestCase)
  strict private
    FJSON: TJSON_Element;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseTextNull;
    procedure TestParseTextNumber;
    procedure TestParseTextNumberAsInteger;
    procedure TestParseTextString;
    procedure TestParseTextBooleanTrue;
    procedure TestParseTextBooleanFalse;
    procedure TestParseTextArrayOfString;
    procedure TestParseTextObject;
  end;

  //JSON -> Object
  TestTJSONParser = class(TTestCase)
  public
    procedure CheckSimpleObject( Obj: TSimple; Index: Integer );
    procedure CheckSimpleListObject( Obj: TSimpleLists );
  published
    procedure TestParseObject;
    procedure TestParseObjectInArray;
    procedure TestParseArrayOfObjects;
    procedure TestParseLists;
    procedure TestParseContainer;
    procedure TestParseArrayOfContainer;
  end;

  //Object -> JSON
  TestTJSONSerializer = class(TTestCase)
  public
    Function MakeSimpleObject( Index: Integer ): TSimple;
    Function MakeSimpleListsObject: TSimpleLists;
    Function MakeSimpleContainerObject( Index: Integer ): TSimpleContainer;
  published
    procedure TestToJSON;
    //These tests also (indirectly) tests ToJSON():
    procedure TestSerializeSimple;
    procedure TestSerializeListOfObjects;
    procedure TestSerializeLists;
    procedure TestSerializeContainer;
    procedure TestSerializeListOfContainer;
  end;

Function StripWhiteSpace( S: String ): String;

implementation

Uses
  System.DateUtils;

Const
  SimpleJSON_1 = '{"Str":"Hello","C":"T","Int":88,"Bool":true,"Number":5.125,"Day":"2014-08-05","Timestamp":"2014-08-05T11:28:12","Enum":"Middle","AByte":255,"AWord":65535,"AInt64":10200300400}';
  SimpleJSON_2 = '{"Str":"World","C":"Y","Int":99,"Bool":false,"Number":15.125,"Day":"2014-09-05","Timestamp":"2014-08-05T17:28:12","Enum":"Last","AByte":128,"AWord":32768,"AInt64":900200300400}';
  SimpleContainerJSON_1 = '{"Name":"Roberto","Objects":[' + SimpleJSON_2 + ',' + SimpleJSON_1 + ']}';
  SimpleContainerJSON_2 = '{"Name":"Benito","Objects":[' + SimpleJSON_1 + ']}';
  SimpleListJSON = '{"Str":["Hello","World","Jimmy"],' +
                   '"StrList":["Hallo","Welt","Jens"],' +
                   '"C":["H","A","T"],'+
                   '"Int":[88,99,100],'+
                   '"Bool":[true,true,false],'+
                   '"Number":[5.125,10,250],'+
                   '"Day":["2014-08-05","2014-08-07","2014-08-09"],'+
                   '"Timestamp":["2014-08-05T11:00:00","2014-08-07T11:15:00","2014-08-09T11:30:00"]}';
  ArrayOfSimpleContainerJSON = '[' + SimpleContainerJSON_1 + ',' + SimpleContainerJSON_2 + ']';

Function StripWhiteSpace( S: String ): String;

  Procedure StripChar( C: char );
  Begin
    while Pos( C, Result ) > 0 do
      Delete( Result, Pos( C, Result), 1 );
  End;

Begin
  Result := S;
  StripChar( CH_LF );
  StripChar( CH_CR );
  StripChar( CH_SPACE );
  StripChar( CH_TAB );
End;

{ TestTJSONElements }

procedure TestTJSONElements.SetUp;
begin
  FJSON := nil;
end;

procedure TestTJSONElements.TearDown;
begin
  FJSON.Free;
  FJSON := nil;
end;

procedure TestTJSONElements.TestArrayEmpty;
begin
  FJSON := TJSON_Array.Create;
  CheckEquals( '[ ]', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestArrayOfBoolean;
begin
  FJSON := TJSON_Array.Create;
  TJSON_Array(FJSON).Add( TJSON_Boolean.Create( True ) );
  TJSON_Array(FJSON).Add( TJSON_Boolean.Create( False ) );
  TJSON_Array(FJSON).Add( TJSON_Boolean.Create( True ) );
  CheckEquals( '[ true, false, true ]', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestArrayOfFloat;
begin
  FJSON := TJSON_Array.Create;
  TJSON_Array(FJSON).Add( TJSON_Number.Create( 1.25 ) );
  TJSON_Array(FJSON).Add( TJSON_Number.Create( 10.50 ) );
  TJSON_Array(FJSON).Add( TJSON_Number.Create( 100.75 ) );
  CheckEquals( '[ 1.25, 10.5, 100.75 ]', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestArrayOfInteger;
begin
  FJSON := TJSON_Array.Create;
  TJSON_Array(FJSON).Add( TJSON_Number.CreateInteger( 1 ) );
  TJSON_Array(FJSON).Add( TJSON_Number.CreateInteger( 10 ) );
  TJSON_Array(FJSON).Add( TJSON_Number.CreateInteger( 100 ) );
  CheckEquals( '[ 1, 10, 100 ]', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestArrayOfObject;

  Procedure AddObject( Name, Value: String );
  var
    O: TJSON_Object;
  Begin
    O := TJSON_Object.Create;
    O.Add( TJSON_Pair.Create( Name, TJSON_String.Create( Value ) ) );
    TJSON_Array(FJSON).Add( O );
  End;

begin
  FJSON := TJSON_Array.Create;
  AddObject( 'Name', 'Michael' );
  AddObject( 'Navn', 'Jakob' );
  AddObject( 'Nombre', 'Miguel' );
  CheckEquals( '[{"Name":"Michael"},{"Navn":"Jakob"},{"Nombre":"Miguel"}]', StripWhiteSpace( FJSON.ToJSON ) );
end;

procedure TestTJSONElements.TestArrayOfString;
begin
  FJSON := TJSON_Array.Create;
  TJSON_Array(FJSON).Add( TJSON_String.Create( 'First' ) );
  TJSON_Array(FJSON).Add( TJSON_String.Create( 'Middle' ) );
  TJSON_Array(FJSON).Add( TJSON_String.Create( 'Last' ) );
  CheckEquals( '[ "First", "Middle", "Last" ]', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestBoolean;
begin
  FJSON := TJSON_Boolean.Create( True );
  CheckEquals( 'true', FJSON.ToJSON );

  TJSON_Boolean(FJSON).Value := False;
  CheckEquals( 'false', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestNull;
begin
  FJSON := TJSON_Null.Create;
  CheckEquals( 'null', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestNumber;
begin
  FJSON := TJSON_Number.Create( 100.25 );
  CheckEquals( '100.25', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestNumberAsInteger;
begin
  FJSON := TJSON_Number.CreateInteger( 125 );
  CheckEquals( '125', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestObject;
begin
  FJSON := TJSON_Object.Create;
  TJSON_Object(FJSON).Add( TJSON_Pair.Create( 'Name',   TJSON_String.Create( 'Jesper' ) ) );
  TJSON_Object(FJSON).Add( TJSON_Pair.Create( 'Age',    TJSON_Number.CreateInteger( 36 ) ) );
  TJSON_Object(FJSON).Add( TJSON_Pair.Create( 'Height', TJSON_Number.Create( 186.5 ) ) );
  TJSON_Object(FJSON).Add( TJSON_Pair.Create( 'Male',   TJSON_Boolean.Create( true ) ) );
  CheckEquals( '{"Name":"Jesper","Age":36,"Height":186.5,"Male":true}', StripWhiteSpace( FJSON.ToJSON ) );
end;

procedure TestTJSONElements.TestPair;
begin
  FJSON := TJSON_Pair.Create( 'abc', TJSON_String.Create( 'Remote' ) );
  CheckEquals( '"abc": "Remote"', FJSON.ToJSON );
end;

procedure TestTJSONElements.TestString;
begin
  FJSON := TJSON_String.Create( 'abc' );
  CheckEquals( '"abc"', FJSON.ToJSON );

  TJSON_String(FJSON).Value := 'line1'#13#10'line2';
  CheckEquals( '"line1\r\nline2"', FJSON.ToJSON );

  TJSON_String(FJSON).Value := 'line1\line2';
  CheckEquals( '"line1\\line2"', FJSON.ToJSON );
end;

{ TestTJSON }

procedure TestTJSON.SetUp;
begin
  FJSON := nil;
end;

procedure TestTJSON.TearDown;
begin
  FJSON.Free;
  FJSON := nil;
end;

procedure TestTJSON.TestParseTextArrayOfString;
begin
  FJSON := TJSON.ParseText( '["One", "Other"]' );
  CheckIs( FJSON, TJSON_Array );
  CheckEquals( 2, TJSON_Array(FJSON).Count );
  CheckIs( TJSON_Array(FJSON).Elements[0], TJSON_String );
  CheckIs( TJSON_Array(FJSON).Elements[1], TJSON_String );
  CheckEquals( 'One', TJSON_String( TJSON_Array(FJSON).Elements[0] ).Value );
  CheckEquals( 'Other', TJSON_String( TJSON_Array(FJSON).Elements[1] ).Value );
end;

procedure TestTJSON.TestParseTextBooleanFalse;
begin
  FJSON := TJSON.ParseText( 'false' );
  CheckIs( FJSON, TJSON_Boolean );
  CheckEquals( false, TJSON_Boolean(FJSON).Value );
end;

procedure TestTJSON.TestParseTextBooleanTrue;
begin
  FJSON := TJSON.ParseText( 'true' );
  CheckIs( FJSON, TJSON_Boolean );
  CheckEquals( true, TJSON_Boolean(FJSON).Value );
end;

procedure TestTJSON.TestParseTextNull;
begin
  FJSON := TJSON.ParseText( 'null' );
  CheckIs( FJSON, TJSON_Null );
end;

procedure TestTJSON.TestParseTextNumber;
begin
  FJSON := TJSON.ParseText( '127.25' );
  CheckIs( FJSON, TJSON_Number );
  CheckEquals( 127.25, TJSON_Number(FJSON).Value );
end;

procedure TestTJSON.TestParseTextNumberAsInteger;
begin
  FJSON := TJSON.ParseText( '127' );
  CheckIs( FJSON, TJSON_Number );
  CheckEquals( 127, TJSON_Number(FJSON).AsInteger );
end;

procedure TestTJSON.TestParseTextObject;
begin
  FJSON := TJSON.ParseText( '{"One": "Other","Two": 222}' );
  CheckIs( FJSON, TJSON_Object );
  CheckEquals( 2, TJSON_Object(FJSON).Count );
  CheckIs( TJSON_Object(FJSON).Pairs[0], TJSON_Pair );
  CheckIs( TJSON_Object(FJSON).Pairs[1], TJSON_Pair );
  CheckEquals( 'One',   TJSON_Pair( TJSON_Object(FJSON).Pairs[0] ).Name );
  CheckEquals( 'Two', TJSON_Pair( TJSON_Object(FJSON).Pairs[1] ).Name );
  CheckEquals( 'Other', TJSON_String( TJSON_Pair( TJSON_Object(FJSON).Pairs[0] ).Value ).Value );
  CheckEquals( 222,     TJSON_Number( TJSON_Pair( TJSON_Object(FJSON).Pairs[1] ).Value ).AsInteger );
end;

procedure TestTJSON.TestParseTextString;
begin
  FJSON := TJSON.ParseText( '"Peter\nLars"' );
  CheckIs( FJSON, TJSON_String );
  CheckEquals( 'Peter'#10'Lars', TJSON_String(FJSON).Value );
end;

{ TestTJSONParser }

procedure TestTJSONParser.CheckSimpleListObject(Obj: TSimpleLists);
begin
  CheckEquals( 3, Obj.Str.Count,       'TList<String>.Count mismatch' );
  CheckEquals( 3, Obj.StrList.Count,   'StringList.Count mismatch' );
  CheckEquals( 3, Obj.C.Count,         'TList<Char>.Count mismatch' );
  CheckEquals( 3, Obj.Int.Count,       'TList<Integer>.Count mismatch' );
  CheckEquals( 3, Obj.Bool.Count,      'TList<Boolean>.Count mismatch' );
  CheckEquals( 3, Obj.Number.Count,    'TList<Float>.Count mismatch' );
  CheckEquals( 3, Obj.Day.Count,       'TList<TDate>.Count mismatch' );
  CheckEquals( 3, Obj.Timestamp.Count, 'TList<TDateTime>.Count mismatch' );

  CheckEquals( 'Hello', Obj.Str[0], 'TList<String> mismatch' );
  CheckEquals( 'World', Obj.Str[1], 'TList<String> mismatch' );
  CheckEquals( 'Jimmy', Obj.Str[2], 'TList<String> mismatch' );

  CheckEquals( 'Hallo', Obj.StrList[0], 'StringList mismatch' );
  CheckEquals( 'Welt',  Obj.StrList[1], 'StringList mismatch' );
  CheckEquals( 'Jens',  Obj.StrList[2], 'StringList mismatch' );

  CheckEquals( 'H', Obj.C[0], 'TList<Char> mismatch' );
  CheckEquals( 'A', Obj.C[1], 'TList<Char> mismatch' );
  CheckEquals( 'T', Obj.C[2], 'TList<Char> mismatch' );

  CheckEquals( 88,  Obj.Int[0], 'TList<Integer> mismatch' );
  CheckEquals( 99,  Obj.Int[1], 'TList<Integer> mismatch' );
  CheckEquals( 100, Obj.Int[2], 'TList<Integer> mismatch' );

  CheckEquals( True,  Obj.Bool[0], 'TList<Boolean> mismatch' );
  CheckEquals( True,  Obj.Bool[1], 'TList<Boolean> mismatch' );
  CheckEquals( False, Obj.Bool[2], 'TList<Boolean> mismatch' );

  CheckEquals( 5.125, Obj.Number[0], 'TList<Float> mismatch' );
  CheckEquals(    10, Obj.Number[1], 'TList<Float> mismatch' );
  CheckEquals(   250, Obj.Number[2], 'TList<Float> mismatch' );

  CheckEquals( EncodeDate(2014,8,5), Obj.Day[0], 'TList<TDate> mismatch' );
  CheckEquals( EncodeDate(2014,8,7), Obj.Day[1], 'TList<TDate> mismatch' );
  CheckEquals( EncodeDate(2014,8,9), Obj.Day[2], 'TList<TDate> mismatch' );

  CheckEquals( EncodeDateTime(2014,8,5,11,00,0,0), Obj.Timestamp[0], 'TList<TDateTime> mismatch' );
  CheckEquals( EncodeDateTime(2014,8,7,11,15,0,0), Obj.Timestamp[1], 'TList<TDateTime> mismatch' );
  CheckEquals( EncodeDateTime(2014,8,9,11,30,0,0), Obj.Timestamp[2], 'TList<TDateTime> mismatch' );
end;

procedure TestTJSONParser.CheckSimpleObject(Obj: TSimple; Index: Integer);
begin
  if Index = 1 then
  Begin
    CheckEquals( 'Hello',                             Obj.Str,       'String mismatch' );
    CheckEquals( 'T',                                 Obj.C,         'Char mismatch' );
    CheckEquals( 88,                                  Obj.Int,       'Integer mismatch' );
    CheckEquals( True,                                Obj.Bool,      'Boolean mismatch' );
    CheckEquals( 5.125,                               Obj.Number,    'Float mismatch' );
    CheckEquals( EncodeDate(2014,8,5),                Obj.Day,       'TDate mismatch' );
    CheckEquals( EncodeDateTime(2014,8,5,11,28,12,0), Obj.Timestamp, 'TDateTime mismatch' );
    CheckEquals( 255,                                 Obj.AByte,     'Byte mismatch' );
    CheckEquals( 65535,                               Obj.AWord,     'Word mismatch' );
    CheckEquals( 10200300400,                         Obj.AInt64,    'Int64 mismatch' );
    CheckTrue(   Obj.Enum = Middle,                                  'Enum mismatch' );
  End
  Else if Index = 2 then
  Begin
    CheckEquals( 'World',                             Obj.Str,       'String mismatch' );
    CheckEquals( 'Y',                                 Obj.C,         'Char mismatch' );
    CheckEquals( 99,                                  Obj.Int,       'Integer mismatch' );
    CheckEquals( False,                               Obj.Bool,      'Boolean mismatch' );
    CheckEquals( 15.125,                              Obj.Number,    'Float mismatch' );
    CheckEquals( EncodeDate(2014,9,5),                Obj.Day,       'TDate mismatch' );
    CheckEquals( EncodeDateTime(2014,8,5,17,28,12,0), Obj.Timestamp, 'TDateTime mismatch' );
    CheckEquals( 128,                                 Obj.AByte,     'Byte mismatch' );
    CheckEquals( 32768,                               Obj.AWord,     'Word mismatch' );
    CheckEquals( 900200300400,                        Obj.AInt64,    'Int64 mismatch' );
    CheckTrue(   Obj.Enum = Last,                                    'Enum mismatch' );
  End
  Else
    CheckTrue( False, 'CheckSimpleObject: Index out of bounds' );
end;

procedure TestTJSONParser.TestParseArrayOfContainer;
var
  Obj: TObjectList<TSimpleContainer>;
begin
  Obj := TObjectList<TSimpleContainer>.Create;
  Try
    TJSONParser.Parse( ArrayOfSimpleContainerJSON, Obj );
    CheckEquals( 'Roberto', Obj[0].Name, 'Name differs' );
    CheckEquals( 2, Obj[0].Objects.Count, 'Obj.Objects.Count differs' );
    CheckSimpleObject( Obj[0].Objects[0], 2 );
    CheckSimpleObject( Obj[0].Objects[1], 1 );
    CheckEquals( 'Benito', Obj[1].Name, 'Name differs' );
    CheckEquals( 1, Obj[1].Objects.Count, 'Obj.Objects.Count differs' );
    CheckSimpleObject( Obj[1].Objects[0], 1 );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONParser.TestParseContainer;
var
  Obj: TSimpleContainer;
begin
  Obj := TSimpleContainer.Create;
  Try
    TJSONParser.Parse( SimpleContainerJSON_1, Obj );
    CheckEquals( 'Roberto', Obj.Name, 'Name differs' );
    CheckEquals( 2, Obj.Objects.Count, 'Obj.Objects.Count differs' );
    CheckSimpleObject( Obj.Objects[0], 2 );
    CheckSimpleObject( Obj.Objects[1], 1 );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONParser.TestParseArrayOfObjects;
var
  Obj: TObjectList<TSimple>;
begin
  Obj := TObjectList<TSimple>.Create;
  Try
    TJSONParser.Parse( '[ ' + SimpleJSON_1 + ', ' + SimpleJSON_2 + ' ]', Obj );
    CheckEquals( 2, Obj.Count, 'List.Count differs' );
    CheckSimpleObject( Obj[0], 1 );
    CheckSimpleObject( Obj[1], 2 );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONParser.TestParseLists;
var
  Obj: TSimpleLists;
  JSON: string;
begin
  JSON := SimpleListJSON;
  Obj := TSimpleLists.Create;
  Try
    TJSONParser.Parse(JSON, Obj);
    CheckSimpleListObject( Obj );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONParser.TestParseObject;
var
  Obj: TSimple;
begin
  Obj := TSimple.Create;
  Try
    TJSONParser.Parse( SimpleJSON_1, Obj );
    CheckSimpleObject( Obj, 1 );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONParser.TestParseObjectInArray;
var
  Obj: TSimple;
begin
  Obj := TSimple.Create;
  Try
    TJSONParser.Parse( '[' + SimpleJSON_1 + ']', Obj );
    CheckSimpleObject( Obj, 1 );
  Finally
    Obj.Free;
  End;
end;

{ TestTJSONSerializer }

procedure TestTJSONSerializer.TestToJSON;
var
  JSON: TJSON_Element;
  Obj: TSimple;
begin
  Obj := MakeSimpleObject( 1 );
  Try
    JSON := TJSONSerializer.ToJSON( Obj );

    CheckTrue( JSON.IsObject );
    CheckEquals( 11, JSON.AsObject.Count, 'JSON.AsObject.Count' );
    CheckTrue( JSON.AsObject.Pairs[0].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[1].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[2].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[3].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[4].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[5].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[6].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[7].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[8].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[9].IsPair, 'IsPair' );
    CheckTrue( JSON.AsObject.Pairs[10].IsPair, 'IsPair' );

    CheckEquals( 'Str',       JSON.AsObject.Pairs[0].AsPair.Name );
    CheckEquals( 'C',         JSON.AsObject.Pairs[1].AsPair.Name );
    CheckEquals( 'Int',       JSON.AsObject.Pairs[2].AsPair.Name );
    CheckEquals( 'Bool',      JSON.AsObject.Pairs[3].AsPair.Name );
    CheckEquals( 'Number',    JSON.AsObject.Pairs[4].AsPair.Name );
    CheckEquals( 'Day',       JSON.AsObject.Pairs[5].AsPair.Name );
    CheckEquals( 'Timestamp', JSON.AsObject.Pairs[6].AsPair.Name );
    CheckEquals( 'Enum',      JSON.AsObject.Pairs[7].AsPair.Name );
    CheckEquals( 'AByte',     JSON.AsObject.Pairs[8].AsPair.Name );
    CheckEquals( 'AWord',     JSON.AsObject.Pairs[9].AsPair.Name );
    CheckEquals( 'AInt64',    JSON.AsObject.Pairs[10].AsPair.Name );

    CheckTrue( JSON.AsObject.Pairs[0].AsPair.Value.IsString,  '0:IsString' );
    CheckTrue( JSON.AsObject.Pairs[1].AsPair.Value.IsString,  '1:IsString (Char)' );
    CheckTrue( JSON.AsObject.Pairs[2].AsPair.Value.IsNumber,  '2:IsNumber (Int)' );
    CheckTrue( JSON.AsObject.Pairs[3].AsPair.Value.IsBoolean, '3:IsBoolean' );
    CheckTrue( JSON.AsObject.Pairs[4].AsPair.Value.IsNumber,  '4:IsNumber (Float)' );
    CheckTrue( JSON.AsObject.Pairs[5].AsPair.Value.IsString,  '5:IsString (Date)' );
    CheckTrue( JSON.AsObject.Pairs[6].AsPair.Value.IsString,  '6:IsString (DateTime)' );
    CheckTrue( JSON.AsObject.Pairs[7].AsPair.Value.IsString,  '7:IsString' );
    CheckTrue( JSON.AsObject.Pairs[8].AsPair.Value.IsNumber,  '8:IsNumber (Byte)' );
    CheckTrue( JSON.AsObject.Pairs[9].AsPair.Value.IsNumber,  '9:IsNumber (Word)' );
    CheckTrue( JSON.AsObject.Pairs[10].AsPair.Value.IsNumber, '10:IsNumber (Int64)' );

    CheckEquals( 'Hello',               JSON.AsObject.Pairs[0].AsPair.Value.AsString.Value );
    CheckEquals( 'T',                   JSON.AsObject.Pairs[1].AsPair.Value.AsString.Value );
    CheckEquals( 88,                    JSON.AsObject.Pairs[2].AsPair.Value.AsNumber.Value );
    CheckEquals( True,                  JSON.AsObject.Pairs[3].AsPair.Value.AsBoolean.Value );
    CheckEquals( 5.125,                 JSON.AsObject.Pairs[4].AsPair.Value.AsNumber.Value, 0.000001 );
    CheckEquals( '2014-08-05',          JSON.AsObject.Pairs[5].AsPair.Value.AsString.Value );
    CheckEquals( '2014-08-05T11:28:12', JSON.AsObject.Pairs[6].AsPair.Value.AsString.Value );
    CheckEquals( 'Middle',              JSON.AsObject.Pairs[7].AsPair.Value.AsString.Value );
    CheckEquals( 255,                   JSON.AsObject.Pairs[8].AsPair.Value.AsNumber.AsInteger );
    CheckEquals( 65535,                 JSON.AsObject.Pairs[9].AsPair.Value.AsNumber.AsInteger );
    CheckEquals( 10200300400,           JSON.AsObject.Pairs[10].AsPair.Value.AsNumber.AsInt64 );
  Finally
    Obj.Free;
  End;
end;

function TestTJSONSerializer.MakeSimpleContainerObject( Index: Integer ): TSimpleContainer;
begin
  if Index = 1 then
  Begin
    Result := TSimpleContainer.Create;
    Result.Name := 'Roberto';
    Result.Objects.Add( MakeSimpleObject(2) );
    Result.Objects.Add( MakeSimpleObject(1) );
  End
  Else if Index = 2 then
  Begin
    Result := TSimpleContainer.Create;
    Result.Name := 'Benito';
    Result.Objects.Add( MakeSimpleObject(1) );
  End
  Else raise Exception.Create('Index out of bounds');
end;

function TestTJSONSerializer.MakeSimpleListsObject: TSimpleLists;
begin
  Result := TSimpleLists.Create;
  Result.Str.Add( 'Hello' );
  Result.Str.Add( 'World' );
  Result.Str.Add( 'Jimmy' );
  Result.StrList.CommaText := 'Hallo,Welt,Jens';
  Result.Int.AddRange( [88, 99, 100] );
  Result.Bool.AddRange( [true, true, false] );
  Result.Number.AddRange( [5.125, 10, 25e1] );
  Result.C.AddRange( ['H', 'A', 'T'] );
  Result.Day.AddRange( [ EncodeDate(2014,08,05), EncodeDate(2014,08,07), EncodeDate(2014,08,09) ] );
  Result.Timestamp.AddRange( [ EncodeDateTime(2014,08,05,11,00,00,00), EncodeDateTime(2014,08,07,11,15,00,00),
                               EncodeDateTime(2014,08,09,11,30,00,00) ] );
end;

function TestTJSONSerializer.MakeSimpleObject(Index: Integer): TSimple;
begin
  if Index = 1 then
  Begin
    Result := TSimple.Create;
    Result.Str       := 'Hello';
    Result.C         := 'T';
    Result.Int       := 88;
    Result.Bool      := True;
    Result.Number    := 5.125;
    Result.Day       := EncodeDate(2014,8,5);
    Result.Timestamp := EncodeDateTime(2014,8,5,11,28,12,0);
    Result.AByte     := 255;
    Result.AWord     := 65535;
    Result.AInt64    := 10200300400;
    Result.Enum      := Middle;
  End
  Else if Index = 2 then
  Begin
    Result := TSimple.Create;
    Result.Str       := 'World';
    Result.C         := 'Y';
    Result.Int       := 99;
    Result.Bool      := False;
    Result.Number    := 15.125;
    Result.Day       := EncodeDate(2014,9,5);
    Result.Timestamp := EncodeDateTime(2014,8,5,17,28,12,0);
    Result.AByte     := 128;
    Result.AWord     := 32768;
    Result.AInt64    := 900200300400;
    Result.Enum      := Last;
  End
  Else
    raise Exception.Create( 'MakeSimpleObject: Index out of bounds' );
end;

procedure TestTJSONSerializer.TestSerializeContainer;
var
  JSON: String;
  Obj: TSimpleContainer;
begin
  Obj := MakeSimpleContainerObject(1);
  Try
    JSON := TJSONSerializer.Serialize( Obj );
    JSON := StripWhiteSpace( JSON );
    CheckEquals( SimpleContainerJSON_1, JSON );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONSerializer.TestSerializeListOfContainer;
var
  JSON: String;
  Obj: TObjectList<TSimpleContainer>;
begin
  Obj := TObjectList<TSimpleContainer>.Create;
  Obj.Add( MakeSimpleContainerObject( 1 ) );
  Obj.Add( MakeSimpleContainerObject( 2 ) );
  Try
    JSON := TJSONSerializer.Serialize( Obj );
    JSON := StripWhiteSpace( JSON );
    CheckEquals( ArrayOfSimpleContainerJSON, JSON );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONSerializer.TestSerializeListOfObjects;
var
  JSON: String;
  Obj: TObjectList<TSimple>;
begin
  Obj := TObjectList<TSimple>.Create;
  Obj.Add( MakeSimpleObject( 1 ) );
  Obj.Add( MakeSimpleObject( 2 ) );
  Try
    JSON := TJSONSerializer.Serialize( Obj );
    JSON := StripWhiteSpace( JSON );
    CheckEquals( '[' + SimpleJSON_1 + ',' + SimpleJSON_2 + ']', JSON );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONSerializer.TestSerializeLists;
var
  JSON: String;
  Obj: TSimpleLists;
begin
  Obj := MakeSimpleListsObject;
  Try
    JSON := TJSONSerializer.Serialize( Obj );
    JSON := StripWhiteSpace( JSON );
    CheckEquals( SimpleListJSON, JSON );
  Finally
    Obj.Free;
  End;
end;

procedure TestTJSONSerializer.TestSerializeSimple;
var
  JSON: String;
  Obj: TSimple;
begin
  Obj := MakeSimpleObject( 1 );
  Try
    JSON := TJSONSerializer.Serialize( Obj );
    JSON := StripWhiteSpace( JSON );

    CheckEquals( SimpleJSON_1,
//    '{"Str":"Hello",' +
//                 '"C":"T",' +
//                 '"Int":88,' +
//                 '"Bool":true,' +
//                 '"Number":5.125,' +
//                 '"Day":"2014-08-05",' +
//                 '"Timestamp":"2014-08-05T11:28:12"}',
                 JSON );
  Finally
    Obj.Free;
  End;
end;

initialization
  RegisterTest(TestTJSONElements.Suite);
  RegisterTest(TestTJSON.Suite);
  RegisterTest(TestTJSONParser.Suite);
  RegisterTest(TestTJSONSerializer.Suite);
end.

