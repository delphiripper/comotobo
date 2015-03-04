{
  JSON v1.0

  August 9th 2014

  Author: Jesper B. Christensen
          delphiripper@gmail.com

  *******************************************************************************

  No rights reserved - copy and modify at will

  *******************************************************************************

  THIS SOFTWARE IS PROVIDED BY Jesper B. Christensen ``AS IS'' AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL Jesper B. Christensen BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  *******************************************************************************

  This unit allows you to serialize/deserialize Delphi objects to/from JSON.
  It is designed to work with Data Transfer Objects (DTO). The Delphi objects
  should:
    - NOT contains uninitialized object properties (no nil pointers)
    - Use TStringList or TList for "arrays" of simple types
    - Use generic TObjectList for "arrays" of objects

  The purpose of the classes:
    - TJSON:           String (JSON) -> TJSON_Element (tree)
    - TJSONParser:     String (JSON) -> Object
    - TJSONSerializer: Object        -> TJSON_Element (tree)
                       Object        -> String (JSON)

  Known limitations:
    - No list of lists (but lists of objects with lists are OK)
    - Records not supported (use objects)
    - (Delphi) arrays not supported (use lists)
    - Variants not supported (use simple types)

  *******************************************************************************

  Changes:

  v1.0 2014-08-09 * first version
  v1.1 2015-03-04 * TJSONParser.IsDateTime() added
                  * Indy HTTP replaced by WinHttp (allows automatic windows authentication)

}
unit JSON;

interface

Uses
  System.Rtti, System.SysUtils, System.Generics.Collections;

{$REGION 'Exception classes'}
Type
  EJSONFormatError = Class(Exception);
  JSONException = Class(Exception);
{$ENDREGION}

{$REGION 'JSON Tree classes'}

{$REGION 'JSON Tree elements'}
  TJSONtype = (jtElement, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject, jtPair);

  TJSON_Null = Class;
  TJSON_Number = Class;
  TJSON_String = Class;
  TJSON_Boolean = Class;
  TJSON_Pair = Class;
  TJSON_Array = Class;
  TJSON_Object = Class;

  TJSON_Element = Class
  Protected
    Class procedure IncIndent( var s: String ); inline;
    Class procedure DecIndent( var s: String ); inline;
  Public
    function SelfType: TJSONtype; virtual;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Virtual; Abstract;
    Function ToJSON: String;

    Function IsNumber: Boolean; inline;
    Function IsString: Boolean; inline;
    Function IsBoolean: Boolean; inline;
    Function IsNull: Boolean; inline;
    Function IsArray: Boolean; inline;
    Function IsObject: Boolean; inline;
    Function IsPair: Boolean; inline;

    Function AsNumber: TJSON_Number; inline;
    Function AsString: TJSON_String; inline;
    Function AsBoolean: TJSON_Boolean; inline;
    Function AsNull: TJSON_Null; inline;
    Function AsArray: TJSON_Array; inline;
    Function AsObject: TJSON_Object; inline;
    Function AsPair: TJSON_Pair; inline;
  End;

  TJSON_Null = Class( TJSON_Element )
  Public
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_Number = Class( TJSON_Element )
  strict private
    FValue: Extended;
    FIntValue: Int64;
    FIsInteger: Boolean;
    function GetValue: Extended;
  Public
    Property Value: Extended read GetValue;
    Property IsInteger: Boolean read FIsInteger;
    Function AsInteger: Integer;
    Function AsInt64: Int64;
    Constructor Create( AValue: Extended );
    Constructor CreateInteger( AValue: Int64 );
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_String = Class( TJSON_Element )
  strict private
    FValue: String;
  Public
    Property Value: String read FValue write FValue;
    Constructor Create( AValue: String );
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_Boolean = Class( TJSON_Element )
  private
    FValue: Boolean;
  Public
    Property Value: Boolean read FValue write FValue;
    Constructor Create( AValue: Boolean );
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_Array = Class( TJSON_Element )
  Private
    FList: TObjectList<TJSON_Element>;
    function GetCount: Integer;
    function GetElement(Index: Integer): TJSON_Element;
  Public
    Property Count: Integer read GetCount;
    Property Elements[Index: Integer]: TJSON_Element read GetElement;
    Procedure Add( AElement: TJSON_Element );
    Constructor Create;
    Destructor Destroy; Override;
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_Pair = Class( TJSON_Element )
  strict private
    FName: String;
    FValue: TJSON_Element;
  Public
    Property Name: String read FName write FName;
    Property Value: TJSON_Element read FValue write FValue;
    Constructor Create( AName: String = ''; AValue: TJSON_Element = nil);
    Destructor Destroy; Override;
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;

  TJSON_Object = Class( TJSON_Element )
  Private
    FList: TObjectList<TJSON_Pair>;
    function GetCount: Integer;
    function GetPair(Index: Integer): TJSON_Pair;
  Public
    Property Count: Integer read GetCount;
    Property Pairs[Index: Integer]: TJSON_Pair read GetPair;
    Procedure Add( APair: TJSON_Pair );
    Constructor Create;
    Destructor Destroy; Override;
    function SelfType: TJSONtype; override;
    Procedure Serialize( S: TStringBuilder; var Indent: String ); Override;
  End;
{$ENDREGION}

  ///	<summary>
  ///	  TJSON load a JSON document into a tree structure
  ///	</summary>
  TJSON = class
  private
    class procedure EatWhitespace( const JSON: string; var Idx: Integer ); Inline;
    class procedure Eat(           const JSON: string; var Idx: Integer; Ch: Char ); Inline;
    class procedure EatOptional(   const JSON: string; var Idx: Integer; Ch: Char ); Inline;
    class procedure EatOneOf(      const JSON: string; var Idx: Integer; CharSet: String ); Inline;
    class function Compare(        const JSON: String; Idx: Integer; CompareTo: String): Boolean; Inline;
    class function ParseValue(     const JSON: String; var Idx: Integer ): TJSON_Element;
    class function ParseArray(     const JSON: String; var Idx: Integer ): TJSON_Array;
    class function ParseObject(    const JSON: String; var Idx: Integer ): TJSON_Object;
    class function ParsePair(      const JSON: String; var Idx: Integer ): TJSON_Pair;
    class function ParseString(    const JSON: String; var Idx: Integer ): String; Inline;
    class function ParseNumber(    const JSON: String; var Idx: Integer ): TJSON_Number;
  public
    class function ParseText(const JSON: string): TJSON_Element;
    class function Get(const URL: string): String; Overload;
    class Procedure Get(const URL: string; Obj: TObject); Overload;
  end;
{$ENDREGION}

  ///	<summary>
  ///	  TJSONParser parses a JSON document (using TJSON) and sets the
  ///	  corresponding properties of an object.
  ///	</summary>
  TJSONParser = Class
  Private
    {$REGION 'Private section'}
    Class Function IsBoolean( AType: TRttiType ): Boolean; inline;
    Class Function IsDateType( AType: TRttiType ): Boolean; inline;
    Class Function ParseDateTime( S: String ): TDateTime; inline;
    class function CompatibleTypes( JSONType: TJSONtype; DelphiType: TRttiType ): Boolean; inline;
    class function FindCompatibleAddMethod( ListType: TRttiType; JSONElementType: TJSONtype;
                                            var AParameter: TRttiParameter ): TRttiMethod;
    Class Procedure ParseObject( J: TJSON_Object; Obj: TObject );
    Class Procedure ParseList( J: TJSON_Array; Obj: TObject );
    Class Procedure ParseProperty( J: TJSON_Element; PropertyName: String; Obj: TObject; ObjType: TRttiType );
    Class Procedure ParseListProperty( J: TJSON_Array; Obj: TObject; ObjProp: TRttiProperty );
    Class Procedure SetNumberProperty(  J: TJSON_Number;  Obj: TObject; ObjProp: TRttiProperty );
    Class Procedure SetStringProperty(  J: TJSON_String;  Obj: TObject; ObjProp: TRttiProperty );
    Class Procedure SetBooleanProperty( J: TJSON_Boolean; Obj: TObject; ObjProp: TRttiProperty );
    Class Procedure SetNullProperty( ObjProp: TRttiProperty );
    {$ENDREGION}
  Public
    ///	<summary>
    ///	  Parses a JSON document (using TJSON) and sets the
    ///	  corresponding properties of Obj
    ///	</summary>
    Class Procedure Parse( JSON: String; Obj: TObject );
    Class Function IsDateTime( Str: String ): Boolean;
  End;

  TListObjectDetails = Record
    CountProperty: TRttiProperty;
    AddMethod: TRttiMethod;
    GetMethod: TRttiMethod;
    ElementType: TRttiType;
    Function IsListObject: Boolean;
  End;

  ///	<summary>
  ///	  TJSONSerializer writes the properties of an object to JSON
  ///	</summary>
  TJSONSerializer = Class
  Private
    {$REGION 'Private section'}
    Class function IsListObject( AObject: TObject; var ListObjectDetails: TListObjectDetails ): Boolean;
    Class Function SerializeValue( Const Value: TValue; ValueType: TRttiType ): TJSON_Element;
    Class Function SerializeObjectList( Obj: TObject; ListObjectDetails: TListObjectDetails ): TJSON_Array;
    Class Function SerializeObject( Obj: TObject ): TJSON_Object;
    {$ENDREGION}
  Public
    ///	<summary>
    ///	  Make a TJSON (tree) from Obj
    ///	</summary>
    Class Function ToJSON( Obj: TObject ): TJSON_Element; Overload;
    ///	<summary>
    ///	  Serializes Obj to JSON
    ///	</summary>
    Class Function Serialize( Obj: TObject ): String; Overload;
  End;

{$REGION 'Char constants'}
Const
  CH_BACKSPACE    = #$0008;
  CH_TAB          = #$0009;
  CH_LF           = #$000A;
  CH_FORM_FEED    = #$000C;
  CH_CR           = #$000D;
  CH_SPACE        = #$0020;
  CH_QUOTE        = #$0022;
  CH_COMMA        = #$002C;
  CH_SLASH        = #$002F;
  CH_COLON        = #$003A;
  CH_LEFT_SQUARE  = #$005B;
  CH_BACKSLASH    = #$005C;
  CH_RIGHT_SQUARE = #$005D;
  CH_LEFT_CURLY   = #$007B;
  CH_RIGHT_CURLY  = #$007D;
  WHITE_SPACE     = CH_TAB + CH_LF + CH_CR + CH_SPACE;
  NUMBER          = '0123456789';
  HEX_CHARS       = '0123456789ABCDEFabcdef';
{$ENDREGION}

Procedure SerializeString(Const Str: String; S: TStringBuilder);

implementation

Uses
  System.TypInfo, System.DateUtils, System.Types, System.Classes, System.Variants, WinHttp_TLB,
  Winapi.ActiveX, Vcl.AxCtrls;

var
  Init: Boolean = False;
  ctx: TRttiContext;
  JSONFormat: TFormatSettings;

Type
  THTTPAccept = ( aDefault, aJSON, aXML );
  THTTPRequestType = ( rGET, rPOST );

Const
  HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
  HTTPREQUEST_SETCREDENTIALS_FOR_PROXY  = 1;

Function WinHTTP( RequestType: THTTPRequestType; URL, ADUserName, ADPassword: String; ResultStream: TStream;
                  Accept: THTTPAccept = aDefault; ABodyText: String = ''; ContentType: THTTPAccept = aDefault ): Integer;
const
  RequestTypeStr: Array[THTTPRequestType] of string = ( 'GET', 'POST' );
var
  Http: IWinHttpRequest;
  HttpStream: IStream;
  OleStream: TOleStream;
Begin
  HttpStream := nil;
  OleStream  := nil;
  Http := CoWinHttpRequest.Create;
  Try
    Http.Open( RequestTypeStr[RequestType], URL, False );
    if ADUserName <> '' then
    Begin
      Http.SetAutoLogonPolicy( AutoLogonPolicy_Never );
      Http.SetCredentials( ADUserName, ADPassword, HTTPREQUEST_SETCREDENTIALS_FOR_SERVER );
    End
    Else
      Http.SetAutoLogonPolicy( AutoLogonPolicy_Always );
    Case Accept of
      aJSON: Http.SetRequestHeader( 'accept', 'application/json' );
      aXML : Http.SetRequestHeader( 'accept', 'application/xml' );
    End;
    case RequestType of
      rGET: Http.Send(EmptyParam);
      rPOST:
        begin
          case ContentType of
            aJSON: Http.SetRequestHeader( 'Content-Type', 'application/json' );
            aXML: Http.SetRequestHeader( 'Content-Type', 'application/xml' );
          end;
          Http.SetRequestHeader( 'Content-Length', IntToStr(Length(ABodyText)) );
          Http.Send(ABodyText);
        end;
    end;
    Result := Http.Status;
    HttpStream := IUnknown(http.ResponseStream) as IStream;
    OleStream  := TOleStream.Create(HttpStream);
    OleStream.Position := 0;
    ResultStream.CopyFrom( OleStream, OleStream.Size );
  Finally
    OleStream.Free;
    HttpStream := nil;
    Http := nil;
  End;
End;

Procedure SerializeString(Const Str: String; S: TStringBuilder);
var
  i: Integer;
begin
  S.Append( CH_QUOTE );
  for i := 1 to length(Str) do
    case Str[i] of
      CH_BACKSLASH: S.Append( '\' + CH_BACKSLASH );
      CH_SLASH:     S.Append( '\' + CH_SLASH );
      CH_QUOTE:     S.Append( '\' + CH_QUOTE );
      CH_BACKSPACE: S.Append( '\b' );
      CH_TAB:       S.Append( '\t' );
      CH_LF:        S.Append( '\n' );
      CH_CR:        S.Append( '\r' );
      CH_FORM_FEED: S.Append( '\f' );
    else
      if Ord( Str[i] ) < $20 then
        S.Append( '\u' + IntToHex( ord( Str[i] ), 4 ) )
      else
        S.Append( Str[i] );
    end;
  S.Append( CH_QUOTE );
end;

{ TJSON }

class function TJSON.Compare(const JSON: String; Idx: Integer; CompareTo: String): Boolean;
var
  I, Count: Integer;
begin
  Result := True;
  Count := CompareTo.Length;
  for I := 1 to Count do
    if JSON[Idx + I - 1] <> CompareTo[ I ] then
       Exit( False );
end;

class procedure TJSON.Eat(const JSON: string; var Idx: Integer; Ch: Char);
begin
  If (Idx > JSON.Length) then
    raise EJSONFormatError.Create('End of JSON: Expected "' + Ch + '"');
  If JSON[Idx] = Ch then
    Inc(Idx)
  Else
    raise EJSONFormatError.CreateFmt('Expected "%s". Found [%d]: %s', [Ch, Idx, Copy(JSON, Idx, 30) ] );
end;

class procedure TJSON.EatOneOf(const JSON: string; var Idx: Integer; CharSet: String);
begin
  If (Idx > JSON.Length) then
    raise EJSONFormatError.Create('End of JSON. Expected one of these: "' + CharSet + '"');
  If Pos( JSON[Idx], CharSet ) > 0 then
    Inc(Idx)
  Else
    raise EJSONFormatError.CreateFmt('Expected a character in "%s". Found [%d]: %s', [CharSet, Idx, Copy(JSON, Idx, 30) ] );
end;

class procedure TJSON.EatOptional(const JSON: string; var Idx: Integer; Ch: Char);
begin
  If (Idx > JSON.Length) then
    raise EJSONFormatError.Create('End of JSON: Expected "' + Ch + '"');
  If JSON[Idx] = Ch then
    Inc(Idx);
end;

class procedure TJSON.EatWhitespace(const JSON: string; var Idx: Integer);
begin
  while (Idx <= JSON.Length) and (Pos( JSON[Idx], WHITE_SPACE ) > 0) do
    Inc(Idx);
end;

class procedure TJSON.Get(const URL: string; Obj: TObject);
begin
  TJSONParser.Parse( Get( URL ), Obj );
end;

class function TJSON.Get(const URL: string): String;
var
  HTTPResult: Integer;
  Response: TStringStream;
begin
  Response := TStringStream.Create( '', TEncoding.UTF8 );
  Try
    HTTPResult := WinHTTP( rGET, URL, '', '', Response, aJSON );
    if HTTPResult = 200 then
      Result := Response.DataString
    else
      raise JSONException.Create( 'HTTP result for "' + URL + '" was ' + IntToStr( HTTPResult ) + ': ' +
                                  Copy(Response.DataString,1,500));
  Finally
    Response.Free;
  End;
end;

class function TJSON.ParseValue(const JSON: String; var Idx: Integer): TJSON_Element;
begin
  Case JSON[Idx] of
    CH_LEFT_SQUARE : Result := ParseArray(JSON, Idx);
    CH_LEFT_CURLY  : Result := ParseObject(JSON, Idx);
    CH_QUOTE       : Result := TJSON_String.Create( ParseString( JSON, Idx ) );
    't'            : Begin
                       if Compare(JSON, Idx, 'true') then
                          Result := TJSON_Boolean.Create( True )
                       else
                         raise EJSONFormatError.CreateFmt('Expected "true" [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
                       inc(Idx, 4);
                     End;
    'f'            : Begin
                       if Compare(JSON, Idx, 'false') then
                          Result := TJSON_Boolean.Create( False )
                       else
                         raise EJSONFormatError.CreateFmt('Expected "false" [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
                       inc(Idx, 5);
                     End;
    'n'            : Begin
                       if Compare(JSON, Idx, 'null') then
                          Result := TJSON_Null.Create
                       else
                         raise EJSONFormatError.CreateFmt('Expected "false" [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
                       inc(Idx, 4);
                     End;
    '-', '0'..'9' : Result := ParseNumber(JSON, Idx);
    else
      raise EJSONFormatError.CreateFmt('Unexpected character [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
  End;
end;

class function TJSON.ParseArray(const JSON: String; var Idx: Integer): TJSON_Array;
begin
  Result := TJSON_Array.Create;
  Try
    Eat( JSON, Idx, CH_LEFT_SQUARE );
    EatWhitespace( JSON, Idx );
    While JSON[Idx] <> CH_RIGHT_SQUARE do
    Begin
      Result.Add( ParseValue(JSON, Idx) );
      EatWhitespace( JSON, Idx );
      if JSON[Idx] = CH_COMMA then
      Begin
        inc(Idx); //Eat comma
        EatWhitespace( JSON, Idx );
        if JSON[Idx] = CH_RIGHT_SQUARE then
          raise EJSONFormatError.CreateFmt( 'Expected value [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
      End
      else if JSON[Idx] <> CH_RIGHT_SQUARE then
          raise EJSONFormatError.CreateFmt( 'Expected "]" [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
    End;
    Eat( JSON, Idx, CH_RIGHT_SQUARE );
  Except
    Result.Free;
    Raise;
  End;
end;

class function TJSON.ParseNumber(const JSON: String; var Idx: Integer): TJSON_Number;
var
  Start: Integer;
  IsInteger: Boolean;
  NumberStr: String;
begin
  IsInteger := True;
  Start := Idx;
  //Optional "-" as first char
  EatOptional( JSON, Idx, '-' );
  //Required 0-9 digit
  EatOneOf( JSON, Idx, NUMBER );
  //Optional chars
  While (Idx <= JSON.Length) and (Pos( JSON[Idx], NUMBER ) > 0) do
    Inc(Idx);
  //Decimal
  if JSON[Idx] = '.' then
  Begin
    IsInteger := False;
    Inc(Idx);
    //Required 0-9 digit
    EatOneOf( JSON, Idx, NUMBER );
    //Optional chars
    While (Idx <= JSON.Length) and (Pos( JSON[Idx], NUMBER ) > 0) do
      Inc(Idx);
  End;
  //Exponential
  if (Idx <= JSON.Length) and
     ( (JSON[Idx] = 'e') or (JSON[Idx] = 'E') ) then
  Begin
    IsInteger := False;
    inc(Idx);
    if (Idx > JSON.Length) then
      raise EJSONFormatError.Create('Expected "+", "-" or number');
    if (JSON[Idx] = '+') or (JSON[Idx] = '-') then
      Inc(Idx);
    //Required 0-9 digit
    EatOneOf( JSON, Idx, NUMBER );
    //Optional chars
    While Pos( JSON[Idx], NUMBER ) > 0 do
      Inc(Idx);
  End;
  NumberStr := Copy(JSON, Start, Idx-Start);
  If IsInteger then
    Result := TJSON_Number.CreateInteger( StrToInt64( NumberStr ) )
  Else
    Result := TJSON_Number.Create( StrToFloat( NumberStr, JSONFormat ) );
end;

class function TJSON.ParseObject(const JSON: String; var Idx: Integer): TJSON_Object;
begin
  Result := TJSON_Object.Create;
  Try
    Eat( JSON, Idx, CH_LEFT_CURLY );
    EatWhitespace( JSON, Idx );
    While JSON[Idx] <> CH_RIGHT_CURLY do
    Begin
      Result.Add( ParsePair(JSON, Idx) );
      EatWhitespace( JSON, Idx );
      if JSON[Idx] = CH_COMMA then
      Begin
        inc(Idx); //Eat comma
        EatWhitespace( JSON, Idx );
        if JSON[Idx] = CH_RIGHT_CURLY then
          raise EJSONFormatError.CreateFmt( 'Expected value [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
      End
      else if JSON[Idx] <> CH_RIGHT_CURLY then
          raise EJSONFormatError.CreateFmt( 'Expected "}" [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
    End;
    Eat( JSON, Idx, CH_RIGHT_CURLY );
  Except
    Result.Free;
    Raise;
  End;
end;

class function TJSON.ParsePair(const JSON: String; var Idx: Integer): TJSON_Pair;
var
  Value: TJSON_Element;
  Name: string;
begin
  Name := ParseString( JSON, Idx );
  EatWhitespace( JSON, Idx );
  Eat( JSON, Idx, CH_COLON );
  EatWhitespace( JSON, Idx );
  Value := ParseValue( JSON, Idx );
  Result := TJSON_Pair.Create( Name, Value );
end;

class function TJSON.ParseString(const JSON: String; var Idx: Integer): String;
var
  CharCode: Word;
  Start: Integer;
begin
  Result := '';
  Eat( JSON, Idx, CH_QUOTE );
  while (Idx <= JSON.Length) and (JSON[Idx] <> CH_QUOTE) do
  Begin
    If JSON[Idx] <> CH_BACKSLASH then
    Begin
      Start := Idx;
      while (Idx+1 <= JSON.Length) and
            (JSON[Idx+1] <> CH_BACKSLASH) and
            (JSON[Idx+1] <> CH_QUOTE) do
      Begin
        Inc(Idx);
        If (JSON[Idx] <= #$001F ) then
          raise EJSONFormatError.Create('Illegal character in string value: #' + IntToHex( Ord( JSON[Idx] ), 4 ) );
      End;
      Result := Result + Copy( JSON, Start, Idx-Start+1 );
      inc(Idx);
    End
    Else
    Begin
      inc(Idx);
      if (Idx > JSON.Length) then
        raise EJSONFormatError.Create('End of JSON: Unterminated string');

      case JSON[Idx] of
        CH_SLASH,
        CH_BACKSLASH,
        CH_QUOTE: Result := Result + JSON[Idx];
        'b': Result := Result + CH_BACKSPACE;
        'f': Result := Result + CH_FORM_FEED;
        'n': Result := Result + CH_LF;
        'r': Result := Result + CH_CR;
        't': Result := Result + CH_TAB;
        'u': Begin
               Result := Result + CH_TAB;
               if (Pos( JSON[Idx+1], HEX_CHARS ) = 0) or
                  (Pos( JSON[Idx+2], HEX_CHARS ) = 0) or
                  (Pos( JSON[Idx+3], HEX_CHARS ) = 0) or
                  (Pos( JSON[Idx+4], HEX_CHARS ) = 0) then
                 raise EJSONFormatError.Create('Invalid hex number: ' + Copy(JSON, Idx+1, 4) );
               CharCode := StrToInt( '$' + Copy(JSON, Idx+1, 4) );
               Result := Result + WideChar(CharCode);
               inc(Idx, 3); //Not 4 because there is an "Inc(Idx)" below
             End;
        else
          raise EJSONFormatError.Create('Invalid escaped character "' + JSON[Idx] + '"');
      end;
      inc(Idx);
    End;
  End;
  Eat( JSON, Idx, CH_QUOTE );
end;

class function TJSON.ParseText(const JSON: string): TJSON_Element;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := 1;
  Try
    EatWhitespace( JSON, Idx );
    Result := ParseValue( JSON, Idx );
    EatWhitespace( JSON, Idx );
    if Idx <= JSON.Length then
      raise EJSONFormatError.CreateFmt( 'More than one root object [%d]: %s', [Idx, Copy(JSON, Idx, 30) ] );
  Except
    Result.Free;
    Raise;
  End;
end;

{ TJSON_Element }

function TJSON_Element.AsArray: TJSON_Array;
begin
  Result := Self as TJSON_Array;
end;

function TJSON_Element.AsBoolean: TJSON_Boolean;
begin
  Result := Self as TJSON_Boolean;
end;

function TJSON_Element.AsNull: TJSON_Null;
begin
  Result := Self as TJSON_Null;
end;

function TJSON_Element.AsNumber: TJSON_Number;
begin
  Result := self as TJSON_Number;
end;

function TJSON_Element.AsObject: TJSON_Object;
begin
  Result := Self as TJSON_Object;
end;

function TJSON_Element.AsPair: TJSON_Pair;
begin
  Result := Self as TJSON_Pair;
end;

function TJSON_Element.AsString: TJSON_String;
begin
  Result := Self as TJSON_String;
end;

class procedure TJSON_Element.DecIndent(var s: String);
begin
  s := Copy( s, 1, s.Length-4 );
end;

class procedure TJSON_Element.IncIndent(var s: String);
begin
  s := s + '    ';
end;

function TJSON_Element.IsArray: Boolean;
begin
  Result := Self is TJSON_Array;
end;

function TJSON_Element.IsBoolean: Boolean;
begin
  Result := Self is TJSON_Boolean;
end;

function TJSON_Element.IsNull: Boolean;
begin
  Result := Self is TJSON_Null;
end;

function TJSON_Element.IsNumber: Boolean;
begin
  Result := Self is TJSON_Number;
end;

function TJSON_Element.IsObject: Boolean;
begin
  Result := Self is TJSON_Object;
end;

function TJSON_Element.IsPair: Boolean;
begin
  Result := Self is TJSON_Pair;
end;

function TJSON_Element.IsString: Boolean;
begin
  Result := Self is TJSON_String;
end;

function TJSON_Element.SelfType: TJSONtype;
begin
  Result := jtElement;
end;

function TJSON_Element.ToJSON: String;
var
  Indent: String;
  S: TStringBuilder;
begin
  S := TStringBuilder.Create;
  Try
    Indent := '';
    Serialize( S, Indent );
    Result := S.ToString;
  Finally
    S.Free;
  End;
end;

{ TJSON_Number }

function TJSON_Number.AsInt64: Int64;
begin
  if IsInteger then
    Result := FIntValue
  else
    Result := Round( Int( FValue ) );
end;

function TJSON_Number.AsInteger: Integer;
begin
  if IsInteger then
    Result := FIntValue
  else
    Result := Round( Int( FValue ) );
end;

constructor TJSON_Number.Create(AValue: Extended);
begin
  FValue := AValue;
  FIsInteger := False;
end;

constructor TJSON_Number.CreateInteger(AValue: Int64);
begin
  FIsInteger := True;
  FIntValue := AValue;
end;

function TJSON_Number.GetValue: Extended;
begin
  if IsInteger then
    Result := FIntValue
  else
    Result := FValue;
end;

function TJSON_Number.SelfType: TJSONtype;
begin
  Result := jtNumber;
end;

procedure TJSON_Number.Serialize(S: TStringBuilder; var Indent: String);
begin
  if IsInteger then
    S.Append( FIntValue )
  else
    S.Append( FloatToStr( FValue, JSONFormat ) );
end;

{ TJSON_String }

constructor TJSON_String.Create(AValue: String);
begin
  FValue := AValue;
end;

function TJSON_String.SelfType: TJSONtype;
begin
  Result := jtString;
end;

procedure TJSON_String.Serialize(S: TStringBuilder; var Indent: String);
begin
  SerializeString( FValue, S );
end;

{ TJSON_Boolean }

constructor TJSON_Boolean.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

function TJSON_Boolean.SelfType: TJSONtype;
begin
  Result := jtBoolean;
end;

procedure TJSON_Boolean.Serialize(S: TStringBuilder; var Indent: String);
begin
  if FValue then
    S.Append( 'true' )
  else
    S.Append( 'false' );
end;

{ TJSON_Null }

function TJSON_Null.SelfType: TJSONtype;
begin
  Result := jtNull;
end;

procedure TJSON_Null.Serialize(S: TStringBuilder; var Indent: String);
begin
  S.Append( 'null' );
end;

{ TJSON_Array }

procedure TJSON_Array.Add(AElement: TJSON_Element);
begin
  FList.Add( AElement );
end;

constructor TJSON_Array.Create;
begin
  FList := TObjectList<TJSON_Element>.Create;
end;

destructor TJSON_Array.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJSON_Array.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSON_Array.GetElement(Index: Integer): TJSON_Element;
begin
  Result := FList[Index];
end;

function TJSON_Array.SelfType: TJSONtype;
begin
  Result := jtArray;
end;

procedure TJSON_Array.Serialize(S: TStringBuilder; var Indent: String);
var
  i: Integer;
  Splitter: String;
begin
  if FList.Count = 0 then
    S.Append( CH_LEFT_SQUARE + ' ' + CH_RIGHT_SQUARE )
  else
  Begin
    Splitter := CH_COMMA + ' ';
    if FList[0].SelfType = jtObject then
    Begin
      S.Append( CH_LEFT_SQUARE + CH_CR + CH_LF);
      IncIndent( Indent );
      Splitter := CH_COMMA + CH_CR + CH_LF;
    End
    Else
      S.Append( CH_LEFT_SQUARE + ' ' );

    for i := 1 to FList.Count do
    Begin
      FList[i-1].Serialize( S, Indent );
      if i < FList.Count then
        S.Append( Splitter );
    End;

    if FList[0].SelfType = jtObject then
    Begin
      DecIndent( Indent );
      S.AppendLine.Append( Indent ).Append( CH_RIGHT_SQUARE );
    End
    else
      S.Append( ' ' + CH_RIGHT_SQUARE );
  End;
end;

{ TJSON_Object }

procedure TJSON_Object.Add(APair: TJSON_Pair);
begin
  FList.Add(APair);
end;

constructor TJSON_Object.Create;
begin
  FList := TObjectList<TJSON_Pair>.Create;
end;

destructor TJSON_Object.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJSON_Object.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSON_Object.GetPair(Index: Integer): TJSON_Pair;
begin
  Result := FList[Index];
end;

function TJSON_Object.SelfType: TJSONtype;
begin
  Result := jtObject;
end;

procedure TJSON_Object.Serialize(S: TStringBuilder; var Indent: String);
var
  i: Integer;
begin
  S.Append( Indent + CH_LEFT_CURLY ).AppendLine;
  IncIndent( Indent );
  for i := 1 to FList.Count do
  Begin
    FList[i-1].Serialize( S, Indent );
    if i < FList.Count then
      S.Append( CH_COMMA );
    S.AppendLine;
  End;
  DecIndent( Indent );
  S.Append( Indent + CH_RIGHT_CURLY );
end;

{ TJSON_Pair }

constructor TJSON_Pair.Create(AName: String; AValue: TJSON_Element);
begin
  FName := AName;
  FValue := AValue;
end;

destructor TJSON_Pair.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TJSON_Pair.SelfType: TJSONtype;
begin
  Result := jtPair;
end;

procedure TJSON_Pair.Serialize(S: TStringBuilder; var Indent: String);
begin
  S.Append( Indent );
  SerializeString( Name, S );
  S.Append( CH_COLON + ' ' );
  FValue.Serialize( S, Indent );
end;

{ TJSONParser }

class function TJSONParser.IsBoolean(AType: TRttiType): Boolean;
begin
  Result := (AType.TypeKind = tkEnumeration) and
            (AType.QualifiedName = 'System.Boolean');
end;

class function TJSONParser.IsDateTime(Str: String): Boolean;

  Function CheckMask( Const S: String; Const Mask: String ): Boolean;
  var
    I: Integer;
  Begin
    Result := Length(s) = Length(Mask);
    if Result then
      for I := 1 to Length(s) do
      Begin
        if Mask[I] = '0' then
          Result := CharInSet( S[I], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] )
        Else
          Result := S[I] = Mask[i];
        if not Result then
          Exit;
      End;
  End;

begin
  Result := CheckMask( Str, '0000-00-00T00:00:00Z' ) or
            CheckMask( Str, '0000-00-00T00:00:00' ) or
            CheckMask( Str, '0000-00-00Z' ) or
            CheckMask( Str, '0000-00-00' );
end;

class function TJSONParser.IsDateType(AType: TRttiType): Boolean;
begin
  Result := (AType.TypeKind = tkFloat) and
            ( (AType.QualifiedName = 'System.TDateTime') or
              (AType.QualifiedName = 'System.TDate') or
              (AType.QualifiedName = 'System.TTime') );
end;

Class Function TJSONParser.ParseDateTime( S: String ): TDateTime;
var
  y, m, d, h, min, sec, ms: Word;
Begin //2014-05-24T06:00:00
  y   := StrToInt( Copy(S,  1, 4) );
  m   := StrToInt( Copy(S,  6, 2) );
  d   := StrToInt( Copy(S,  9, 2) );
  if (Length(s) >= 11) and (S[11] = 'T') then
  Begin
    h   := StrToInt( Copy(S, 12, 2) );
    min := StrToInt( Copy(S, 15, 2) );
    sec := StrToInt( Copy(S, 18, 2) );
    ms  := 0;
  End
  Else
  Begin
    h   := 0;
    min := 0;
    sec := 0;
    ms  := 0;
  End;
  Result := EncodeDateTime( y, m, d, h, min, sec, ms );
End;

class function TJSONParser.CompatibleTypes( JSONType: TJSONtype; DelphiType: TRttiType ): Boolean;
Begin
  if (DelphiType.TypeKind = tkClass) and (JSONType = jtObject) then
    Result := True
  else If IsDateType( DelphiType ) and (JSONType = jtString) then
    Result := True
  else If (DelphiType.TypeKind in [tkString, tkLString, tkWString, tkUString]) and (JSONType = jtString) then
    Result := True
  else If (DelphiType.TypeKind in [tkChar, tkWChar]) and (JSONType = jtString) then
    Result := True
  else If (DelphiType.TypeKind in [tkInteger, tkFloat, tkInt64]) and (JSONType = jtNumber) then
    Result := True
  else If IsBoolean(DelphiType) and (JSONType = jtBoolean) then
    Result := True
  else
    Result := False;
End;

class function TJSONParser.FindCompatibleAddMethod( ListType: TRttiType; JSONElementType: TJSONtype;
  var AParameter: TRttiParameter ): TRttiMethod;
var
  Method: TRttiMethod;
  Parameters: TArray<TRttiParameter>;
Begin
  For Method in ListType.GetMethods do
    if (Method.Name = 'Add') then
    Begin
      Parameters := Method.GetParameters;
      If (Length(Parameters) = 1) then
      Begin
        AParameter := Parameters[0];
        if CompatibleTypes( JSONElementType, AParameter.ParamType ) then
          Exit( Method )
      End;
    End;
  Result := nil;
End;

Class procedure TJSONParser.Parse(JSON: String; Obj: TObject);
var
  J: TJSON_Element;
  ListObjectDetails: TListObjectDetails;
  ObjIsList: Boolean;
begin
  if not Init then
  Begin
    ctx := TRttiContext.Create;
    Init := True;
  End;
  J := TJSON.ParseText( JSON );
  ObjIsList := TJSONSerializer.IsListObject( Obj, ListObjectDetails );
  Try
    If J.SelfType = jtObject then
      ParseObject( J as TJSON_Object, Obj )
    else If (J.SelfType = jtArray) then
    Begin
      If ObjIsList then
        ParseList( J as TJSON_Array, Obj )
      else If (TJSON_Array(J).Count = 1) and
              (TJSON_Array(J).Elements[0].SelfType = jtObject) then
        ParseObject( TJSON_Array(J).Elements[0]  as TJSON_Object, Obj )
      else
        raise JSONException.Create('JSON has array. Obj is not a list.');
    End
    else
      raise JSONException.Create('JSON is not an object or an array');
  Finally
    J.Free;
    ctx.Free;
  End;
end;

class procedure TJSONParser.ParseList(J: TJSON_Array; Obj: TObject);
var
  ItemConstructorMethod: TRttiMethod;
  AddMethod: TRttiMethod;
  Parameter: TRttiParameter;
  ItemType: TRttiType;
  NewItem: TValue;
  i: Integer;
  ObjType: TRttiType;
  JSONElementType: TJSONtype;
begin
  if (J.Count = 0) then
    EXIT;

  ObjType := ctx.GetType(Obj.ClassInfo);
  JSONElementType := J.Elements[0].SelfType;

  //Find a matching add method
  AddMethod := FindCompatibleAddMethod( ObjType, JSONElementType, Parameter );

  //Did we find a suitable Add method
  If Assigned(AddMethod) then
  Begin
    if JSONElementType = jtObject then
    Begin
      ItemType := Parameter.ParamType;
      ItemConstructorMethod := ItemType.GetMethod('Create');
      //Create items and parse values into items
      for i := 0 to J.Count-1 do
      Begin
        //Create item
        NewItem := ItemConstructorMethod.Invoke( ItemType.AsInstance.MetaclassType, [] );
        //Add item to list
        AddMethod.Invoke( Obj, [NewItem] );
        //Read item's values from JSON
        ParseObject( J.Elements[i]  as TJSON_Object, NewItem.AsObject );
      End;
    End
    Else If IsDateType(Parameter.ParamType) and (JSONElementType = jtString) then
    Begin
      for i := 0 to J.Count-1 do
        AddMethod.Invoke( Obj, [ TValue.From<double>( ParseDateTime( TJSON_String(J.Elements[i]).Value) ) ] );
    End
    Else If (Parameter.ParamType.TypeKind in [tkChar, tkWChar]) and (JSONElementType = jtString) then
    Begin
      for i := 0 to J.Count-1 do
        if TJSON_String(J.Elements[i]).Value.Length >= 1 then
          AddMethod.Invoke( Obj, [ TValue.From<char>( TJSON_String(J.Elements[i]).Value[1] ) ] );
    End
    Else If (JSONElementType = jtString) then
    Begin
      for i := 0 to J.Count-1 do
        AddMethod.Invoke( Obj, [ TValue.From<string>( TJSON_String(J.Elements[i]).Value ) ] );
    End
    Else If (JSONElementType = jtNumber) then
    Begin
      Case AddMethod.getparameters[0].ParamType.typekind of
        tkInteger:
          for i := 0 to J.Count-1 do
            AddMethod.Invoke( Obj, [ TValue.From<Integer>( TJSON_Number(J.Elements[i]).AsInteger ) ] );
        tkInt64:
          for i := 0 to J.Count-1 do
            AddMethod.Invoke( Obj, [ TValue.From<Int64>( TJSON_Number(J.Elements[i]).AsInt64 ) ] );
        tkFloat:
          for i := 0 to J.Count-1 do
            AddMethod.Invoke( Obj, [ TValue.From<Extended>( TJSON_Number(J.Elements[i]).Value ) ] );
      End;
    End
    Else //Number, Boolean
    Begin
      for i := 0 to J.Count-1 do
        AddMethod.Invoke( Obj, [ TValue.From<Boolean>( TJSON_Boolean(J.Elements[i]).Value ) ] );
    End;
  End;
end;

Class procedure TJSONParser.ParseListProperty(J: TJSON_Array; Obj: TObject; ObjProp: TRttiProperty);
var
  TheList: TValue;
begin
  if (ObjProp.PropertyType.TypeKind = tkClass) then
  Begin
    TheList := ObjProp.GetValue(Obj);
    ParseList( J, TheList.AsObject );
  End;
end;

Class procedure TJSONParser.ParseObject(J: TJSON_Object; Obj: TObject);
var
  I: Integer;
  ObjType: TRttiType;
  Pair: TJSON_Pair;
begin
  ObjType := ctx.GetType(Obj.ClassInfo);
  for I := 0 to J.Count-1 do
  Begin
    Pair := J.Pairs[i];
    ParseProperty( Pair.Value, Pair.Name, Obj, ObjType );
  End;
end;

Class procedure TJSONParser.ParseProperty(J: TJSON_Element; PropertyName: String; Obj: TObject; ObjType: TRttiType);
var
  ObjProp: TRttiProperty;
  UPropertyName: String;
begin
  UPropertyName := UpperCase(PropertyName);
  for ObjProp in ObjType.GetProperties do
    if (Uppercase(ObjProp.Name) = UPropertyName) then
    Begin
      if J.SelfType = jtNumber then
        SetNumberProperty( J as TJSON_Number, Obj, ObjProp )
      else if J.SelfType = jtString then
        SetStringProperty( J as TJSON_String, Obj, ObjProp )
      else if J.SelfType = jtBoolean then
        SetBooleanProperty( J as TJSON_Boolean, Obj, ObjProp )
      else if J.SelfType = jtNull then
        SetNullProperty( ObjProp )
      else if J.SelfType = jtArray then
        ParseListProperty( J as TJSON_Array, Obj, ObjProp )
      else if J.SelfType = jtObject then
        ParseObject( J  as TJSON_Object, ObjProp.GetValue(Obj).AsObject )
    End;
end;

Class procedure TJSONParser.SetBooleanProperty(J: TJSON_Boolean; Obj: TObject; ObjProp: TRttiProperty);
begin
  if IsBoolean(ObjProp.PropertyType) then
     ObjProp.SetValue( Obj, TValue.From<boolean>( J.Value ) );
end;

Class procedure TJSONParser.SetNullProperty(ObjProp: TRttiProperty);
begin

end;

Class procedure TJSONParser.SetNumberProperty(J: TJSON_Number; Obj: TObject; ObjProp: TRttiProperty);
begin
  Case ObjProp.PropertyType.TypeKind of
    tkFloat:   ObjProp.SetValue( Obj, TValue.From<Extended>( J.Value ) );
    tkInteger: ObjProp.SetValue( Obj, TValue.From<integer>( J.AsInteger ) );
    tkInt64:   ObjProp.SetValue( Obj, TValue.From<Int64>( J.AsInt64 ) );
    tkVariant: ObjProp.SetValue( Obj, TValue.From<Extended>( J.Value ) );
  End;
end;

Class procedure TJSONParser.SetStringProperty(J: TJSON_String; Obj: TObject; ObjProp: TRttiProperty);
var
  EnumInt: Integer;
  Value: TValue;
begin
  Case ObjProp.PropertyType.TypeKind of
    tkUString, tkString, tkLString: ObjProp.SetValue( Obj, TValue.From<string>( J.Value ) );
    tkChar, tkWChar:                ObjProp.SetValue( Obj, TValue.From<char>( J.Value[1] ) );
    tkEnumeration:   //If ObjProp.PropertyType.QualifiedName <> 'System.Boolean' then
                     Begin
                       EnumInt := GetEnumValue( ObjProp.PropertyType.Handle, J.Value );
                       TValue.Make( EnumInt, ObjProp.PropertyType.Handle, Value );
                       ObjProp.SetValue( Obj, Value );
                     End
    else if IsDateType(ObjProp.PropertyType) then
            ObjProp.SetValue( Obj, TValue.From<double>( ParseDateTime(J.Value) ) );
  End;
end;

{ TJSONSerializer }

class function TJSONSerializer.IsListObject(AObject: TObject; var ListObjectDetails: TListObjectDetails): Boolean;
var
  ObjType, AddParameterType: TRttiType;
  ObjProperty: TRttiIndexedProperty;
  Method: TRttiMethod;
  GetMethodParameters: TArray<TRttiParameter>;
begin
  {
    1) Function Add( Obj: T ): Anytype
         Has Add method
         Must be named 'Add'
         Must have exactly one parameter of type T

    2) Property Count: Integer
         Has Count property
         Must be named 'Count'

    3) Property Something[ Index: Integer ]: T read GetMethod
         Has an indexed property of type T
         Index parameter must be of type Integer
         GetMethod must have exactly one parameter of type integer
  }
  ListObjectDetails.CountProperty := nil;
  ListObjectDetails.AddMethod     := nil;
  ListObjectDetails.GetMethod     := nil;
  ListObjectDetails.ElementType   := nil;

  ObjType := Ctx.GetType( AObject.ClassType );
  for Method in ObjType.GetMethods do
    //Part 1
    if (Method.Name = 'Add') and
       (Length(Method.GetParameters) = 1) then
    Begin
      AddParameterType := Method.GetParameters[0].ParamType;
      //Part 3
      for ObjProperty in ObjType.GetIndexedProperties do
        if ObjProperty.IsReadable and
           (ObjProperty.PropertyType.QualifiedName = AddParameterType.QualifiedName) then
        Begin
          GetMethodParameters := ObjProperty.ReadMethod.GetParameters;
          if (Length(GetMethodParameters) = 1) and
             (GetMethodParameters[0].ParamType.TypeKind = tkInteger) then
          Begin
            ListObjectDetails.ElementType := AddParameterType;
            ListObjectDetails.AddMethod := Method;
            ListObjectDetails.GetMethod := ObjProperty.ReadMethod;
          End;
        End;
    End;
  //Part 2
  ListObjectDetails.CountProperty := ObjType.GetProperty('Count');
  if (ListObjectDetails.CountProperty <> nil) and
     (ListObjectDetails.CountProperty.PropertyType.TypeKind <> tkInteger) then
    ListObjectDetails.CountProperty := nil;

  Result := ListObjectDetails.IsListObject;
end;

class function TJSONSerializer.Serialize( Obj: TObject): String;
var
  JSON: TJSON_Element;
begin
  JSON := ToJSON( Obj );
  Try
    Result := JSON.ToJSON;
  Finally
    JSON.Free;
  End;
end;

class function TJSONSerializer.ToJSON(Obj: TObject): TJSON_Element;
var
  ListObjectDetails: TListObjectDetails;
begin
  if IsListObject( Obj, ListObjectDetails ) then
    Result := SerializeObjectList( Obj, ListObjectDetails )
  Else
    Result := SerializeObject( Obj );
end;

class Function TJSONSerializer.SerializeObject( Obj: TObject ): TJSON_Object;
var
  ObjType: TRttiType;
  ObjProperty: TRttiProperty;
  Value: TValue;
begin
  Result := TJSON_Object.Create;
  ObjType := ctx.GetType(Obj.ClassInfo);
  for ObjProperty in ObjType.GetProperties do
    if (ObjProperty.Visibility in [mvPublic, mvPublished]) and
       ObjProperty.IsReadable then
    Begin
      if ( ObjProperty.IsWritable and
           (ObjProperty.PropertyType.TypeKind in [ tkInteger, tkEnumeration, tkFloat, tkChar, tkWChar, tkString,
                                                   tkLString, tkInt64, tkUString ] ) )
      or
         (ObjProperty.PropertyType.TypeKind = tkClass) then
      Begin
        Value := ObjProperty.GetValue(Obj);
        Result.Add( TJSON_Pair.Create( ObjProperty.Name, SerializeValue( Value, ObjProperty.PropertyType ) ) );
      End;
    End;
end;

class Function TJSONSerializer.SerializeObjectList(Obj: TObject; ListObjectDetails: TListObjectDetails): TJSON_Array;
var
  i, Cnt: Integer;
  Value: TValue;
begin
  Result := TJSON_Array.Create;
  Cnt := ListObjectDetails.CountProperty.GetValue(Obj).AsInteger;
  if Cnt = 0 then
    EXIT;

  for i := 1 to Cnt do
  Begin
    Value := ListObjectDetails.GetMethod.Invoke( Obj, [ TValue.From<integer>(i-1) ] );
    Result.Add( SerializeValue( Value, ListObjectDetails.ElementType ) );
  End;
end;

class function TJSONSerializer.SerializeValue(const Value: TValue; ValueType: TRttiType ): TJSON_Element;
var
  ListObjectDetails: TListObjectDetails;
begin
  Case Value.Kind of
    tkInteger: Result := TJSON_Number.CreateInteger( Value.AsInteger );
    tkInt64:   Result := TJSON_Number.CreateInteger( Value.AsInt64 );
    tkFloat:   if (ValueType.QualifiedName = 'System.TDateTime') or
                  (ValueType.QualifiedName = 'System.TTime') then
                 Result := TJSON_String.Create( FormatDateTime( 'yyyy-mm-dd"T"hh:mm:ss', Value.AsExtended, JSONFormat ) )
               else if (ValueType.QualifiedName = 'System.TDate') then
                 Result := TJSON_String.Create( FormatDateTime( 'yyyy-mm-dd', Value.AsExtended, JSONFormat ) )
               else
                 Result := TJSON_Number.Create( Value.AsExtended );
    tkChar, tkWChar, tkString, tkLString, tkUString:
      Result := TJSON_String.Create( Value.AsString );
    tkEnumeration:
      Begin
        If ValueType.QualifiedName = 'System.Boolean' then
        Begin
          if Value.AsOrdinal = 0 then
            Result := TJSON_Boolean.Create( False )
          Else
            Result := TJSON_Boolean.Create( True );
        End
        Else
          Result := TJSON_String.Create( GetEnumName( Value.TypeInfo, Value.AsOrdinal ) );
      End;
    tkClass:
      Begin
        if IsListObject( Value.AsObject, ListObjectDetails ) then
          Result := SerializeObjectList( Value.AsObject, ListObjectDetails )
        Else
          Result := SerializeObject( Value.AsObject );
      End
    Else
      Result := nil;
  End;
end;

{ TListObjectDetails }

function TListObjectDetails.IsListObject: Boolean;
begin
  Result := Assigned( CountProperty ) and
            Assigned( AddMethod ) and
            Assigned( GetMethod ) and
            Assigned( ElementType );
end;

initialization
  JSONFormat := FormatSettings;
  JSONFormat.DecimalSeparator := '.';
  JSONFormat.ThousandSeparator := ',';
  JSONFormat.DateSeparator := '-';
  JSONFormat.TimeSeparator := ':';
finalization
  if Init then
     ctx.Free;
end.
