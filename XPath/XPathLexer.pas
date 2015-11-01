unit XPathLexer;

interface

Uses
  System.Classes, System.SysUtils, System.Generics.Collections;

Const
  CH_WHITESPACE   : TSysCharSet = [' ', #9, #10, #13];
  CH_DIGITS       : TSysCharSet = ['0' .. '9'];
  CH_IDENT_FIRST  : TSysCharSet = ['A' .. 'Z', '_'];
  CH_IDENT        : TSysCharSet = ['0' .. '9', 'A' .. 'Z', '_', '-'];
  CH_SYMBOL_FIRST : TSysCharSet = ['@', '/', '*', '+', '-', '[', ']', '(', ')', '=', '<', '>', ':', '.', '!', '$', ',', '.'];

Type
  TTokenType = ( ttIdentifier, ttInteger, ttFloat, ttString,
                 ttParenStart, ttParenEnd, ttBracketStart, ttBracketEnd,
                 ttAt, ttColonColon, ttDot, ttDotDot, ttSlash, ttDoubleSlash, ttDollar, ttComma,
                 ttPlus, ttMinus, ttMultiply,
                 ttEquals, ttNotEquals, ttLess, ttLessEqual, ttGreater, ttGreaterEqual,
                 ttOr, ttAnd, ttDiv, ttIDiv, ttMod,
                 ttChild, ttDescendant, ttDescendantOrSelf, ttParent, ttSelf,
                 ttFollowingSibling, ttFollowing, ttNamespace, ttAncestor,
                 ttPrecedingSibling, ttPreceding, ttAncestorOrSelf,
                 ttDocumentNode, ttElement, ttAttribute, ttSchemaElement, ttSchemaAttribute,
                 ttProcessingInstruction, ttComment, ttText, ttNode,
                 ttEOF );
  TTokenTypeSet = Set of TTokenType;

Const
  StringTokens: TTokenTypeSet = [ ttOr, ttAnd, ttDiv, ttIDiv, ttMod,
                                  ttChild, ttDescendant, ttDescendantOrSelf, ttParent, ttSelf,
                                  ttFollowingSibling, ttFollowing, ttNamespace, ttAncestor,
                                  ttPrecedingSibling, ttPreceding, ttAncestorOrSelf,
                                  ttDocumentNode, ttElement, ttAttribute, ttSchemaElement, ttSchemaAttribute,
                                  ttProcessingInstruction, ttComment, ttText, ttNode ];
  SingleSymbols: TTokenTypeSet = [ ttParenStart, ttParenEnd, ttBracketStart, ttBracketEnd,
                                   ttAt, ttDot, ttSlash, ttDollar, ttComma,
                                   ttPlus, ttMinus, ttMultiply,
                                   ttEquals, ttLess, ttGreater ];
  DoubleSymbols: TTokenTypeSet = [ ttColonColon, ttDotDot, ttDoubleSlash, ttNotEquals, ttLessEqual, ttGreaterEqual ];

Type
  TTokenTypeHelper = Record Helper for TTokenType
    Function AsString: String;
    Const TokenStr : Array[TTokenType] of string = (
                 'Identifier', 'Integer', 'Float', 'String',
                 '(', ')', '[', ']',
                 '@', '::', '.', '..', '/', '//', '$', ',',
                 '+', '-', '*',
                 '=', '!=', '<', '<=', '>', '>=',
                 'or', 'and', 'div', 'idiv', 'mod',
                 'child', 'descendant', 'descendant-or-self', 'parent', 'self',
                 'following-sibling', 'following', 'namespace', 'ancestor',
                 'preceding-sibling', 'preceding', 'ancestor-or-self',
                 'document-node', 'element', 'attribute', 'schema-element', 'schema-attribute',
                 'processing-instruction', 'comment', 'text', 'node',
                 'EOF' );
  End;

  TToken = class
  Private
    FText      : String;
    FTokenType : TTokenType;
    FLine      : Integer;
    FIndex     : Integer;
    function GetAsFloat: Double;
    function GetAsInt: Int64;
  Public
    Property Text      : String     read FText;
    Property TokenType : TTokenType read FTokenType;
    Property Line      : Integer    read FLine;
    Property LineIndex : Integer    read FIndex;
    Property AsInt     : Int64      read GetAsInt;
    Property AsFloat   : Double     read GetAsFloat;

    Function Clone: TToken;

    Constructor Create( ATokenType: TTokenType; AText: String; ALine, AIndex: Integer );
  end;

  ELexerException = Class(Exception);
  EParseException = Class(Exception);

  TLexer = Class
  Private
    FTokenList  : TObjectList<TToken>;
    FToken      : TToken;
    FCurIdx     : Integer;
    Procedure Lex( AInput: String );
    Function ParseSymbol( AStr: String; ATokens: TTokenTypeSet; ALine, AIndex: Integer ): Boolean;
    Function StrInTokenSet( AStr: String; ATokens: TTokenTypeSet ): TTokenType;
  Public
    Property Token: TToken read FToken;
    Function EOF: Boolean;
    Function Peek: TToken;
    Function Remaining: Integer;
    Procedure Next;
    Procedure Eat( ATokenType: TTokenType );

    Constructor Create( AInput: String );
    Destructor Destroy; Override;
  End;

implementation

{ TToken }

function TToken.Clone: TToken;
begin
  Result := TToken.Create( FTokenType, FText, FLine, FIndex );
end;

constructor TToken.Create(ATokenType: TTokenType; AText: String; ALine, AIndex: Integer);
begin
  FText      := AText;
  FTokenType := ATokenType;
  FLine      := ALine;
  FIndex     := AIndex;
end;

function TToken.GetAsFloat: Double;
begin
  Result := StrToFloat( FText );
end;

function TToken.GetAsInt: Int64;
begin
  Result := StrToInt( FText );
end;

{ TLexer }

constructor TLexer.Create(AInput: String);
begin
  FCurIdx     := 0;
  FTokenList  := TObjectList<TToken>.Create;
  Lex( AInput );
end;

destructor TLexer.Destroy;
begin
  FTokenList.Free;
  inherited;
end;

procedure TLexer.Eat(ATokenType: TTokenType);
begin
  if Token.TokenType <> ATokenType then
    raise EParseException.CreateFmt( '"%s" expected', [ATokenType.AsString] );
  Next;
end;

function TLexer.EOF: Boolean;
begin
  Result := (FCurIdx >= FTokenList.Count) or (Token.TokenType = ttEOF);
end;

procedure TLexer.Lex( AInput: String );
var
  StartP, P, Line, Len, LineStart: Integer;
  LStr, Str, TwoChars: String;
  ExpectIdentifier: Boolean;
begin
  FTokenList.Clear;
  FToken    := nil;
  FCurIdx   := 0;
  LineStart := 1;
  Line      := 1;
  Len       := Length(AInput);
  if Len < 1 then
  Begin
    FTokenList.Add( TToken.Create( ttEOF, Str, Line, Len+1 ) );
    FToken := FTokenList[0];
    EXIT;
  End;
  P := 1;
  repeat
    Str := '';
    //Eat whitespace
    //while (P <= Len) and (AInput[P] in [' ', #9, #10, #13]) do
    while (P <= Len) and CharInSet( AInput[P], CH_WHITESPACE) do
    Begin
      if AInput[P] = #13 then
      Begin
        inc(Line);
        LineStart := P + 1;
      End;
      Inc(P);
    End;

    TwoChars := '';
    if (P + 1 <= Len) then
      TwoChars := Copy( AInput, P, 2 );

    if (P <= Len) then
    Begin
      //Comment
      If (TwoChars = '(:') then
      Begin
        while (P <= Len) and (TwoChars <> ':)') do
        Begin
          if AInput[P] = #13 then
          Begin
            inc(Line);
            LineStart := P + 1;
          End;
          Inc(P);
          TwoChars := Copy( AInput, P, 2 );
        End;
        Inc(P, 2);
      End
      //Tokens
      Else if (P <= Len) then
      Begin
        if CharInSet( AInput[P], CH_SYMBOL_FIRST ) then
        Begin
          if ParseSymbol( TwoChars, DoubleSymbols, Line, P-LineStart ) then
            inc(P,2)
          else if ParseSymbol( AInput[P], SingleSymbols, Line, P-LineStart ) then
            inc(P)
          else
            Raise ELexerException.CreateFmt( 'Unknown character: "%s" (Line %d, Index %d)', [AInput[P], Line, P-LineStart] );
        End
        //Numbers
        else if CharInSet( AInput[P], CH_DIGITS ) then //(AInput[P] in ['0' .. '9'])
        Begin
          StartP := P;

          //read integer part
          while (P <= Len) and CharInSet( AInput[P], CH_DIGITS ) do
          Begin
            Str := Str + AInput[P];
            inc(P);
          End;

          //Float (but not "..")
          if (P+1 <= Len) and (AInput[P] = '.') and CharInSet( AInput[P+1], CH_DIGITS ) then
          Begin
            str := str + '.';
            inc(p);
            while (P <= Len) and CharInSet( AInput[P], CH_DIGITS ) do
            Begin
              Str := Str + AInput[P];
              inc(P);
            End;
            FTokenList.Add( TToken.Create( ttFloat, Str, Line, StartP-LineStart ) );
          End
          //Integer
          else
            FTokenList.Add( TToken.Create( ttInteger, Str, Line, StartP-LineStart ) );
        End
        //Identifiers
        else if CharInSet( UpCase(AInput[P]), CH_IDENT_FIRST ) then
        Begin
          StartP := P;
          Str := Str + AInput[P];
          inc(P);

          while (P <= Len) and CharInSet( UpCase(AInput[P]), CH_IDENT ) do
          Begin
            Str := Str + AInput[P];
            inc(P);
          End;

          //Is reserved word?
          LStr := Lowercase( Str );
          ExpectIdentifier := False;
          If (FTokenList.Count > 0) then
          Begin
            ExpectIdentifier := ( (FTokenList.Last.TokenType = ttColonColon) and
                                  not ( ((LStr = 'text') or (LStr = 'node')) and
                                        (Copy(AInput, P, 1) = '(') ) );
            ExpectIdentifier := ExpectIdentifier
                                OR
                                ( (FTokenList.Last.TokenType in [ttSlash, ttDoubleSlash]) and
                                  not ( (Copy(AInput, P, 2) = '::') and
                                        (StrInTokenSet( LStr, [ ttChild, ttDescendant, ttDescendantOrSelf, ttParent, ttSelf,
                                                                ttFollowingSibling, ttFollowing, ttNamespace, ttAncestor,
                                                                ttPrecedingSibling, ttPreceding, ttAncestorOrSelf ] ) <> ttEOF) ) );
          End;

          If ExpectIdentifier or
             not ParseSymbol( LStr, StringTokens, Line, StartP-LineStart ) then
            FTokenList.Add( TToken.Create( ttIdentifier, Str, Line, StartP-LineStart ) );
        End
        //String
        else if (AInput[P] = '''') then
        Begin
          StartP := P;
          Inc(P);
          while (P <= Len) and not ((AInput[P] = '''') OR (AInput[P] = #13)) do
          Begin
            Str := Str + AInput[P];
            inc(P);
          End;
          if (P <= Len) and (AInput[P] = '''') then
          Begin
            FTokenList.Add( TToken.Create( ttString, Str, Line, StartP-LineStart ) );
            Inc(P);
          End
          else //if (P > Len) or (AInput[P] = #13) then
            raise ELexerException.CreateFmt( 'Unended string: "%s ... (Line %d, index %d)', [Str, Line, StartP-LineStart] );
        End
        Else
          raise ELexerException.CreateFmt( 'Illegal character: "%s" (Line %d, index %d)', [AInput[P], Line, P-LineStart] );
      End;
    End;
  until (P > Len);

  FTokenList.Add( TToken.Create( ttEOF, ttEOF.AsString, Line, Len+1 ) );
  FToken := FTokenList[0];
end;

procedure TLexer.Next;
begin
  inc( FCurIdx );
  if FCurIdx < FTokenList.Count then
    FToken := FTokenList[ FCurIdx ]
  Else
    FToken := nil;
end;

function TLexer.ParseSymbol(AStr: String; ATokens: TTokenTypeSet; ALine, AIndex: Integer): Boolean;
var
  TokenType: TTokenType;
begin
  TokenType := StrInTokenSet(AStr, ATokens);
  Result := (TokenType <> ttEOF);
  if Result then
    FTokenList.Add( TToken.Create( TokenType, AStr, ALine, AIndex ) );
end;

function TLexer.Peek: TToken;
begin
  if FCurIdx < FTokenList.Count-1 then
    Result := FTokenList[ FCurIdx + 1 ]
  else
    Result := FTokenList[ FTokenList.Count-1 ];
end;

function TLexer.Remaining: Integer;
begin
  Result := FTokenList.Count - (FCurIdx + 1);
end;

function TLexer.StrInTokenSet(AStr: String; ATokens: TTokenTypeSet): TTokenType;
var
  TokenType: TTokenType;
begin
  Result := ttEOF;
  For TokenType in ATokens do
    if TokenType.AsString = AStr then
      Result := TokenType;
end;

{ TTokenTypeHelper }

function TTokenTypeHelper.AsString: String;
begin
  Result := TokenStr[self];
end;

end.
