unit TestLexer;

interface

uses
  TestFramework, XPathLexer;

type
  TestTLexer = class(TTestCase)
  strict private
    FLexer: TLexer;
    Procedure CheckToken( ATokenType: TTokenType; AStr: String ); Overload;
    Procedure CheckToken( ATokenType: TTokenType ); Overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLexer;
    procedure TestLexer_Uppercase;
    procedure TestLexer_Identifier_After_Slash;
    procedure TestLexer_Identifier_After_DoubleSlash;
    procedure TestLexer_Identifier_After_DoubleColon;
    procedure TestLexer_Text_After_DoubleColon;
  end;

implementation

procedure TestTLexer.CheckToken(ATokenType: TTokenType; AStr: String);
begin
  CheckTrue( FLexer.Token.TokenType = ATokenType, 'Expected token ' + FLexer.Token.TokenType.AsString +
                                                  ', found ' + ATokenType.AsString );
  CheckEquals( AStr, FLexer.Token.Text, 'Text' );
  FLexer.Next;
end;

procedure TestTLexer.CheckToken(ATokenType: TTokenType);
begin
  CheckToken( ATokenType, ATokenType.AsString );
end;

procedure TestTLexer.SetUp;
begin
  FLexer := nil;
end;

procedure TestTLexer.TearDown;
begin
  FLexer.Free;
end;

procedure TestTLexer.TestLexer;
begin
  FLexer := TLexer.Create( 'Identifier 25 0.25 ''Hello World'' ' +
                           '()[]' +
                           '@.::/..//$,' +
                           '+-*' +
                           '=!=<><=>=' +
                           'or and div idiv mod ' +
                           'child.descendant.descendant-or-self.parent.self.' +
                           'following-sibling.following.namespace.ancestor.' +
                           'preceding-sibling.preceding.ancestor-or-self ' +
                           'document-node element attribute schema-element schema-attribute ' +
                           'processing-instruction comment text node' );
  CheckToken( ttIdentifier, 'Identifier' );
  CheckToken( ttInteger,    '25' );
  CheckToken( ttFloat,      '0.25' );
  CheckToken( ttString,     'Hello World' );
  CheckToken( ttParenStart );
  CheckToken( ttParenEnd );
  CheckToken( ttBracketStart );
  CheckToken( ttBracketEnd );
  CheckToken( ttAt );
  CheckToken( ttDot );
  CheckToken( ttColonColon );
  CheckToken( ttSlash );
  CheckToken( ttDotDot );
  CheckToken( ttDoubleSlash );
  CheckToken( ttDollar );
  CheckToken( ttComma );
  CheckToken( ttPlus );
  CheckToken( ttMinus );
  CheckToken( ttMultiply );
  CheckToken( ttEquals );
  CheckToken( ttNotEquals );
  CheckToken( ttLess );
  CheckToken( ttGreater );
  CheckToken( ttLessEqual );
  CheckToken( ttGreaterEqual );
  CheckToken( ttOr );
  CheckToken( ttAnd );
  CheckToken( ttDiv );
  CheckToken( ttIDiv );
  CheckToken( ttMod );
  CheckToken( ttChild );
  CheckToken( ttDot );
  CheckToken( ttDescendant );
  CheckToken( ttDot );
  CheckToken( ttDescendantOrSelf );
  CheckToken( ttDot );
  CheckToken( ttParent );
  CheckToken( ttDot );
  CheckToken( ttSelf );
  CheckToken( ttDot );
  CheckToken( ttFollowingSibling );
  CheckToken( ttDot );
  CheckToken( ttFollowing );
  CheckToken( ttDot );
  CheckToken( ttNamespace );
  CheckToken( ttDot );
  CheckToken( ttAncestor );
  CheckToken( ttDot );
  CheckToken( ttPrecedingSibling );
  CheckToken( ttDot );
  CheckToken( ttPreceding );
  CheckToken( ttDot );
  CheckToken( ttAncestorOrSelf );
  CheckToken( ttDocumentNode );
  CheckToken( ttElement );
  CheckToken( ttAttribute );
  CheckToken( ttSchemaElement );
  CheckToken( ttSchemaAttribute );
  CheckToken( ttProcessingInstruction );
  CheckToken( ttComment );
  CheckToken( ttText );
  CheckToken( ttNode );
  CheckToken( ttEOF );
end;

procedure TestTLexer.TestLexer_Identifier_After_DoubleColon;
begin
  FLexer := TLexer.Create( '::text/' );
  CheckToken( ttColonColon );
  CheckToken( ttIdentifier, 'text' );
  CheckToken( ttSlash );
end;

procedure TestTLexer.TestLexer_Identifier_After_DoubleSlash;
begin
  FLexer := TLexer.Create( '//div div' );
  CheckToken( ttDoubleSlash );
  CheckToken( ttIdentifier, 'div' );
  CheckToken( ttDiv );
end;

procedure TestTLexer.TestLexer_Identifier_After_Slash;
begin
  FLexer := TLexer.Create( '/div div' );
  CheckToken( ttSlash );
  CheckToken( ttIdentifier, 'div' );
  CheckToken( ttDiv );
end;

procedure TestTLexer.TestLexer_Text_After_DoubleColon;
begin
  FLexer := TLexer.Create( '::text()' );
  CheckToken( ttColonColon );
  CheckToken( ttText );
end;

procedure TestTLexer.TestLexer_Uppercase;
begin
  FLexer := TLexer.Create( 'IDENTIFIER 25 0.25 ''HELLO WORLD'' ' +
                           '()[]' +
                           '@.::/..//$,' +
                           '+-*' +
                           '=!=<><=>=' +
                           'OR AND DIV IDIV MOD ' +
                           'CHILD.DESCENDANT.DESCENDANT-OR-SELF.PARENT.SELF.' +
                           'FOLLOWING-SIBLING.FOLLOWING.NAMESPACE.ANCESTOR.' +
                           'PRECEDING-SIBLING.PRECEDING.ANCESTOR-OR-SELF ' +
                           'DOCUMENT-NODE ELEMENT ATTRIBUTE SCHEMA-ELEMENT SCHEMA-ATTRIBUTE ' +
                           'PROCESSING-INSTRUCTION COMMENT TEXT NODE' );
  CheckToken( ttIdentifier, 'IDENTIFIER' );
  CheckToken( ttInteger,    '25' );
  CheckToken( ttFloat,      '0.25' );
  CheckToken( ttString,     'HELLO WORLD' );
  CheckToken( ttParenStart );
  CheckToken( ttParenEnd );
  CheckToken( ttBracketStart );
  CheckToken( ttBracketEnd );
  CheckToken( ttAt );
  CheckToken( ttDot );
  CheckToken( ttColonColon );
  CheckToken( ttSlash );
  CheckToken( ttDotDot );
  CheckToken( ttDoubleSlash );
  CheckToken( ttDollar );
  CheckToken( ttComma );
  CheckToken( ttPlus );
  CheckToken( ttMinus );
  CheckToken( ttMultiply );
  CheckToken( ttEquals );
  CheckToken( ttNotEquals );
  CheckToken( ttLess );
  CheckToken( ttGreater );
  CheckToken( ttLessEqual );
  CheckToken( ttGreaterEqual );
  CheckToken( ttOr );
  CheckToken( ttAnd );
  CheckToken( ttDiv );
  CheckToken( ttIDiv );
  CheckToken( ttMod );
  CheckToken( ttChild );
  CheckToken( ttDot );
  CheckToken( ttDescendant );
  CheckToken( ttDot );
  CheckToken( ttDescendantOrSelf );
  CheckToken( ttDot );
  CheckToken( ttParent );
  CheckToken( ttDot );
  CheckToken( ttSelf );
  CheckToken( ttDot );
  CheckToken( ttFollowingSibling );
  CheckToken( ttDot );
  CheckToken( ttFollowing );
  CheckToken( ttDot );
  CheckToken( ttNamespace );
  CheckToken( ttDot );
  CheckToken( ttAncestor );
  CheckToken( ttDot );
  CheckToken( ttPrecedingSibling );
  CheckToken( ttDot );
  CheckToken( ttPreceding );
  CheckToken( ttDot );
  CheckToken( ttAncestorOrSelf );
  CheckToken( ttDocumentNode );
  CheckToken( ttElement );
  CheckToken( ttAttribute );
  CheckToken( ttSchemaElement );
  CheckToken( ttSchemaAttribute );
  CheckToken( ttProcessingInstruction );
  CheckToken( ttComment );
  CheckToken( ttText );
  CheckToken( ttNode );
  CheckToken( ttEOF );
end;

initialization
  RegisterTest(TestTLexer.Suite);
end.

