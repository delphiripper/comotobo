unit XPath;

interface

Uses
  System.Generics.Collections, HtmlParser, XPathLexer;

Type
  TXType = ( xBool, xInt, xFloat, xString, xNodelist );

  TXValue = Record
  Strict Private
    FInt    : Integer;
    FFloat  : Double;
    FString : String;
    FNodes  : TArray<TNode>;
  Public
    XType: TXType;
    Function AsInt: Integer;
    Function AsFloat: Double;
    Function AsString: String;
    Function AsNodes: TArray<TNode>;
    Function AsBoolean: Boolean;
    Function CanCoerceTo( AOperand: TXValue ): Boolean;
    Constructor CreateBoolean( AValue: Boolean );
    Constructor CreateInt( AValue: Integer );
    Constructor CreateFloat( AValue: Double );
    Constructor CreateString( AValue: String );
    Constructor CreateNodes( AValue: TArray<TNode> );
  End;

  TXContext = Record
    Document : TDom;
    Node     : TNode;
    Position : Integer;
    Constructor Create( ADocument: TDom; ANode: TNode; APosition: Integer );
  End;

  TXOperator = ( oNoOp,
                 oOr, oAnd,
                 oEqual, oNotEqual, oLess, oLessOrEqual, oGreater, oGreaterOrEqual, oAdd, oSubtract,
                 oMultiply, oDivide, oIntDivide, oModulo,
                 oNegate,
                 oStep );

  TXExpr = Class;

  TXEval = Class
    Public Function Operation( AOperand1: TXValue; AOperator: TXOperator; AOperand2: TXValue ): TXValue;
    Public Function Eval( AContext: TXContext ): TXValue; virtual; abstract;
  End;

  TXParameterList = Class
  Strict private
    FParameters: TObjectList<TXExpr>;
  Public
    Property Parameters: TObjectList<TXExpr> read FParameters;
    Function Eval( AContext: TXContext ): TList<TXValue>;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  TXPrimaryExpr = Class(TXEval)
  Public Type TPrimaryExprKind = ( kValue, kVar, kContext, kExpr, kFunctionCall );
  Strict private
    FKind          : TPrimaryExprKind;
    FToken         : TToken;
    FParameterList : TXParameterList;
    FXExpr         : TXExpr;
    function EvalFunction(AContext: TXContext): TXValue;
  Public
    Property Kind: TPrimaryExprKind read FKind;
    Function Eval( AContext: TXContext ): TXValue; override;
    Constructor CreateSimple( AToken: TToken );
    Constructor CreateFunctionCall( AToken: TToken; AParameterList: TXParameterList );
    Constructor CreateXExpr( AXExpr: TXExpr );
    Destructor Destroy; Override;
  End;

  TXExpr = Class(TXEval)
  Public Type TNodeExprKind = ( ekNegate, ekRoot, ekPath, ekExpr );
  Strict private
    FKind      : TNodeExprKind;
    FXOperand  : TXEval;
    FXChildren : TList< TPair<TXOperator,TXEval> >;
    Function EvalPath( AContext: TXContext ): TXValue;
    Function EvalExpr( AContext: TXContext ): TXValue;
  Public
    Property Kind: TNodeExprKind read FKind write FKind;
    Function Eval( AContext: TXContext ): TXValue; override;
    Procedure AddChild( AOp: TXOperator; AOperand: TXEval );
    Constructor CreateXExpr( AKind: TNodeExprKind; AXOperand: TXEval );
    Destructor Destroy; Override;
  End;

  TXNodeTest = Class(TXEval)
  Public Type TNodeTestKind = ( kWildcard, kIdentifier, kKindTest );
  Strict private
    FKind      : TNodeTestKind;
    FToken     : TToken;
    FTokenType : TTokenType;
    Procedure ProcessAxis( AAxis: TTokenType; ANodes: TNodeList; ANodeList: TNodeList );
    Procedure ProcessNode( AAxis: TTokenType; ANode: TNode; ANodeList: TNodeList );
  Public
    Property Kind: TNodeTestKind read FKind;
    Function Eval( AContext: TXContext ): TXValue; override; //Used in predicates
    Function EvalAxis( AAxis: TTokenType; AContext: TXContext ): TXValue;
    Constructor CreateWildcard;
    Constructor CreateIdentifier( AToken: TToken );
    Constructor CreateKindTest( ATokenType: TTokenType );
    Destructor Destroy; Override;
  End;

  TXStep = Class(TXEval)
  Strict private
    FAxis     : TTokenType;
    FNodeTest : TXNodeTest;
  Public
    Function Eval( AContext: TXContext ): TXValue; override;
    Constructor CreateAxisStep( AAxis: TTokenType; ANodeTest: TXNodeTest );
    Constructor Create( ANodeTest: TXNodeTest );
    Destructor Destroy; Override;
  End;

  TXStepExpr = Class(TXEval)
  Strict private
    FStep       : TXEval;
    FPredicates : TObjectList<TXExpr>;
  Public
    Function Eval( AContext: TXContext ): TXValue; override;
    Procedure AddPredicate( APredicate: TXExpr );
    Constructor Create( AStep: TXEval );
    Destructor Destroy; Override;
  End;

  TXPath = Class
  Public
    Class Function ParsePrimaryExpr( ALexer: TLexer ): TXPrimaryExpr;
    Class Function ParseParameterList( ALexer: TLexer ): TXParameterList;
    Class Function ParseNodeTest( ALexer: TLexer ): TXNodeTest;
    Class Function ParseStep( ALexer: TLexer ): TXEval;
    Class Function ParseStepExpr( ALexer: TLexer ): TXEval;
    Class Function ParseRelativePathExpr( ALexer: TLexer ): TXExpr;
    Class Function ParsePathExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseUnaryExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseMultiplicativeExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseAdditiveExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseComparisonExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseAndExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseExpr( ALexer: TLexer ): TXExpr;
    Class Function ParseXPath( XPath: String ): TXEval;

    Class Function Eval( Dom: TDom; XPath: String ): TXValue;
  End;

implementation

Uses
  System.SysUtils;

{ TXPath }

class function TXPath.Eval(Dom: TDom; XPath: String): TXValue;
var
  Path: TXEval;
begin
  Path := ParseXPath( XPath );
  Try
    Result := Path.Eval( TXContext.Create( Dom, Dom.Root, 1 ) );
  Finally
    Path.Free;
  End;
end;

class function TXPath.ParseAdditiveExpr(ALexer: TLexer): TXExpr;
var
  Operand: TXEval;
  Op: TXOperator;
begin
  Result := ParseMultiplicativeExpr( ALexer );
  If ALexer.Token.TokenType in [ttPlus, ttMinus] then
  Begin
    Result := TXExpr.CreateXExpr( ekExpr, Result );
    Try
      while ALexer.Token.TokenType in [ttPlus, ttMinus] do
      Begin
        Case ALexer.Token.TokenType of
          ttPlus  : Op := oAdd;
          ttMinus : Op := oSubtract;
          Else Op := oNoOp;
        end;
        if Op = oNoOp then
          raise EParseException.Create('Unknown additive operator');
        ALexer.Next;
        Operand := ParseMultiplicativeExpr( ALexer );
        Result.AddChild( Op, Operand );
      End;
    Except
      Result.Free;
      Raise;
    End;
  End;
end;

class function TXPath.ParseAndExpr(ALexer: TLexer): TXExpr;
var
  Operand: TXEval;
begin
  Result := ParseComparisonExpr( ALexer );
  if ALexer.Token.TokenType = ttAnd then
  Begin
    Result := TXExpr.CreateXExpr( ekExpr, Result );
    Try
      while ALexer.Token.TokenType = ttAnd do
      Begin
        ALexer.Next;
        Operand := ParseComparisonExpr( ALexer );
        Result.AddChild( oAnd, Operand );
      End;
    Except
      Result.Free;
      Raise;
    End;
  End;
end;

class function TXPath.ParseComparisonExpr(ALexer: TLexer): TXExpr;
var
  Operand: TXEval;
  Op: TXOperator;
begin
  Result := ParseAdditiveExpr( ALexer );
  If ALexer.Token.TokenType in [ttEquals, ttNotEquals, ttLess, ttLessEqual, ttGreater, ttGreaterEqual] then
  Begin
    Result := TXExpr.CreateXExpr( ekExpr, Result );
    Try
      while ALexer.Token.TokenType in [ttEquals, ttNotEquals, ttLess, ttLessEqual, ttGreater, ttGreaterEqual] do
      Begin
        Case ALexer.Token.TokenType of
          ttEquals       : Op := oEqual;
          ttNotEquals    : Op := oNotEqual;
          ttLess         : Op := oLess;
          ttLessEqual    : Op := oLessOrEqual;
          ttGreater      : Op := oGreater;
          ttGreaterEqual : Op := oGreaterOrEqual;
          else Op := oNoOp;
        end;
        if Op = oNoOp then
          raise EParseException.Create('Unknown comparison operator');
        ALexer.Next;
        Operand := ParseAdditiveExpr( ALexer );
        Result.AddChild( Op, Operand );
      End;
    Except
      Result.Free;
      Raise;
    End;
  End;
end;

class function TXPath.ParseExpr(ALexer: TLexer): TXExpr;
var
  Operand: TXEval;
begin
  Result := ParseAndExpr( ALexer );
  If ALexer.Token.TokenType = ttOr then
  Begin
    Result := TXExpr.CreateXExpr( ekExpr, Result );
    Try
      while ALexer.Token.TokenType = ttOr do
      Begin
        ALexer.Next;
        Operand := ParseAndExpr( ALexer );
        Result.AddChild( oOr, Operand );
      End;
    Except
      Result.Free;
      Raise;
    End;
  End;
end;

class function TXPath.ParseMultiplicativeExpr(ALexer: TLexer): TXExpr;
var
  Operand: TXEval;
  Op: TXOperator;
begin
  Result := ParseUnaryExpr( ALexer );
  if ALexer.Token.TokenType in [ttMultiply, ttDiv, ttIDiv, ttMod] then
  Begin
    Result := TXExpr.CreateXExpr( ekExpr, Result );
    Try
      while ALexer.Token.TokenType in [ttMultiply, ttDiv, ttIDiv, ttMod] do
      Begin
        Case ALexer.Token.TokenType of
          ttMultiply : Op := oMultiply;
          ttDiv      : Op := oDivide;
          ttIDiv     : Op := oIntDivide;
          ttMod      : Op := oModulo;
          Else Op := oNoOp;
        end;
        if Op = oNoOp then
          raise EParseException.Create('Unknown multiplicative operator');
        ALexer.Next;
        Operand := ParseUnaryExpr( ALexer );
        Result.AddChild( Op, Operand );
      End;
    Except
      Result.Free;
      Raise;
    End;
  End;
end;

class function TXPath.ParseNodeTest(ALexer: TLexer): TXNodeTest;
begin
  Result := nil;
  Try
    if ALexer.Token.TokenType = ttMultiply then
    Begin
      Result := TXNodeTest.CreateWildcard;
      ALexer.Next;
    End
    else if ALexer.Token.TokenType = ttIdentifier then
    Begin
      Result := TXNodeTest.CreateIdentifier( ALexer.Token );
      ALexer.Next;
    End
    else
    Begin
      Result := TXNodeTest.CreateKindTest( ALexer.Token.TokenType );
      ALexer.Next;
      ALexer.Eat( ttParenStart );
      ALexer.Eat( ttParenEnd );
    End;
  Except
    Result.Free;
    Raise;
  End;
end;

class function TXPath.ParseParameterList(ALexer: TLexer): TXParameterList;
begin
  Result := TXParameterList.Create;
  if not (ALexer.Token.TokenType in [ttEOF, ttParenEnd]) then
    Result.Parameters.Add( ParseExpr(ALexer) );

  while (ALexer.Token.TokenType = ttComma) do
  Begin
    ALexer.Eat( ttComma );
    Result.Parameters.Add( ParseExpr(ALexer) );
  End;
end;

class function TXPath.ParsePathExpr(ALexer: TLexer): TXExpr;
var
  ChildStep, Path: TXExpr;
  DescendantOrSelfStep: TXStep;
begin
  if ALexer.Token.TokenType = ttSlash then
  Begin
    ALexer.Next;
    Result := TXExpr.CreateXExpr( ekRoot, ParseRelativePathExpr( ALexer ) );
  End
  else if ALexer.Token.TokenType = ttDoubleSlash then
  Begin
    //Replace //ChildStep with /descendant-or-self::node()/ChildStep ( ROOT -> (DescendantOrSelfStep, [ChildStep]) )
    ALexer.Next;
    ChildStep := ParseRelativePathExpr( ALexer );
    DescendantOrSelfStep := TXStep.CreateAxisStep( ttDescendantOrSelf, TXNodeTest.CreateKindTest( ttNode ) );
    Path := TXExpr.CreateXExpr( ekPath, DescendantOrSelfStep );
    Path.AddChild( oStep, ChildStep );
    Result := TXExpr.CreateXExpr( ekRoot, Path );
  End
  else
    Result := ParseRelativePathExpr( ALexer );
end;

class function TXPath.ParsePrimaryExpr(ALexer: TLexer): TXPrimaryExpr;
var
  Expr: TXExpr;
  Ident: TToken;
  Parameters: TXParameterList;
begin
  Expr := nil;
  Result := nil;
  Parameters := nil;
  Try
    case ALexer.Token.TokenType of
      ttInteger, ttFloat, ttString:
        Begin
          Result := TXPrimaryExpr.CreateSimple( ALexer.Token );
          ALexer.Next;
        End;
      ttDot:
        Begin
          ALexer.Next;
          Result := TXPrimaryExpr.CreateXExpr( TXExpr.CreateXExpr( ekExpr, TXStep.CreateAxisStep( ttSelf, TXNodeTest.CreateKindTest( ttNode ) ) ) );
        End;
      ttDollar:
        Begin
          ALexer.Next;
          if ALexer.Token.TokenType <> ttIdentifier then
            raise EParseException.Create('Identifier expected');
          Result := TXPrimaryExpr.CreateSimple( ALexer.Token );
          ALexer.Next;
        End;
      ttParenStart:
        Begin
          ALexer.Next;
          Expr := ParseExpr( ALexer );
          ALexer.Eat( ttParenEnd );
          Result := TXPrimaryExpr.CreateXExpr( Expr );
        End;
      ttIdentifier:
        if ALexer.Peek.TokenType = ttParenStart then
        Begin
          Ident := ALexer.Token;
          ALexer.Next;
          ALexer.Eat( ttParenStart );
          Parameters := ParseParameterList( ALexer );
          ALexer.Eat( ttParenEnd );
          Result := TXPrimaryExpr.CreateFunctionCall( Ident, Parameters );
        End;
    end;
    if Result = nil then
      raise EParseException.Create('(PrimaryExpr) Unexpected token: ' + ALexer.Token.TokenType.AsString);
  Except
    Expr.Free;
    Parameters.Free;
    raise;
  End;
end;

class function TXPath.ParseRelativePathExpr(ALexer: TLexer): TXExpr;
begin
  Result := TXExpr.CreateXExpr( ekExpr, ParseStepExpr( ALexer ) );
  Try
    while ALexer.Token.TokenType in [ttSlash, ttDoubleSlash] do
    Begin
      Result.Kind := ekPath;
      If ALexer.Token.TokenType = ttSlash then
      Begin
        ALexer.Next;
        Result.AddChild( oStep, ParseStepExpr( ALexer ) );
      End
      else If ALexer.Token.TokenType = ttDoubleSlash then
      Begin
        ALexer.Next;
        Result.AddChild( oStep, TXExpr.CreateXExpr( ekPath, TXStep.CreateAxisStep( ttDescendantOrSelf, TXNodeTest.CreateKindTest( ttNode ) ) ) );
        Result.AddChild( oStep, ParseRelativePathExpr( ALexer ) );
      End
      Else
        raise EParseException.Create('Unknown step operator');
    End;
  Except
    Result.Free;
    Raise;
  End;
end;

class function TXPath.ParseStep(ALexer: TLexer): TXEval;
var
  Axis: TTokenType;
  NodeTest: TXNodeTest;
  Token: TToken;
begin
  // ".."
  if ALexer.Token.TokenType = ttDotDot then
  Begin
    ALexer.Next;
    Token := TToken.Create( ttNode, 'node', -1, -1 );
    Try
      Result := TXStep.CreateAxisStep( ttParent, TXNodeTest.CreateKindTest( Token.TokenType ) );
    Finally
      Token.Free;
    End;
  End
  // Axis "::" NodeTest
  else if ALexer.Token.TokenType in [ ttChild, ttDescendant, ttDescendantOrSelf, ttParent, ttSelf,
                                      ttAttribute, ttFollowingSibling, ttFollowing, ttNamespace,
                                      ttAncestor, ttPrecedingSibling, ttPreceding, ttAncestorOrSelf ] then
  Begin
    Axis := ALexer.Token.TokenType;
    ALexer.Next;
    ALexer.Eat( ttColonColon );
    Result := TXStep.CreateAxisStep( Axis, ParseNodeTest( ALexer ) );
  End
  // "@" NodeTest
  else if ALexer.Token.TokenType = ttAt then
  Begin
    ALexer.Next;
    Result := TXStep.CreateAxisStep( ttAttribute, ParseNodeTest( ALexer ) );
  End
  // NodeTest
  Else
  Begin
    NodeTest := ParseNodeTest( ALexer );
    If NodeTest.Kind = kIdentifier then
      Result := TXStep.CreateAxisStep( ttChild, NodeTest )
    else
      Result := TXStep.Create( NodeTest );
  End;
end;

class function TXPath.ParseStepExpr(ALexer: TLexer): TXEval;
var
  Predicate: TXExpr;
begin
  if (ALexer.Token.TokenType = ttIdentifier) and
     (ALexer.Peek.TokenType  <> ttParenStart) then
    Result := ParseStep( ALexer )
  else if ALexer.Token.TokenType in [ ttChild, ttDescendant, ttDescendantOrSelf, ttParent, ttSelf, ttAttribute,
                                      ttFollowingSibling, ttFollowing, ttNamespace, ttAncestor,
                                      ttPrecedingSibling, ttPreceding, ttAncestorOrSelf,
                                      ttAt, ttMultiply,
                                      ttDocumentNode, ttElement, ttAttribute, ttSchemaElement, ttSchemaAttribute,
                                      ttProcessingInstruction, ttComment, ttText, ttNode,
                                      ttDotDot ] then
    Result := ParseStep( ALexer )
  else
    Result := ParsePrimaryExpr( ALexer );
  try
    Result := TXStepExpr.Create( Result );
    While ALexer.Token.TokenType = ttBracketStart do
    Begin
      ALexer.Eat( ttBracketStart );
      Predicate := ParseExpr( ALexer );
      TXStepExpr(Result).AddPredicate( Predicate );
      ALexer.Eat( ttBracketEnd );
    End;
  except
    Result.Free;
    Raise;
  end;
end;

class function TXPath.ParseUnaryExpr(ALexer: TLexer): TXExpr;
var
  Negate: Boolean;
begin
  Negate := False;
  while ALexer.Token.TokenType in [ttPlus, ttMinus] do
  Begin
    if ALexer.Token.TokenType = ttMinus then
      Negate := Not Negate;
    ALexer.Next;
  End;
  Result := ParsePathExpr( ALexer );
  if Negate then
  Begin
    Result := TXExpr.CreateXExpr( ekNegate, Result );
    Result.AddChild( oNegate, nil );
  End;
end;

class function TXPath.ParseXPath(XPath: String): TXEval;
var
  Lexer: TLexer;
begin
  Lexer := TLexer.Create( XPath );
  Try
    Result := ParseExpr( Lexer );
  Finally
    Lexer.Free;
  End;
end;

{ TXValue }

function TXValue.AsBoolean: Boolean;
begin
  case XType of
    xBool,
    xInt      : Result := FInt <> 0;
    xFloat    : Result := FFloat <> 0;
    xString   : Result := FString <> '';
    xNodelist : Result := Length(FNodes) <> 0;
    else raise EParseException.Create('Cannot coerce to boolean');
  end;
end;

function TXValue.AsFloat: Double;
begin
  case XType of
    xBool,
    xInt      : Result := FInt;
    xFloat    : Result := FFloat;
    xString   : raise EParseException.Create('Cannot coerce string to float');
    else raise EParseException.Create('Cannot coerce nodelist to float');
  end;
end;

function TXValue.AsInt: Integer;
begin
  case XType of
    xBool,
    xInt      : Result := FInt;
    xFloat    : raise EParseException.Create('Cannot coerce float to int');
    xString   : raise EParseException.Create('Cannot coerce string to int');
    else raise EParseException.Create('Cannot coerce nodelist to int');
  end;
end;

function TXValue.AsNodes: TArray<TNode>;
begin
  case XType of
    xBool,
    xInt      : raise EParseException.Create('Cannot coerce int to nodelist');
    xFloat    : raise EParseException.Create('Cannot coerce float to nodelist');
    xString   : raise EParseException.Create('Cannot coerce string to nodelist');
    xNodelist : Result := FNodes;
    else raise EParseException.Create('Cannot coerce to nodelist');
  end;
end;

function TXValue.AsString: String;
begin
  case XType of
    xBool,
    xInt      : Result := IntToStr(FInt);
    xFloat    : Result := FloatToStr(FFloat);
    xString   : Result := FString;
    else raise EParseException.Create('Cannot coerce nodelist to string');
  end;
end;

function TXValue.CanCoerceTo(AOperand: TXValue): Boolean;
begin
  case XType of
    xBool,
    xInt,
    xFloat    : Result := (AOperand.XType in [xBool, xInt, xFloat]);
    xString   : Result := (AOperand.XType = xString);
    xNodelist : Result := (AOperand.XType = xNodelist);
    Else Result := False;
  end;
end;

constructor TXValue.CreateBoolean(AValue: Boolean);
begin
  if AValue then
    FInt := 1
  Else
    FInt := 0;
  FFloat  := 0;
  FString := '';
  FNodes  := nil;
  XType   := xBool;
end;

constructor TXValue.CreateFloat(AValue: Double);
begin
  FInt    := 0;
  FFloat  := AValue;
  FString := '';
  FNodes  := nil;
  XType   := xFloat;
end;

constructor TXValue.CreateInt(AValue: Integer);
begin
  FInt    := AValue;
  FFloat  := 0;
  FString := '';
  FNodes  := nil;
  XType   := xInt;
end;

constructor TXValue.CreateNodes(AValue: TArray<TNode>);
begin
  FInt    := 0;
  FFloat  := 0;
  FString := '';
  FNodes  := Copy( AValue, 0, Length(AValue) );
  XType   := xNodelist;
end;

constructor TXValue.CreateString(AValue: String);
begin
  FInt    := 0;
  FFloat  := 0;
  FString := AValue;
  FNodes  := nil;
  XType   := xString;
end;

{ TXContext }

constructor TXContext.Create(ADocument: TDom; ANode: TNode; APosition: Integer);
begin
  Document := ADocument;
  Node     := ANode;
  Position := APosition;
end;

{ TXPrimaryExpr }

constructor TXPrimaryExpr.CreateFunctionCall(AToken: TToken; AParameterList: TXParameterList);
begin
  FKind          := kFunctionCall;
  FToken         := AToken.Clone;
  FParameterList := AParameterList;
  FXExpr         := nil;
end;

constructor TXPrimaryExpr.CreateSimple(AToken: TToken);
begin
  FKind          := kValue;
  FToken         := AToken.Clone;
  FParameterList := nil;
  FXExpr         := nil;
  if AToken.TokenType = ttIdentifier then
    FKind := kVar
  Else if AToken.TokenType = ttDot then
    FKind := kContext;
end;

constructor TXPrimaryExpr.CreateXExpr(AXExpr: TXExpr);
begin
  FKind          := kExpr;
  FToken         := nil;
  FParameterList := nil;
  FXExpr         := AXExpr;
end;

destructor TXPrimaryExpr.Destroy;
begin
  FParameterList.Free;
  FToken.Free;
  FXExpr.Free;
  inherited;
end;

function TXPrimaryExpr.EvalFunction(AContext: TXContext): TXValue;

  Function AssertParameterCount( ACount: Integer ): Boolean;
  Begin
    if FParameterList.Parameters.Count <> ACount then
      raise EParseException.CreateFmt( FToken.Text + '(): Expected %d parameters', [ACount] );
    Result := True;
  End;

var
  ActualParameters: TList<TXValue>;
Begin
  ActualParameters := FParameterList.Eval( AContext );
  Try
    if FToken.Text = 'position' then
    Begin
      if AssertParameterCount(0) then
        Result := TXValue.CreateInt( AContext.Position );
    End
    else if FToken.Text = 'text' then
    Begin
      if AssertParameterCount(0) then
        Result := TXValue.CreateString( AContext.Node.Text )
    End
    else if FToken.Text = 'true' then
    Begin
      if AssertParameterCount(0) then
        Result := TXValue.CreateBoolean( True )
    End
    else if FToken.Text = 'false' then
    Begin
      if AssertParameterCount(0) then
        Result := TXValue.CreateBoolean( False );
    End
    else if FToken.Text = 'not' then
    Begin
      if AssertParameterCount(1) then
        Result := TXValue.CreateBoolean( not ActualParameters[0].AsBoolean );
    End
    else
      raise EParseException.Create( 'Unknown function: ' + FToken.Text );
  Finally
    ActualParameters.Free;
  End;
End;

function TXPrimaryExpr.Eval(AContext: TXContext): TXValue;
begin
  case FKind of
    kValue        : case FToken.TokenType of
                      ttInteger : Result := TXValue.CreateInt( FToken.AsInt );
                      ttFloat   : Result := TXValue.CreateFloat( FToken.AsFloat );
                      ttString  : Result := TXValue.CreateString( FToken.Text );
                    end;
    kVar          : raise EParseException.Create( 'Variables not supported' );
    kContext      : raise EParseException.Create( 'Context not supported' );
    kExpr         : Result := FXExpr.Eval( AContext );
    kFunctionCall : Result := EvalFunction( AContext );
  end;
end;

{ TXExpr }

procedure TXExpr.AddChild(AOp: TXOperator; AOperand: TXEval);
begin
  FXChildren.Add( TPair<TXOperator, TXEval>.Create( AOp, AOperand ) );
end;

constructor TXExpr.CreateXExpr(AKind: TNodeExprKind; AXOperand: TXEval);
begin
  FKind      := AKind;
  FXChildren := TList< TPair<TXOperator,TXEval> >.Create;
  FXOperand  := AXOperand;
end;

destructor TXExpr.Destroy;
var
  Pair: TPair<TXOperator,TXEval>;
begin
  for Pair in FXChildren do
    Pair.Value.Free;
  FXOperand.Free;
  FXChildren.Free;
  inherited;
end;

function TXExpr.EvalPath(AContext: TXContext): TXValue;
var
  Expr       : TXEval;
  NodeResult : TXValue;
  Node       : TNode;
  InputList  : TNodeList;
  NodeList   : TNodeList;
  Index      : Integer;
begin
  NodeList := TNodeList.Create(False);
  InputList := TNodeList.Create(False);
  Try
    Expr := FXOperand;
    InputList.AddRange( AContext.Node );
    Index := 0;
    Repeat
      for Node in InputList do
      Begin
        NodeResult := Expr.Eval( TXContext.Create( AContext.Document, Node, 1 ) );
        if (NodeResult.XType = xNodelist) then
          NodeList.AddRange( NodeResult.AsNodes );
      End;
      //More steps?
      Expr := nil;
      if Index < FXChildren.Count then
      Begin // InputList = NodeList;
        InputList.Clear;
        InputList.AddRange( NodeList.ToArray );
        NodeList.Clear;
        Expr := FXChildren[Index].Value;
        inc( Index );
      End;
    Until Expr = nil;
    Result := TXValue.CreateNodes( NodeList.ToArray );
  Finally
    NodeList.Free;
    InputList.Free;
  End;
end;

function TXExpr.EvalExpr(AContext: TXContext): TXValue;
var
  Pair: TPair<TXOperator,TXEval>;
  Operand: TXValue;
begin
  Result := FXOperand.Eval( AContext );
  for Pair in FXChildren do
  Begin
    Operand := Pair.Value.Eval(AContext);
    Result := Operation( Result, Pair.Key, Operand );
  End;
end;

function TXExpr.Eval(AContext: TXContext): TXValue;
begin
  case FKind of
    ekNegate : Result := Operation( FXOperand.Eval( AContext ), oNegate, TXValue.CreateBoolean( False ) );
    ekRoot   : Result := FXOperand.Eval( TXContext.Create( AContext.Document, AContext.Document.Root, 1 ) );
    ekPath   : Result := EvalPath( AContext );
    ekExpr   : Result := EvalExpr( AContext );
  end;
end;

{ TXEval }

function TXEval.Operation(AOperand1: TXValue; AOperator: TXOperator; AOperand2: TXValue): TXValue;
begin
  case AOperator of
    oNoOp   : Result := AOperand1;
    oOr     : Result := TXValue.CreateBoolean( AOperand1.AsBoolean or AOperand2.AsBoolean );
    oAnd    : Result := TXValue.CreateBoolean( AOperand1.AsBoolean and AOperand2.AsBoolean );
    oEqual: if not AOperand1.CanCoerceTo( AOperand2 ) then
              Result := TXValue.CreateBoolean( False )
            Else
            Case AOperand1.XType of
              xBool     : Result := TXValue.CreateBoolean( AOperand1.AsInt    = AOperand2.AsInt );
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    = AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  = AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString = AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
              //xNodelist : Result := TXValue.CreateBoolean( AOperand1.AsNodes  = AOperand2.AsNodes );
            End;
    oNotEqual: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    <> AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  <> AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString <> AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
            End;
    oLess:  Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    < AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  < AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString < AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
            End;
    oLessOrEqual: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    <= AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  <= AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString <= AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
            End;
    oGreater: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    > AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  > AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString > AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
            End;
    oGreaterOrEqual: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateBoolean( AOperand1.AsInt    >= AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateBoolean( AOperand1.AsFloat  >= AOperand2.AsFloat );
              xString   : Result := TXValue.CreateBoolean( AOperand1.AsString >= AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot compare nodelists');
            End;
    oAdd: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt(    AOperand1.AsInt    + AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateFloat(  AOperand1.AsFloat  + AOperand2.AsFloat );
              xString   : Result := TXValue.CreateString( AOperand1.AsString + AOperand2.AsString );
              xNodelist : raise EParseException.Create('Cannot add nodelists');
            End;
    oSubtract: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt(   AOperand1.AsInt    - AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateFloat( AOperand1.AsFloat  - AOperand2.AsFloat );
              xString   : raise EParseException.Create('Cannot subtract strings');
              xNodelist : raise EParseException.Create('Cannot subtract nodelists');
            End;
    oMultiply: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt(   AOperand1.AsInt    * AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateFloat( AOperand1.AsFloat  * AOperand2.AsFloat );
              xString   : raise EParseException.Create('Cannot multiply strings');
              xNodelist : raise EParseException.Create('Cannot multiply nodelists');
            End;
    oDivide: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateFloat( AOperand1.AsInt   / AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateFloat( AOperand1.AsFloat / AOperand2.AsFloat );
              xString   : raise EParseException.Create('Cannot divide strings');
              xNodelist : raise EParseException.Create('Cannot divide nodelists');
            End;
    oIntDivide: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt( AOperand1.AsInt   div AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateInt( Trunc(AOperand1.AsFloat / AOperand2.AsFloat) );
              xString   : raise EParseException.Create('Cannot divide strings');
              xNodelist : raise EParseException.Create('Cannot divide nodelists');
            End;
    oModulo: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt( AOperand1.AsInt   mod AOperand2.AsInt );
              xFloat    : Result := TXValue.CreateInt( Trunc(AOperand1.AsFloat) - Trunc(AOperand1.AsFloat / AOperand2.AsFloat) );
              xString   : raise EParseException.Create('Cannot divide strings');
              xNodelist : raise EParseException.Create('Cannot divide nodelists');
            End;
    oNegate: Case AOperand1.XType of
              xBool,
              xInt      : Result := TXValue.CreateInt(   -AOperand1.AsInt   );
              xFloat    : Result := TXValue.CreateFloat( -AOperand1.AsFloat );
              xString   : raise EParseException.Create('Cannot negate strings');
              xNodelist : raise EParseException.Create('Cannot negate nodelists');
            End;
  Else
    raise EParseException.Create('Unsupported operator');
  end;
end;

{ TXStep }

constructor TXStep.Create(ANodeTest: TXNodeTest);
begin
  FAxis     := ttEOF;
  FNodeTest := ANodeTest;
end;

constructor TXStep.CreateAxisStep(AAxis: TTokenType; ANodeTest: TXNodeTest);
begin
  FAxis     := AAxis;
  FNodeTest := ANodeTest;
end;

destructor TXStep.Destroy;
begin
  FNodeTest.Free;
  inherited;
end;

function TXStep.Eval(AContext: TXContext): TXValue;
begin
  If FAxis = ttEOF then
    Result := FNodeTest.Eval( AContext )
  else
    Result := FNodeTest.EvalAxis( FAxis, AContext );
end;

{ TXNodeTest }

constructor TXNodeTest.CreateIdentifier(AToken: TToken);
begin
  FKind  := kIdentifier;
  FToken := AToken.Clone;
  FTokenType := ttIdentifier;
end;

constructor TXNodeTest.CreateKindTest(ATokenType: TTokenType);
begin
  FKind  := kKindTest;
  FToken := nil;
  FTokenType := ATokenType;
end;

constructor TXNodeTest.CreateWildcard;
begin
  FKind  := kWildcard;
  FToken := nil;
  FTokenType := ttMultiply;
end;

destructor TXNodeTest.Destroy;
begin
  FToken.Free;
  inherited;
end;

function TXNodeTest.Eval(AContext: TXContext): TXValue;
begin
  Case FKind of
    kWildcard: Result := TXValue.CreateNodes( AContext.Node.Elements.ToArray );
    kKindTest:
      Case FTokenType of
        ttNode : Result := TXValue.CreateNodes( [AContext.Node] );
        ttText : Result := TXValue.CreateString( AContext.Node.Text );
        else raise EParseException.Create('KindTest not implemented');
      End;
    Else raise EParseException.Create('Kind test not implemented');
  End;
end;

function TXNodeTest.EvalAxis(AAxis: TTokenType; AContext: TXContext): TXValue;
var
  NodeList: TNodeList;
begin
  NodeList := TNodeList.Create(False);
  Try
    case AAxis of
      ttChild,
      ttDescendant:
        Begin
          ProcessAxis( AAxis, AContext.Node.Elements, NodeList );
          Result := TXValue.CreateNodes( NodeList.ToArray );
        End;
      ttDescendantOrSelf:
        Begin
          ProcessNode( AAxis, AContext.Node, NodeList );
          ProcessAxis( AAxis, AContext.Node.Elements, NodeList );
          Result := TXValue.CreateNodes( NodeList.ToArray );
        End;
      ttParent:
        Begin
          ProcessAxis( AAxis, AContext.Node.ParentNode.Elements, NodeList );
          Result := TXValue.CreateNodes( NodeList.ToArray );
        End;
      ttSelf:
        Begin
          ProcessNode( AAxis, AContext.Node, NodeList );
          Result := TXValue.CreateNodes( NodeList.ToArray );
        End;
//      ttFollowingSibling : ;
//      ttFollowing        : ;
//      ttNamespace        : ;
//      ttAncestor         : ;
//      ttPrecedingSibling : ;
//      ttPreceding        : ;
//      ttAncestorOrSelf   : ;
      ttAttribute:
        Begin
          If AContext.Node.HasAttribute(FToken.Text) then
            Result := TXValue.CreateString( AContext.Node.AttributeByName[ FToken.Text ] )
          else
            Result := TXValue.CreateBoolean( False );
        End
      else raise EParseException.Create('Axis not implemented');
    end;
  Finally
    NodeList.Free;
  End;
end;

procedure TXNodeTest.ProcessNode(AAxis: TTokenType; ANode: TNode; ANodeList: TNodeList);
begin
  Case FKind of
    kWildcard   : ANodeList.Add( ANode );
    kIdentifier : If (AAxis = ttAttribute) and (ANode.HasAttribute(FToken.Text)) then
                    ANodeList.Add( ANode )
                  else if (ANode.Name = FToken.Text) then
                    ANodeList.Add( ANode );
    kKindTest   :
      Case FTokenType of
        ttNode : ANodeList.Add( ANode );
        else raise EParseException.Create('KindTest not implemented');
      End;
    else raise EParseException.Create('Unknown NodeTest');
  End;
end;

procedure TXNodeTest.ProcessAxis(AAxis: TTokenType; ANodes, ANodeList: TNodeList);
var
  Node: TNode;
begin
  for Node in ANodes do
  Begin
    ProcessNode( AAxis, Node, ANodeList );
    if AAxis in [ttDescendant, ttDescendantOrSelf] then
      ProcessAxis( AAxis, Node.Elements, ANodeList );
  End;
end;

{ TXStepExpr }

procedure TXStepExpr.AddPredicate(APredicate: TXExpr);
begin
  FPredicates.Add( APredicate );
end;

constructor TXStepExpr.Create(AStep: TXEval);
begin
  FStep := AStep;
  FPredicates := TObjectList<TXExpr>.Create;
end;

destructor TXStepExpr.Destroy;
begin
  FStep.Free;
  FPredicates.Free;
  inherited;
end;

function TXStepExpr.Eval(AContext: TXContext): TXValue;
var
  Node: TNode;
  NodeList: TNodeList;
  Predicate: TXExpr;
  Included: Boolean;
  Position: Integer;
  PredResult: TXValue;
begin
  Result := FStep.Eval( AContext );
  if FPredicates.Count > 0 then
  Begin
    NodeList := TNodeList.Create(False);
    Try
      Position := 1;
      For Node in Result.AsNodes do
      Begin
        Included := True;
        For Predicate in FPredicates do
        Begin
          PredResult := Predicate.Eval( TXContext.Create( AContext.Document, Node, Position ) );
          if (PredResult.XType = xInt) then
            Included := Included and (PredResult.AsInt = Position) //position abbreviated syntax
          else
            Included := Included and PredResult.AsBoolean;
        End;
        Inc(Position);
        if Included then
          NodeList.Add( Node );
      End;
      Result := TXValue.CreateNodes( NodeList.ToArray );
    Finally
      NodeList.Free;
    End;
  End;
end;

{ TXParameterList }

constructor TXParameterList.Create;
begin
  FParameters := TObjectList<TXExpr>.Create(True);
end;

destructor TXParameterList.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TXParameterList.Eval(AContext: TXContext): TList<TXValue>;
var
  Parameter: TXExpr;
begin
  Result := TList<TXValue>.Create;
  for Parameter in FParameters do
    Result.Add( Parameter.Eval( AContext ) );
end;
end.
