unit TestXPath;

interface

uses
  TestFramework, HtmlParser, XPath;

type
  TestTXPath = class(TTestCase)
  strict private
    FDom: TDom;
    FNodeList: TNodeList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Parser;
    procedure Test_Parser_Add;
    procedure Test_Parser_Subtract;
    procedure Test_Parser_Parenthesis;
    procedure Test_Parser_NodeStep;
    procedure Test_Parser_Function_Not;
    procedure Test_Child;
    procedure Test_Child_Abbreviated;
    procedure Test_Child_Child;
    procedure Test_Child_Child_Abbreviated;
    procedure Test_Self_Or_Descendant;
    procedure Test_Child_Self_Or_Descendant;
    procedure Test_Predicate_XPath;
    procedure Test_Predicate_Position_Abbreviated;
    procedure Test_Predicate_Position_Equal;
    procedure Test_Predicate_Position_NotEqual;
    procedure Test_Predicate_Position_Greater;
    procedure Test_Predicate_Position_GreaterEqual;
    procedure Test_Predicate_Position_Less;
    procedure Test_Predicate_Position_LessEqual;
    procedure Test_Predicate_Text;
    procedure Test_Predicate_Attribute;
    procedure Test_Predicate_Attribute_Value;
  end;

implementation

procedure TestTXPath.SetUp;
begin
  FDom := TDom.Create;
  FNodeList := TNodeList.Create( False );
end;

procedure TestTXPath.TearDown;
begin
  FNodeList.Free;
  FDom.Free;
  FDom := nil;
end;

procedure TestTXPath.Test_Child;
begin
  FDom.LoadFromString( '<!DOCTYPE a="12">'+
                       '<html b="12"><body><p>Hello<br>World</p></body></html>' );
  FNodeList.AddRange( TXPath.Eval( FDom, 'child::html' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = FDom.Root[1], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Child_Abbreviated;
begin
  FDom.LoadFromString( '<!DOCTYPE a="12">'+
                       '<html b="12"><body><p>Hello<br>World</p></body></html>' );
  FNodeList.AddRange( TXPath.Eval( FDom, '/html' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = FDom.Root[1], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Child_Child;
begin
  FDom.LoadFromString( '<!DOCTYPE a="12">'+
                       '<html b="12"><body><p>Hello<br>World</p></body></html>' );
  FNodeList.AddRange( TXPath.Eval( FDom, 'child::html/child::body' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = FDom.Root[1].Children[0], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Child_Child_Abbreviated;
begin
  FDom.LoadFromString( '<!DOCTYPE a="12">'+
                       '<html b="12"><body><p>Hello<br>World</p></body></html>' );
  FNodeList.AddRange( TXPath.Eval( FDom, '/html/body' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = FDom.Root[1].Children[0], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Child_Self_Or_Descendant;
var
  Html: TNode;
  Div1, Div2: TNode;
begin
  FDom.LoadFromString( '<html b="12">' +
                       '<p>Not me!</p>' +
                       '<div>' +
                         '<p>Hello</p>' +
                         '<p>Cruel</p>' +
                       '</div>' +
                       '<div>' +
                         '<p>World</p>' +
                       '</div>' +
                       '</html>' );
  Html := FDom.Root[0];
  Div1 := Html[1];
  Div2 := Html[2];
  FNodeList.AddRange( TXPath.Eval( FDom, '/html/div//p' ).AsNodes );
  CheckEquals( 3, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Div1.Children[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Div1.Children[1], 'NodeList[1]' );
  CheckTrue( FNodeList[2] = Div2.Children[0], 'NodeList[2]' );
end;

procedure TestTXPath.Test_Predicate_Position_Equal;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()=2]' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[1], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Parser;
var
  XPath: TXEval;
  Value: TXValue;
begin
  XPath := TXPath.ParseXPath( '-7' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xInt, 'XType' );
    CheckEquals( -7, Value.AsInt, 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Parser_Add;
var
  XPath: TXEval;
  Value: TXValue;
begin
  XPath := TXPath.ParseXPath( '22+20' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xInt, 'XType' );
    CheckEquals( 42, Value.AsInt, 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Parser_Function_Not;
var
  XPath: TXEval;
  Value: TXValue;
begin
  XPath := TXPath.ParseXPath( 'not(1)' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xBool, 'XType' );
    CheckEquals( False, Value.AsBoolean, 'Value' );
  Finally
    XPath.Free;
  End;

  XPath := TXPath.ParseXPath( 'not(0)' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xBool, 'XType' );
    CheckEquals( True, Value.AsBoolean, 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Parser_NodeStep;
var
  XPath: TXEval;
  Value: TXValue;
begin
  FDom.LoadFromString( '<a>first</a>' +
                       '<b>first</b>' +
                       '<c>first</c>' );
  XPath := TXPath.ParseXPath( '/b' );
  Try
    Value := XPath.Eval( TXContext.Create( FDom, FDom.Root, 0 ) );
    CheckTrue( Value.XType = xNodelist, 'XType' );
    CheckEquals( 1, Length(Value.AsNodes), 'Length(Value.AsNodes)' );
    CheckTrue( FDom.Root[1] =Value.AsNodes[0], 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Parser_Parenthesis;
var
  XPath: TXEval;
  Value: TXValue;
begin
  XPath := TXPath.ParseXPath( '102-(10+50)' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xInt, 'XType' );
    CheckEquals( 42, Value.AsInt, 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Parser_Subtract;
var
  XPath: TXEval;
  Value: TXValue;
begin
  XPath := TXPath.ParseXPath( '102-20-40' );
  Try
    Value := XPath.Eval( TXContext.Create( nil, nil, 0 ) );
    CheckTrue( Value.XType = xInt, 'XType' );
    CheckEquals( 42, Value.AsInt, 'Value' );
  Finally
    XPath.Free;
  End;
end;

procedure TestTXPath.Test_Predicate_Attribute;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p a="1">Goodbye</p>'+
                       '<p b="2">Hello</p>'+
                       '<p a="1">Cruel</p>'+
                       '<p a="1">World</p>'+
                       '<p b="1">World!</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[@a]' ).AsNodes );
  CheckEquals( 3, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[2], 'NodeList[1]' );
  CheckTrue( FNodeList[2] = Body[3], 'NodeList[2]' );
end;

procedure TestTXPath.Test_Predicate_Attribute_Value;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p a="1">Goodbye</p>'+
                       '<p a="2">Hello</p>'+
                       '<p a="1">Cruel</p>'+
                       '<p a="1">World</p>'+
                       '<p b="1">World!</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[@a=''1'']' ).AsNodes );
  CheckEquals( 3, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[2], 'NodeList[1]' );
  CheckTrue( FNodeList[2] = Body[3], 'NodeList[2]' );
end;

procedure TestTXPath.Test_Predicate_Position_Abbreviated;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[2]' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[1], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Predicate_Position_Greater;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()>1]' ).AsNodes );
  CheckEquals( 2, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[1], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[2], 'NodeList[1]' );
end;

procedure TestTXPath.Test_Predicate_Position_GreaterEqual;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()>=2]' ).AsNodes );
  CheckEquals( 2, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[1], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[2], 'NodeList[1]' );
end;

procedure TestTXPath.Test_Predicate_Position_Less;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()<3]' ).AsNodes );
  CheckEquals( 2, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[1], 'NodeList[1]' );
end;

procedure TestTXPath.Test_Predicate_Position_LessEqual;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()<=2]' ).AsNodes );
  CheckEquals( 2, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[1], 'NodeList[1]' );
end;

procedure TestTXPath.Test_Predicate_Position_NotEqual;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[position()!=2]' ).AsNodes );
  CheckEquals( 2, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body[2], 'NodeList[1]' );
end;

procedure TestTXPath.Test_Predicate_Text;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p[text()=''World'']' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body[2], 'NodeList[0]' );
end;

procedure TestTXPath.Test_Predicate_XPath;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//body[p]' ).AsNodes );
  CheckEquals( 1, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body, 'NodeList[0]' );
end;

procedure TestTXPath.Test_Self_Or_Descendant;
var
  Html: TNode;
  Body: TNode;
begin
  FDom.LoadFromString( '<html b="12"><body>' +
                       '<p>Hello</p>'+
                       '<p>Cruel</p>'+
                       '<p>World</p>'+
                       '</body></html>' );
  Html := FDom.Root[0];
  Body := Html.Children[0];
  FNodeList.AddRange( TXPath.Eval( FDom, '//p' ).AsNodes );
  CheckEquals( 3, FNodeList.Count, 'NodeList.Count' );
  CheckTrue( FNodeList[0] = Body.Children[0], 'NodeList[0]' );
  CheckTrue( FNodeList[1] = Body.Children[1], 'NodeList[1]' );
  CheckTrue( FNodeList[2] = Body.Children[2], 'NodeList[2]' );
end;

initialization
  RegisterTest(TestTXPath.Suite);
end.


