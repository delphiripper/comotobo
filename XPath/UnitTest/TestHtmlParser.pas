unit TestHtmlParser;

interface

uses
  TestFramework, HtmlParser;

type
  TestTDom = class(TTestCase)
  strict private
    FDom: TDom;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadFromString;
  end;

implementation

procedure TestTDom.SetUp;
begin
  FDom := TDom.Create;
end;

procedure TestTDom.TearDown;
begin
  FDom.Free;
  FDom := nil;
end;

procedure TestTDom.TestLoadFromString;
var
  Node: TNode;
  Name: String;
begin
  FDom.LoadFromString( '<!DOCTYPE a="12">'+
                       '<html b="12"><body><p>Hello<br>World</p></body></html>' );
  CheckEquals( 2, FDom.Root.Count );
  Node := FDom.Root[0];
  Name := 'FDom.Root[0]';
  CheckEquals( '<!DOCTYPE a="12">', Node.Source,            Name + ': Source' );
  CheckEquals( ' a="12"',           Node.Text,              Name + ': Text' );
  CheckEquals( 0,                   Node.Attributes.Count,  Name + ': Attributes.Count' );
  CheckEquals( 0,                   Node.Count,             Name + ': Count' );
  Node := FDom.Root[1];
  Name := 'FDom.Root[1]';
  CheckEquals( '<html b="12"><body><p>Hello<br>World</p></body></html>', Node.Source,              Name + ': Source' );
  CheckEquals( '<body><p>Hello<br>World</p></body>',                     Node.Text,                Name + ': Text' );
  CheckEquals( 1,                                                        Node.Attributes.Count,    Name + ': Attributes.Count' );
  CheckEquals( 'b',                                                      Node.Attributes[0].Name,  Name + ': Attributes[0].Name' );
  CheckEquals( '12',                                                     Node.Attributes[0].Value, Name + ': Attributes[0].Value' );
  CheckEquals( 1,                                                        Node.Count,               Name + ': Count' );
  Node := FDom.Root[1].Children[0];
  Name := 'FDom.Root[1].Children[0]';
  CheckEquals( '<body><p>Hello<br>World</p></body>', Node.Source,              Name + ': Source' );
  CheckEquals( '<p>Hello<br>World</p>',              Node.Text,                Name + ': Text' );
  CheckEquals( 0,                                    Node.Attributes.Count,    Name + ': Attributes.Count' );
  CheckEquals( 1,                                    Node.Count,               Name + ': Count' );
  Node := FDom.Root[1].Children[0].Children[0];
  Name := 'FDom.Root[1].Children[0].Children[0]';
  CheckEquals( '<p>Hello<br>World</p>', Node.Source,              Name + ': Source' );
  CheckEquals( 'Hello<br>World',        Node.Text,                Name + ': Text' );
  CheckEquals( 0,                       Node.Attributes.Count,    Name + ': Attributes.Count' );
  CheckEquals( 0,                       Node.Count,               Name + ': Count' );
end;

initialization
  RegisterTest(TestTDom.Suite);
end.

