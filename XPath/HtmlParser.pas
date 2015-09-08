unit HtmlParser;

interface

Uses
  System.SysUtils, System.Classes, System.Generics.Collections;

Type
  EDomException  = Class(Exception);

  TDom       = Class;
  TNode      = Class;
  TAttribute = Class;

  TNodeList      = TObjectList<TNode>;
  TAttributeList = TObjectList<TAttribute>;

  TAttribute = Class
  Strict Private
    FName  : String;
    FValue : String;
  Public
    Property Name  : String read FName  write FName;
    Property Value : String read FValue write FValue;
    Constructor Create( AName, AValue: String );
  End;

  TNode = Class
  Private
    FDom               : TDom;
    FParentNode        : TNode;
    FName              : String;
    FStartIndex        : Integer;
    FEndIndex          : Integer;
    FContentStartIndex : Integer;
    FContentEndIndex   : Integer;
    FAttributes        : TAttributeList;
    FElements          : TNodeList;
    function GetChildByName(AName: String): TNode;
    function GetAttributes: TAttributeList;
    function GetAttributeByName(AName: String): String;
    function GetPosition: Integer;
    procedure SetAttributeByName(AName: String; const Value: String);
    function GetCount: Integer;
    function GetChild(Index: Integer): TNode;
  Public
    Property StartIndex    : Integer        read FStartIndex;
    Property EndIndex      : Integer        read FEndIndex write FEndIndex;
    Property ContentStartIndex : Integer    read FContentStartIndex write FContentStartIndex;
    Property ContentEndIndex   : Integer    read FContentEndIndex write FContentEndIndex;
    Property ParentNode    : TNode          read FParentNode;
    Property Name          : String         read FName          write FName;
    Property Position      : Integer        read GetPosition;
    Property Attributes    : TAttributeList read GetAttributes;
    Property Elements      : TNodeList      read FElements;
    Property Count         : Integer        read GetCount;
    Property Children[ Index: Integer ]: TNode read GetChild; Default;
    Property AttributeByName[ AName: String ]: String read GetAttributeByName write SetAttributeByName;
    Property ChildByName[ AName: String ]: TNode read GetChildByName;
    Function Text: String;
    Function Source: String;

    Function Add( ANode: TNode ): Integer;
    Procedure Delete( Index: Integer );
    Function IndexOf( ChildName: String ) : Integer;
    Function FindAttribute( AAttributeName: String ): TAttribute;
    Function HasAttribute( AAttributeName: String ): Boolean;
    Function FindNodeWhere( AName: String; AttributeName: String = ''; AttributeValue: String = '' ): TNode;

    Function GetEnumerator: TEnumerator<TNode>;

    Constructor Create( ADom: TDom; AParentNode: TNode; AStartIndex: Integer );
    Destructor Destroy; Override;
  End;

  TDom = Class
  Strict Private
    P           : Integer;
    Len         : Integer;
    Raw         : String;
    FRoot       : TNode;
    FStrictMode : Boolean;

    Procedure EatWhitespace;
    Procedure Eat( Ch: Char );
    Procedure ReadAttribute( ANode: TNode );
    Procedure CheckEOF;
    Function ReadUntil( AStopChars: String ): String;
    Function ReadNode( AParent: TNode ): TNode;
    Function ReadVoidTag( AParent: TNode; AStartStr, AEndStr, AName: String ): TNode;
    Function ReadTag( AParent: TNode ): TNode;
    Function IsVoidTag( Const ATagName: String ): Boolean;
  Public
    Property Root       : TNode   read FRoot;
    Property StrictMode : Boolean read FStrictMode write FStrictMode;
    Property Document   : String  read Raw;

    Function FindNodeWhere( AName: String; AAttributeName, AAttributeValue: String ): TNode;

    Procedure LoadFromFile( AFilename: String );
    Procedure LoadFromString( AStr: String );
    Procedure LoadFromStrings( AStrings: TStrings );
    Procedure LoadFromStream( AStream: TStream );

    Constructor Create;
    Destructor Destroy; Override;
  End;

implementation

Const
  CR  = #13;
  LF  = #10;
  TAB = #09;
  WhiteSpace = ' ' + CR + LF + TAB;
  VoidTags : Array[0..16] of string = ( 'area', 'base', 'br', 'col', 'command',
                                        'embed', 'hr', 'img', 'input', 'keygen',
                                        'link', 'meta', 'param', 'source', 'track',
                                        'wbr', '!' );

{ TAttribute }

constructor TAttribute.Create(AName, AValue: String);
begin
  FName := AName;
  FValue := AValue;
end;

{ TNode }

function TNode.Add(ANode: TNode): Integer;
begin
  if FElements = nil then
    FElements := TNodeList.Create;

  Result := FElements.Add( ANode );
end;

constructor TNode.Create(ADom: TDom; AParentNode: TNode; AStartIndex: Integer);
begin
  FDom               := ADom;
  FParentNode        := AParentNode;
  FStartIndex        := AStartIndex;
  FEndIndex          := AStartIndex;
  FName              := '';
  FContentStartIndex := 0;
  FContentEndIndex   := 0;
  FAttributes        := TAttributeList.Create;
  FElements          := TNodeList.Create;
end;

procedure TNode.Delete(Index: Integer);
begin
  FElements.Delete(Index);
end;

destructor TNode.Destroy;
begin
  If FElements <> nil then FElements.Free;
  If FAttributes <> nil then FAttributes.Free;
  inherited;
end;

function TNode.FindAttribute(AAttributeName: String): TAttribute;
var
  Attribute: TAttribute;
begin
  Result := nil;
  for Attribute in FAttributes do
    if Lowercase(Attribute.Name) = Lowercase(AAttributeName) then
      EXIT( Attribute );
end;

function TNode.FindNodeWhere(AName, AttributeName, AttributeValue: String): TNode;
var
  i: Integer;
begin
  Result := nil;
  if (Name = AName) and
     (AttributeValue = '') or
     (AttributeByName[AttributeName] = AttributeValue) then
    EXIT( Self );

  i := 0;
  while (Result = nil) and (i < Count) do
  Begin
    Result := Children[i].FindNodeWhere( AName, AttributeName, AttributeValue );
    inc(i);
  End;
end;

function TNode.GetAttributeByName(AName: String): String;
var
  Attribute: TAttribute;
begin
  Result := '';
  if FAttributes = nil then
    EXIT;
  for Attribute in FAttributes do
    if Attribute.Name = AName then
      EXIT( Attribute.Value );
end;

function TNode.GetAttributes: TAttributeList;
begin
  if FAttributes = nil then
    FAttributes := TAttributeList.Create;
  Result := FAttributes;
end;

function TNode.GetChild(Index: Integer): TNode;
begin
  Result := FElements[Index];
end;

function TNode.GetChildByName(AName: String): TNode;
var
  Element: TNode;
begin
  for Element in FElements do
    if Element.Name = AName then
    Begin
      Result := Element;
      EXIT;
    End;
  Result := nil;
end;

function TNode.GetCount: Integer;
begin
  Result := FElements.Count;
end;

function TNode.GetEnumerator: TEnumerator<TNode>;
begin
  Result := FElements.GetEnumerator;
end;

function TNode.GetPosition: Integer;
begin
  if FParentNode <> nil then
    Result := FParentNode.FElements.IndexOf( Self ) + 1
  else
    Result := -1;
end;

function TNode.Text: String;
begin
  Result := '';
  if (FContentStartIndex <> 0) and (FContentEndIndex <> 0) and
     (FContentStartIndex <= FContentEndIndex) then
    Result := Copy( FDom.Document, FContentStartIndex, FContentEndIndex - FContentStartIndex + 1 );
end;

function TNode.HasAttribute(AAttributeName: String): Boolean;
var
  Attribute: TAttribute;
begin
  Attribute := FindAttribute( AAttributeName );
  Result := Attribute <> nil;
end;

Function TNode.IndexOf(ChildName: String) : Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FElements.Count - 1 do
    if FElements[i].Name = ChildName then
    Begin
      Result := i;
      EXIT;
    End;
end;

procedure TNode.SetAttributeByName(AName: String; const Value: String);
var
  i: Integer;
begin
  for i := 0 to Attributes.Count - 1 do
    if Attributes[i].Name = AName then
    Begin
      Attributes[i].Value := Value;
      EXIT;
    End;
  Attributes.Add( TAttribute.Create( AName, Value ) );
end;

function TNode.Source: String;
begin
  Result := '';
  if (FStartIndex <> 0) and (FEndIndex <> 0) and
     (FStartIndex <= FEndIndex) then
    Result := Copy( FDom.Document, FStartIndex, FEndIndex - FStartIndex + 1 );
end;

{ TDom }

procedure TDom.CheckEOF;
begin
  if (p > len) then
    Raise EDomException.Create( 'Unexpected end of file' );
end;

constructor TDom.Create;
begin
  FRoot := TNode.Create( self, nil, 1 );
  FStrictMode := True;
end;

destructor TDom.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TDom.Eat(Ch: Char);
begin
  CheckEOF;
  If (Raw[p] = Ch) then
    inc(p)
  Else
    raise EDomException.Create( '"' + Ch + '" expected at: ' + Copy( Raw, p, 100 ) );
end;

procedure TDom.EatWhitespace;
begin
  while (P <= Len) and CharInSet( Raw[P], [' ', TAB, LF, CR] ) do
    inc(P);
end;

function TDom.FindNodeWhere(AName, AAttributeName, AAttributeValue: String): TNode;
begin
  Result := Root.FindNodeWhere( AName, AAttributeName, AAttributeValue );
end;

function TDom.IsVoidTag(const ATagName: String): Boolean;
var
  AName: String;
begin
  Result := False;
  for AName in VoidTags do
    if AName = ATagName then
      Exit( True );
end;

procedure TDom.LoadFromFile(AFilename: String);
var
  Fil : TStringList;
begin
  Fil := TStringList.Create;
  Try
    Fil.LoadFromFile( AFilename );
    LoadFromStrings( Fil );
  Finally
    Fil.Free;
  End;
end;

procedure TDom.LoadFromStream(AStream: TStream);
var
  S: TStringList;
begin
  S := TStringList.Create;
  Try
    S.LoadFromStream( AStream );
    LoadFromStrings( S );
  Finally
    S.Free;
  End;
end;

procedure TDom.LoadFromString(AStr: String);
Var
  Node: TNode;
begin
  FRoot.Free;
  FRoot := TNode.Create( self, nil, 1 );
  FRoot.EndIndex          := Length(Raw);
  FRoot.ContentStartIndex := 1;
  FRoot.ContentEndIndex   := Length(Raw);

  Raw  := AStr;
  P    := 1;
  Len  := Length(Raw);
  EatWhitespace;
  Repeat
    Node := ReadNode( FRoot );
    if Node <> nil then
      FRoot.Add( Node );
    EatWhitespace;
  Until P > Length(Raw);
end;

procedure TDom.LoadFromStrings(AStrings: TStrings);
begin
  LoadFromString( AStrings.Text );
end;

procedure TDom.ReadAttribute( ANode: TNode );
var
  QuoteChar : Char;
  Name, Value : String;
  Done : Boolean;
begin
  Name := ReadUntil( '=' );
  Eat('=');
  if Raw[p] = '"' then
    QuoteChar := '"'
  else if Raw[p] = '''' then
    QuoteChar := ''''
  else If StrictMode then
    raise EDomException.Create( Format( 'Non strict attribute value: %s=', [ Name, Copy(Raw, p, 50) ] ) )
  else
    QuoteChar := #0;

  if QuoteChar <> #0 then
  Begin
    Eat( QuoteChar );
    Value := '';
    Repeat
      Value := Value + ReadUntil( QuoteChar );
      Eat( QuoteChar );
      Done := (Raw[p] <> QuoteChar);
      if not Done then
      Begin
        Eat( QuoteChar );
        Value := Value + QuoteChar;
      End;
    Until Done;
  End
  Else //NOT Strict: Read until whitespace
    Value := ReadUntil( '/>' + WhiteSpace );
  ANode.Attributes.Add( TAttribute.Create(Name, Value) );
end;

function TDom.ReadVoidTag(AParent: TNode; AStartStr, AEndStr, AName: String ): TNode;
var
  TagStart, ContentStart, L, i: Integer;
begin
  TagStart := P;
  for i := 1 to AStartStr.Length do
    Eat( AStartStr[i] );
  L := AEndStr.Length;
  ContentStart := P;
  while (P <= Len) and (Copy( Raw, P, L ) <> AEndStr) do
    inc(P);
  If (Copy( Raw, P, L ) = AEndStr) then
  Begin
    Result := TNode.Create( AParent.FDom, AParent, TagStart );
    Result.Name := AName;
    Result.ContentStartIndex := ContentStart;
    Result.ContentEndIndex   := P - 1;
    Inc( P, L );
    Result.EndIndex := P - 1;
  End
  else
    Raise EDomException.Create( 'Expected "' + AEndStr + '"' );
end;

Function TDom.ReadTag( AParent: TNode ): TNode;
//   TAG ::= "<" TAG_NAME [ ATTRIBUTE_LIST ] ">" ANY_TEXT "<" "/" TAGNAME ] ">"
//   TAG ::= "<" TAG_NAME [ ATTRIBUTE_LIST ] "/" ">"
//   ATTRIBUTE_LIST ::= ATT_NAME "=" """" VALUE """" [ATTRIBUTE_LIST]
//   ANY_TEXT ::= empty | ANY_ELEM [ANY_TEXT]
//   ANY_ELEM ::= TAG
//   ANY_ELEM ::= any
var
  S        : Integer;
  Name     : String;
  EndOfTag : Boolean;
  Tag      : TNode;
Begin
  Result := TNode.Create( AParent.FDom, AParent, P );
  Try
    Eat( '<' );
    EatWhitespace;
    Name := ReadUntil( '/>' + WhiteSpace );
    Result.Name := Lowercase(Name);
    EatWhitespace;
    while (p < len) and
          (Raw[p] <> '>') and
          (Raw[p] <> '/') do
    Begin
      ReadAttribute( Result );
      EatWhitespace;
    End;

    If IsVoidTag( Result.Name ) then
    Begin
      Result.Free;
      Result := nil;
      if Raw[p] = '/' then
        Eat( '/' );
    End
    else
    Begin
      if Raw[P] = '>' then
      Begin
        Eat( '>' );
        Result.ContentStartIndex := p;
        Repeat
          while (p < Len) and (Raw[p] <> '<') do
            Inc( p );
          S := P;
          Eat( '<' );
          EndOfTag := (Raw[p] = '/');
          if EndOfTag then
          Begin
            Eat( '/' );
            Name := ReadUntil( '/>' + WhiteSpace );
            Result.ContentEndIndex := S-1;
            If Lowercase(Name) <> Result.Name then
              //TODO: Try finishing a parent node
              raise EDomException.Create('Expected "</' + Result.Name + '>"');
          End
          Else if Pos( Raw[p], WhiteSpace ) <= 0 then // ignore "< "
          Begin
            P := S; //Backtrack
            Tag := ReadNode( Result );
            if Tag <> nil then
              Result.Add( Tag );
          End;
        Until EndOfTag;
      End;
      //End of tag
    End;
    Eat( '>' );
    if Result <> nil then
      Result.EndIndex := P-1;
  Except
    Result.Free;
    Raise;
  End;
End;

function TDom.ReadNode( AParent: TNode ): TNode;
begin
  CheckEOF;
  if Raw[p] <> '<' then
    raise EDomException.Create('Expected "<"');

  if Copy( Raw, p, 4 ) = '<!--' then
    Result := ReadVoidTag( AParent, '<!--', '-->', 'Comment' )
  else if Copy( Raw, p, 9 ) = '<![CDATA[' then
    Result := ReadVoidTag( AParent, '<![CDATA[', ']]>', 'CDATA' )
  else if Copy( Raw, p, 9 ) = '<!DOCTYPE' then
    Result := ReadVoidTag( AParent, '<!DOCTYPE', '>', 'DOCTYPE' )
  else if Copy( Raw, p, 2) = '<!' then
    Result := ReadVoidTag( AParent, '<!', '>', 'Unknown' )
  else
    Result := ReadTag( AParent );
end;

function TDom.ReadUntil(AStopChars: String): String;
var
  S : Integer;
begin
  S := P;
  while (P <= Len) and (Pos( Raw[P], AStopChars ) <= 0) do
    inc(P);
  If (P <= Len) and (Pos( Raw[P], AStopChars ) > 0) then
    Result := Copy( Raw, S, P-S )
  else
    raise EDomException.Create( 'Expected one of "' + AStopChars + '", found end-of-file' );
end;

end.
