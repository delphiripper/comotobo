{
  JSON v1.0

  March 4th 2015

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

  This unit can generate Delphi Data Transfer Objects (DTO) in a unit (output)
  from a JSON string (input). The output unit will contain the DTO classes needed
  to hold the data in the JSON input.

  The purpose of the classes:
    - TDTOGenerator:   String (JSON) -> String (Delphi source file)

  Known limitations:
    - Cannot determine Delphi property type for JSON properties with null values
    - Cannot determine Delphi list element type for JSON arrays with no/null values
    - Does not support JSON lists with different element types
    - No detection of name clashes

  *******************************************************************************

  Changes:

  v1.1 2015-03-04 * first version

}
unit JSON2DTO;

interface

Uses
  System.SysUtils, System.Classes, System.Generics.Collections, DJSON;

Type
  EDTOException = Class(Exception);

{$REGION 'DTO Tree classes'}
  TKind = ( kString, kDatetime, kInteger, kFloat, kObject, kList, kBoolean, kUnknown, kUnsupported );

  TDTO_Object = class;
  TDTO_List = class;

  TDTO_Value = class
  Strict Private
    FName: String;
    FKind: TKind;
    FObj: TDTO_Object;
    FList: TDTO_List;
    procedure SetList(const Value: TDTO_List);
    procedure SetObj(const Value: TDTO_Object);
  Public
    Property Name: String read FName write FName;
    Property Kind: TKind read FKind write FKind;
    Property Obj: TDTO_Object read FObj write SetObj;
    Property List: TDTO_List read FList write SetList;
    Function ValidType: Boolean;

    Function IsSameAs( AValue: TDTO_Value ): Boolean;

    Procedure WriteInterface( Strings: TStrings; PreviousList: TObjectList<TDTO_Object> );
    Procedure WriteImplementation( Strings: TStrings );

    Constructor Create;
    Destructor Destroy; Override;
  end;

  TDTO_Property = class
  Strict Private
    FName: String;
    FValue: TDTO_Value;
  Public
    Property Name: String read FName write FName;
    Property Value: TDTO_Value read FValue;

    Function IsSameAs( AProperty: TDTO_Property ): Boolean;
    Function NeedsConstructor: Boolean;
    Procedure WriteInterface( Strings: TStrings; PreviousList: TObjectList<TDTO_Object> );
    Procedure WriteImplementation( Strings: TStrings );

    Constructor Create;
    Destructor Destroy; Override;
  end;

  TDTO_Object = class
  Strict Private
    FName: String;
    FProperties: TObjectList<TDTO_Property>;
    FNeedsConstructor: Boolean;
    FIsDuplicate: Boolean;
    Function NonReservedName( AName: String ): String;
    Function FindDuplicate( ObjList: TObjectList<TDTO_Object> ): TDTO_Object;
  Public
    Property Name: String read FName write FName;
    Property Properties: TObjectList<TDTO_Property> read FProperties;

    Function IsSameAs( Obj: TDTO_Object ): Boolean;
    Function FindProperty( AName: String ): TDTO_Property;
    Procedure WriteInterface( Strings: TStrings; PreviousList: TObjectList<TDTO_Object> );
    Procedure WriteImplementation( Strings: TStrings );

    Constructor Create;
    Destructor Destroy; Override;
  end;

  TDTO_List = class
  Strict Private
    FName: String;
    FElementType: TDTO_Value;
  Public
    Property Name: String read FName write FName;
    Property ElementType: TDTO_Value read FElementType;

    Function TypeName: String;
    Function IsSameAs( List: TDTO_List ): Boolean;
    Procedure WriteInterface( Strings: TStrings; PreviousList: TObjectList<TDTO_Object> );
    Procedure WriteImplementation( Strings: TStrings );

    Constructor Create;
    Destructor Destroy; Override;
  end;
{$ENDREGION}

  TDTOGenerator = Class
  Private
    {$REGION 'Private section'}
    Class Function JSONTypeStr( Typ: TJSONtype ): String;
    Class Function WidenNumber( Current: TKind; Next: TJSON_Number ): TKind;
    Class Function PrettyName( AName: String ): String;
    Class Procedure ParseList(   JSON: TJSON_Array; List: TDTO_List );
    Class Procedure ParseObject( JSON: TJSON_Object; Obj: TDTO_Object );
    Class Procedure ParseValue(  JSON: TJSON_Element; Value: TDTO_Value );
    Class Procedure WritePDO( Root: TDTO_Value; AUnitName: String; Strings: TStrings; AURL: String = '' );
    {$ENDREGION}
  Public
    ///	<summary>
    ///	  Make a Source file with DTO classes from TJSON string
    ///	</summary>
    Class Procedure Parse( Const JSON, ClassName, UnitName: String; Source: TStrings; Const URL: String = '' );
    Class Function TypeName(Value: TDTO_Value): String;
  End;

implementation

Uses
  System.TypInfo;

{ TDTOGenerator }

Class function TDTOGenerator.JSONTypeStr(Typ: TJSONtype): String;
begin
  Result := GetEnumName( TypeInfo(TJSONType), Ord(Typ) );
end;

Class procedure TDTOGenerator.WritePDO(Root: TDTO_Value; AUnitName: String; Strings: TStrings; AURL: String = '');
var
  PreviousList: TObjectList<TDTO_Object>;
begin
  PreviousList := TObjectList<TDTO_Object>.Create(False);
  Try
    if AURL <> '' then
    Begin
      Strings.Add( '// Based on JSON from: ' + AURL );
      Strings.Add( '' );
    End;
    Strings.Add( 'Unit ' + AUnitName + ';' );
    Strings.Add( '' );
    Strings.Add( 'Interface' );
    Strings.Add( '' );
    Strings.Add( 'Uses' );
    Strings.Add( '  System.Classes, System.Generics.Collections;' );
    Strings.Add( '' );
    Strings.Add( 'Type' );
    Root.WriteInterface( Strings, PreviousList );
    if Root.Kind = kList then
    Begin
      Strings.Add( '  ' + Root.List.Name + ' = ' + Root.List.TypeName + ';' );
      Strings.Add( '' );
    End;
    Strings.Add( 'Implementation' );
    Root.WriteImplementation(Strings);
    Strings.Add( '' );
    Strings.Add( 'End.' );
  Finally
    PreviousList.Free;
  End;
end;

class procedure TDTOGenerator.Parse(Const JSON, ClassName, UnitName: String; Source: TStrings; Const URL: String = '');
var
  JSONTree: TJSON_Element;
  DTOTree: TDTO_Value;
begin
  JSONTree := nil;
  DTOTree := nil;
  Try
    JSONTree := TJSON.ParseText( JSON );
    DTOTree := TDTO_Value.Create;
    DTOTree.Name := '*';
    Case JSONTree.SelfType of
      jtArray:
          Begin
            DTOTree.Kind := kList;
            DTOTree.List := TDTO_List.Create;
            DTOTree.List.Name := 'T' + ClassName;
            ParseList( JSONTree.AsArray, DTOTree.List );
          End;
      jtObject:
          Begin
            DTOTree.Kind := kObject;
            DTOTree.Obj := TDTO_Object.Create;
            DTOTree.Obj.Name := 'T' + ClassName;
            ParseObject( JSONTree.AsObject, DTOTree.Obj );
          End;
      else
        raise EDTOException.Create( 'Unsupported root type: ' + JSONTypeStr(JSONTree.SelfType) );
    End;
    WritePDO( DTOTree, UnitName, Source, URL );
  Finally
    JSONTree.Free;
    DTOTree.Free;
  End;
end;

class procedure TDTOGenerator.ParseList(JSON: TJSON_Array; List: TDTO_List);
var
  FirstElement, Element: TJSON_Element;
  FirstIndex, I: Integer;
begin
  if (JSON.Count > 0) then
  Begin
    FirstIndex := 0;
    if (List.ElementType.Kind = kUnknown) then
    Begin
      FirstElement := JSON.Elements[0];
      case FirstElement.SelfType of
        jtNumber: if FirstElement.AsNumber.IsInteger then
                    List.ElementType.Kind := kInteger
                  else
                    List.ElementType.Kind := kFloat;
        jtString:
          Begin
            List.ElementType.Kind := kString;
            if TJSONParser.IsDatetime(FirstElement.AsString.Value) then
              List.ElementType.Kind := kDatetime;
          End;
        jtBoolean: List.ElementType.Kind := kBoolean;
        jtNull:    List.ElementType.Kind := kUnknown;
        jtArray:
          Begin
            List.ElementType.Kind := kList;
            List.ElementType.List := TDTO_List.Create;
            ParseList( FirstElement.AsArray, List.ElementType.List );
          End;
        jtObject:
          Begin
            List.ElementType.Kind := kObject;
            List.ElementType.Obj := TDTO_Object.Create;
            List.ElementType.Obj.Name := List.Name + 'Item';
            ParseObject( FirstElement.AsObject, List.ElementType.Obj );
          End;
        else raise EDTOException.Create('Invalid array element: ' + JSONTypeStr(FirstElement.SelfType) );
      end;
      FirstIndex := 1;
    End;
    //Check and Widen
    if List.ElementType.Kind <> kUnsupported then
    Begin
      for I := FirstIndex to JSON.Count-1 do
      Begin
        Element := JSON.Elements[i];
        case List.ElementType.Kind of
          kString: if Element.SelfType <> jtString then
                     List.ElementType.Kind := kUnsupported;
          kDatetime: if Element.SelfType <> jtString then
                       List.ElementType.Kind := kUnsupported
                     else if not TJSONParser.IsDatetime(Element.AsString.Value) then
                       List.ElementType.Kind := kString;
          kInteger: if Element.SelfType <> jtNumber then
                      List.ElementType.Kind := kUnsupported
                    else if not Element.AsNumber.IsInteger then
                      List.ElementType.Kind := kFloat;
          kFloat: if Element.SelfType <> jtNumber then
                    List.ElementType.Kind := kUnsupported;
          kBoolean: if Element.SelfType <> jtBoolean then
                      List.ElementType.Kind := kUnsupported;
          kList:   ParseList( Element.AsArray, List.ElementType.List );
          kObject: ParseObject( Element.AsObject, List.ElementType.Obj );
        end;
      End;
    End;
  End;
end;

Class procedure TDTOGenerator.ParseObject(JSON: TJSON_Object; Obj: TDTO_Object);
var
  i: Integer;
  Pair: TJSON_Pair;
  Prop: TDTO_Property;
begin
  for i := 0 to JSON.Count-1 do
  Begin
    Pair := JSON.Pairs[i];
    Prop := Obj.FindProperty( Pair.Name );
    if Prop = nil then
    Begin
      Prop := TDTO_Property.Create;
      Prop.Name := PrettyName( Pair.Name );
      ParseValue( Pair.Value, Prop.Value );
      Obj.Properties.Add( Prop );
      //Set sub object names from property name
      if Prop.Value.Kind = kObject then
      Begin
        Prop.Name := PrettyName( Pair.Name );
        Prop.Value.Name := 'T' + Prop.Name;
        Prop.Value.Obj.Name := 'T' + Prop.Name;
      End
      else if (Prop.Value.Kind = kList) and
              (Prop.Value.List.ElementType.Kind = kObject) then
      Begin
        Prop.Name := PrettyName( Pair.Name );
        Prop.Value.Name := 'T' + Prop.Name;
        Prop.Value.List.Name := 'T' + Prop.Name;
        Prop.Value.List.ElementType.Name := 'T' + Prop.Name + 'Item';
        Prop.Value.List.ElementType.Obj.Name := 'T' + Prop.Name + 'Item';
      End;
    End
    Else
    Begin
      case Prop.Value.Kind of
        kString: If (Pair.Value.SelfType <> jtString) and (not Pair.Value.IsNull) then //ignore null values
                   Prop.Value.Kind := kUnsupported;
        kDatetime: If not (Pair.Value.SelfType in [jtString, jtNull]) then //ignore null values
                     Prop.Value.Kind := kUnsupported
                   else If (Pair.Value.SelfType = jtString) and
                           not TJSONParser.IsDatetime(Pair.Value.AsString.Value) then
                     Prop.Value.Kind := kString;
        kInteger: If Pair.Value.SelfType <> jtNumber then
                    Prop.Value.Kind := kUnsupported
                  else If not Pair.Value.AsNumber.IsInteger then
                    Prop.Value.Kind := kFloat;
        kFloat: If Pair.Value.SelfType <> jtNumber then
                  Prop.Value.Kind := kUnsupported;
        kObject: If Pair.Value.SelfType <> jtObject then
                   Prop.Value.Kind := kUnsupported
                 Else
                   ParseValue( Pair.Value, Prop.Value );
        kList: If Pair.Value.SelfType <> jtArray then
                 Prop.Value.Kind := kUnsupported
               Else
                 ParseValue( Pair.Value, Prop.Value );
        kBoolean: If Pair.Value.SelfType <> jtBoolean then
                    Prop.Value.Kind := kUnsupported;
        kUnknown: ParseValue( Pair.Value, Prop.Value );
        kUnsupported: ;
      end;
    End;
  End;
end;

Class procedure TDTOGenerator.ParseValue(JSON: TJSON_Element; Value: TDTO_Value);
begin
  case JSON.SelfType of
    jtNumber:  Value.Kind := WidenNumber( kInteger, JSON.AsNumber );
    jtString:
        Begin
          Value.Kind := kString;
          if TJSONParser.IsDateTime(JSON.AsString.Value) then
            Value.Kind := kDatetime;
        End;
    jtBoolean: Value.Kind := kBoolean;
    jtNull:    Value.Kind := kUnknown;
    jtArray:
        Begin
          Value.Kind := kList;
          if Value.List = nil then //First parse of object?
            Value.List := TDTO_List.Create;
          ParseList( JSON.AsArray, Value.List );
        End;
    jtObject:
        Begin
          Value.Kind := kObject;
          if Value.Obj = nil then //First parse of object?
            Value.Obj := TDTO_Object.Create;
          ParseObject( JSON.AsObject, Value.Obj );
        End;
    jtElement, jtPair: raise EDTOException.Create( 'Invalid JSON element: ' + JSONTypeStr(JSON.SelfType) );
  end;
end;

Class function TDTOGenerator.PrettyName(AName: String): String;
begin
  Result := Copy(AName, 1, 1).ToUpper + Copy(AName, 2, MaxInt);
end;

class function TDTOGenerator.TypeName(Value: TDTO_Value): String;
begin
  case Value.Kind of
    kString   : Result := 'String';
    kDatetime : Result := 'TDateTime';
    kInteger  : Result := 'Integer';
    kFloat    : Result := 'Double';
    kObject   : Result := Value.Obj.Name;
    kList     : Result := Value.List.TypeName;
    kBoolean  : Result := 'Boolean';
    kUnknown  : Result := '?';
    kUnsupported: Result := '?';
  end;
end;

Class function TDTOGenerator.WidenNumber(Current: TKind; Next: TJSON_Number): TKind;
Begin
  if Current = kUnknown then
    Result := kInteger
  else
    Result := Current;
  if (Result = kInteger) and not Next.IsInteger then
    Result := kFloat;
End;

{ TDTO_Object }

constructor TDTO_Object.Create;
begin
  FName := 'TDTOItem';
  FProperties := TObjectList<TDTO_Property>.Create;
  FNeedsConstructor := False;
  FIsDuplicate      := False;
end;

destructor TDTO_Object.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TDTO_Object.FindDuplicate(ObjList: TObjectList<TDTO_Object>): TDTO_Object;
var
  Obj: TDTO_Object;
begin
  for Obj in ObjList do
    if IsSameAs(Obj) then
      Exit( Obj );
  Result := nil;
end;

function TDTO_Object.FindProperty(AName: String): TDTO_Property;
var
  Prop: TDTO_Property;
begin
  for Prop in FProperties do
    if SameText( Prop.Name, AName ) then
      Exit( Prop );
  Result := nil;
end;

function TDTO_Object.IsSameAs(Obj: TDTO_Object): Boolean;
var
  i: Integer;
  Prop, OtherProp: TDTO_Property;
begin
  Result := (FProperties.Count = Obj.Properties.Count);
  if Result then
    for i := 0 to Obj.Properties.Count-1 do
    Begin
      Prop := Properties[i];
      OtherProp := Obj.Properties[i];
      if not Prop.IsSameAs( OtherProp ) then
        Exit( False );
    End;
end;

function TDTO_Object.NonReservedName(AName: String): String;
Const
  Reserved : Array[1..64] of string = (
    'and', 'end', 'interface', 'record', 'var', 'array', 'except', 'is',
    'repeat', 'while', 'as', 'exports', 'label', 'resourcestring', 'with', 'asm',
    'file', 'library', 'set', 'xor', 'begin', 'finalization', 'mod', 'shl',
    'case', 'finally', 'nil', 'shr', 'class', 'for', 'not', 'string',
    'const', 'function', 'object', 'then', 'constructor', 'goto', 'of', 'threadvar',
    'destructor', 'if', 'or', 'to', 'dispinterface', 'implementation', 'packed', 'try',
    'div', 'in', 'procedure', 'type', 'do', 'inherited', 'program', 'unit',
    'downto', 'initialization', 'property', 'until', 'else', 'inline', 'raise', 'uses' );
var
  ReservedWord: String;
begin
  Result := AName;
  for ReservedWord in Reserved do
    if SameText( ReservedWord, AName ) then
      Exit( '&' + AName );
end;

procedure TDTO_Object.WriteImplementation(Strings: TStrings);
var
  Prop: TDTO_Property;
begin
  if not FIsDuplicate then
  Begin
    //Make dependent types
    for Prop in FProperties do
      Prop.WriteImplementation( Strings );

    //Write class
    Strings.Add( '' );
    Strings.Add( '{ ' + Name + ' }' );
    Strings.Add( '' );
    Strings.Add( 'Constructor ' + Name + '.Create;' );
    Strings.Add( 'Begin' );
    for Prop in FProperties do
      Case Prop.Value.Kind of
        kString:   Strings.Add( '  F' + Prop.Name + ' := '''';' );
        kDatetime: Strings.Add( '  F' + Prop.Name + ' := 0;' );
        kInteger:  Strings.Add( '  F' + Prop.Name + ' := 0;' );
        kFloat:    Strings.Add( '  F' + Prop.Name + ' := 0;' );
        kObject:   Strings.Add( '  F' + Prop.Name + ' := ' + TDTOGenerator.TypeName(Prop.Value) + '.Create;' );
        kList:     Strings.Add( '  F' + Prop.Name + ' := ' + TDTOGenerator.TypeName(Prop.Value) + '.Create;' );
        kBoolean:  Strings.Add( '  F' + Prop.Name + ' := False;' );
      End;
    Strings.Add( 'End;' );

    if FNeedsConstructor then
    Begin
      Strings.Add( '' );
      Strings.Add( 'Destructor ' + Name + '.Destroy;' );
      Strings.Add( 'Begin' );
      for Prop in FProperties do
        Case Prop.Value.Kind of
          kObject:   Strings.Add( '  F' + Prop.Name + '.Free;' );
          kList:     Strings.Add( '  F' + Prop.Name + '.Free;' );
        End;
      Strings.Add( '  Inherited;' );
      Strings.Add( 'End;' );
    End;
  End;
end;

procedure TDTO_Object.WriteInterface(Strings: TStrings; PreviousList: TObjectList<TDTO_Object>);
var
  Prop: TDTO_Property;
  PropType, PropStr: String;
  Duplicate: TDTO_Object;
begin
  Duplicate := FindDuplicate( PreviousList );
  FIsDuplicate := Assigned( Duplicate );
  if FIsDuplicate then
  Begin
    if Duplicate.Name = Name then
    Begin
      //Strings.Add( '  //Duplicate: ' + Name + ' = class' );
      //Strings.Add( '' );
    End
    Else
    Begin
      Strings.Add( '  ' + Name + ' = ' + Duplicate.Name + ';' );
      Strings.Add( '' );
    End;
  End
  else
  Begin
    PreviousList.Add( Self );
    //Make dependent types
    for Prop in FProperties do
      Prop.WriteInterface( Strings, PreviousList );
    //Write class
    Strings.Add( '  ' + Name + ' = class' );
    Strings.Add( '  Private' );
    for Prop in FProperties do
      if Prop.Value.ValidType then
      Begin
        Strings.Add( '    F' + Prop.Name + ': ' + TDTOGenerator.TypeName(Prop.Value) + ';' );
        FNeedsConstructor := FNeedsConstructor or Prop.NeedsConstructor;
      End;
    Strings.Add( '  Public' );
    for Prop in FProperties do
      if not Prop.Value.ValidType then
        Strings.Add( '    //Property ' + Prop.Name + ' is indeterminate type' )
      else
      Begin
        PropType := TDTOGenerator.TypeName(Prop.Value);
        PropStr := '    Property ' + NonReservedName(Prop.Name) + ': ' + PropType + ' read F' + Prop.Name;
        if not (Prop.Value.Kind in [kObject, kList]) then
          PropStr := PropStr + ' write F' + Prop.Name;
        Strings.Add( PropStr + ';' )
      End;
    Strings.Add( '    Constructor Create;' );
    if FNeedsConstructor then
      Strings.Add( '    Destructor Destroy; Override;' );
    Strings.Add( '  End;' );
    Strings.Add( '' );
  End;
end;

{ TDTO_Property }

constructor TDTO_Property.Create;
begin
  FName := '';
  FValue := TDTO_Value.Create;
end;

destructor TDTO_Property.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TDTO_Property.IsSameAs(AProperty: TDTO_Property): Boolean;
begin
  Result := (Name = AProperty.Name) and
            (Value.IsSameAs( AProperty.Value ));
end;

function TDTO_Property.NeedsConstructor: Boolean;
begin
  Result := Value.Kind in [kObject, kList];
end;

procedure TDTO_Property.WriteImplementation(Strings: TStrings);
begin
  Value.WriteImplementation( Strings );
end;

procedure TDTO_Property.WriteInterface(Strings: TStrings; PreviousList: TObjectList<TDTO_Object>);
begin
  Value.WriteInterface( Strings, PreviousList );
end;

{ TDTO_Value }

constructor TDTO_Value.Create;
begin
  FName := '';
  FKind := kUnknown;
  FObj  := nil;
  FList := nil;
end;

destructor TDTO_Value.Destroy;
begin
  FObj.Free;
  FList.Free;
  inherited;
end;

function TDTO_Value.IsSameAs(AValue: TDTO_Value): Boolean;
begin
  Result := (Kind = AValue.Kind);
  if Result then
  Begin
    if Kind = kObject then
      Result := FObj.IsSameAs( AValue.Obj )
    else if Kind = kList then
      Result := FList.IsSameAs( AValue.List )
  End;
end;

procedure TDTO_Value.WriteImplementation(Strings: TStrings);
begin
  if Kind = kObject then
    Obj.WriteImplementation( Strings )
  else if Kind = kList then
    List.WriteImplementation( Strings );
end;

procedure TDTO_Value.WriteInterface(Strings: TStrings; PreviousList: TObjectList<TDTO_Object>);
begin
  if Kind = kObject then
    Obj.WriteInterface( Strings, PreviousList )
  else if Kind = kList then
    List.WriteInterface( Strings, PreviousList );
end;

procedure TDTO_Value.SetList(const Value: TDTO_List);
begin
  FList.Free;
  FList := Value;
end;

procedure TDTO_Value.SetObj(const Value: TDTO_Object);
begin
  FObj.Free;
  FObj := Value;
end;

function TDTO_Value.ValidType: Boolean;
begin
  Result := not (Kind in [kUnsupported, kUnknown]);
end;

{ TDTO_List }

constructor TDTO_List.Create;
begin
  FName := '';
  FElementType := TDTO_Value.Create;
end;

destructor TDTO_List.Destroy;
begin
  FElementType.Free;
  inherited;
end;

function TDTO_List.IsSameAs(List: TDTO_List): Boolean;
begin
  Result := ElementType.IsSameAs( List.ElementType );
end;

function TDTO_List.TypeName: String;
begin
  case ElementType.Kind of
    kObject,
    kList:   Result := 'TObjectList<' + TDTOGenerator.TypeName(ElementType) + '>';
    kString: Result := 'TStringList';
    kDatetime,
    kInteger,
    kFloat,
    kBoolean: Result := 'TList<' + TDTOGenerator.TypeName(ElementType) + '>';
  end;
end;

procedure TDTO_List.WriteImplementation(Strings: TStrings);
begin
  if ElementType.Kind = kObject then
    ElementType.Obj.WriteImplementation( Strings )
  else if ElementType.Kind = kList then
    ElementType.List.WriteImplementation( Strings );
end;

procedure TDTO_List.WriteInterface(Strings: TStrings; PreviousList: TObjectList<TDTO_Object>);
begin
  if ElementType.Kind = kObject then
    ElementType.Obj.WriteInterface( Strings, PreviousList )
  else if ElementType.Kind = kList then
    ElementType.List.WriteInterface( Strings, PreviousList );
end;

end.
