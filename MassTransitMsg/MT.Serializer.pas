unit MT.Serializer;
{$IfDef FPC}
  {$mode delphi}
{$EndIf}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections,
  {$IfDef FPC}
    fpjson
  {$Else}
     System.JSON
   , RTTI
  {$EndIf}
  ;

type

{$IFNDEF FPC}
  TJsonData = TJSONValue;
  TJSONStringType = string;
  TJSONEnum = TJSONPair;
  TJSONArray = System.JSON.TJSONArray;
  TJSONArrayEnum = TJSONValue;
  TJSONObject = System.JSON.TJSONObject;
{$ENDIF}

  TMTSerializer = class;

  TMTCustomPropSerializer = class
  public
    procedure PropertyToJsonValue(const ASerializer: TMTSerializer; const AJsonData: TJsonData;
      const AName: String; const AObject: TObject; const {$IfDef FPC}APropInfo: PPropInfo{$Else}AValue: TValue{$EndIf}); virtual; abstract;
    procedure JsonValueToProperty(const ASerializer: TMTSerializer; const AJsonData: TJSONData;
      const AName: String; const AObject: TObject; {$IfDef FPC}const APropInfo: PPropInfo{$Else}var AValue: TValue{$EndIf}); virtual; abstract;
  end;

  TMTCustomPropSerializerClass = class of TMTCustomPropSerializer;


  IMTSerializer = interface
  ['{3C3C64A9-28F8-4FEB-9285-D4FE513FA740}']
    function SerializeObject(const AObject: TObject; AFormat: Boolean): TJSONStringType;
    function SerializeInterface(const AInterface: IUnknown; AFormat: Boolean): TJSONStringType;
    procedure DeserializeObject(const AJSON: TJSONStringType; AObject: TObject);
    procedure DeserializeJsonObject(const AJsonObject: TJSONObject; AObject: TObject);
    procedure DeserializeInterface(const AJSON: TJSONStringType; AInterface: IUnknown);
    procedure RegisterCustomSerializer(ATypeInfo: PTypeInfo; AClass: TMTCustomPropSerializerClass);
  end;

  { TMTSerializer }
  TMTSerializerEmptyOption = (mteoNone, mteoNull, mteoNotAdd);

  TMTSerializer = class(TInterfacedObject, IMTSerializer)
  private
    FEmptyOption: TMTSerializerEmptyOption;
  strict private
    type TMTCustomSerializers = TObjectDictionary<PTypeInfo, TMTCustomPropSerializer>;
  private
    FCustomSerializers: TMTCustomSerializers;
    function GetSerializer(ATypeInfo: PTypeInfo): TMTCustomPropSerializer;
  public
    constructor Create(AEmptyOption: TMTSerializerEmptyOption = mteoNotAdd);
    destructor Destroy; override;
    procedure RegisterCustomSerializer(ATypeInfo: PTypeInfo; AClass: TMTCustomPropSerializerClass);
    procedure ObjectToJSONObject(const AObject: TObject; const AJSonObject: TJsonObject);
    procedure PropertyToJsonValue(const AJsonData: TJsonData; const AName: String;
      const AObject: TObject; {$IfDef FPC}const APropInfo: PPropInfo{$Else}const AValue: TValue{$EndIf});
    procedure ArrayToJsonValue(const AJsonData: TJSONData; const AName: String; const AObject: TObject; const APropInfo: PPropInfo);

    procedure JsonObjectToObject(const AJsonObject: TJSONObject; const AObject: TObject);
    procedure JsonArrayToObject(const AJsonArray: TJSONArray; const AObject: TObject; const APropInfo: {$IfDef FPC}PPropInfo{$Else}TRttiProperty{$EndIf});
    procedure JsonValueToProperty(const AJsonData: TJSONData; const AName: String;
      const AObject: TObject; {$IfDef FPC}const APropInfo: PPropInfo{$Else}var AValue: TValue{$EndIf});

    function SerializeObject(const AObject: TObject; AFormat: Boolean): TJSONStringType;
    function SerializeInterface(const AInterface: IUnknown; AFormat: Boolean): TJSONStringType;
    procedure DeserializeObject(const AJSON: TJSONStringType; AObject: TObject);
    procedure DeserializeJsonObject(const AJsonObject: TJSONObject; AObject: TObject);
    procedure DeserializeInterface(const AJSON: TJSONStringType; AInterface: IUnknown);

    class procedure AddObject(AJsonData: TJSONData; AName: String; AJsonObject: TJSONData);
  end;

{$IFNDEF FPC}
type
  TJSONValueHelper = class helper for TJSONValue
  private
    function GetAsString: String;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsFloat: Double;
  public
    function Add(APair: TJSONPair): TJSONObject; overload;
    function Add(AKey: string; AJsonValue: TJSONValue): TJSONObject; overload;
    property AsString: String read GetAsString;
    property AsInt64: Int64 read GetAsInt64;
    property AsInteger: Integer read GetAsInteger;
    property AsFloat: Double read GetAsFloat;
  end;

function CreateJSON: TJsonData; overload;
function CreateJSON(AValue: Integer): TJsonData; overload;
function CreateJSON(AValue: Int64): TJsonData; overload;
function CreateJSON(AValue: Double): TJsonData; overload;
function CreateJSON(AValue: Boolean): TJsonData; overload;
function CreateJSON(AValue: AnsiString): TJsonData; overload;
{$ENDIF}

  function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;

implementation

uses DateUtils,
  {$IfDef UNIX}
    BaseUnix, Unix
  {$Else}
    Windows
  {$EndIf}
  {$IfDef FPC}
   , jsonparser
  {$Else}
   , REST.Json
  {$EndIf}
   , Variants
  ;



function DateToISODate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', ADate);
end;

function ISODateToDate(const ADate: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(ADate, 1, 4)), StrToInt(Copy(ADate, 6, 2)), StrToInt(Copy(ADate, 9, 2)));
end;


function DateToISO8601(DateTime: TDateTime): UTF8String;
begin
  Result := FormatDateTime('yyyy-mm-dd', DateTime) + 'T' +
            FormatDateTime('hh:mm:ss', DateTime)
end;

function ISO8601ToDate(DateTime: UTF8String): TDateTime;
begin
  Result := EncodeDate(StrToInt(copy(DateTime, 1, 4)),
                       StrToInt(copy(DateTime, 6, 2)),
                       StrToInt(copy(DateTime, 9, 2))) +
            EncodeTime(StrToInt(copy(DateTime, 12, 2)),
                       StrToInt(copy(DateTime, 15, 2)),
                       StrToInt(copy(DateTime, 18, 2)),
                       0);
end;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
begin
  // fs.TimeSeparator := ':';
  Result := DateToISO8601(ADateTime)
  // Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOTimeStampToDateTime(const ADateTime: string): TDateTime;
var
  lDateTime: string;
begin
  lDateTime := ADateTime;
  if lDateTime.Length < 19 then
    raise Exception.CreateFmt('Invalid parameter "%s". Hint: DateTime parameters must be formatted in ISO8601 (e.g. 2010-10-12T10:12:23)', [ADateTime]);

  if lDateTime.Chars[10] = ' ' then
  begin
    lDateTime := lDateTime.Substring(0, 10) + 'T' + lDateTime.Substring(11);
  end;
  Result := ISO8601ToDate(lDateTime);
end;



{ TMTSerializer }

constructor TMTSerializer.Create(AEmptyOption: TMTSerializerEmptyOption = mteoNotAdd);
begin
 FEmptyOption:= AEmptyOption;
 FCustomSerializers := TMTCustomSerializers.Create([doOwnsValues]);
end;

procedure TMTSerializer.ArrayToJsonValue(const AJsonData: TJSONData;
  const AName: String; const AObject: TObject; const APropInfo: PPropInfo);
begin

end;

procedure TMTSerializer.DeserializeInterface(const AJSON: TJSONStringType;
  AInterface: IUnknown);
var AObj: TObject;
begin
  AObj := AInterface as TObject;
  DeserializeObject(AJSON, AObj);
end;

procedure TMTSerializer.DeserializeJsonObject(const AJsonObject: TJSONObject;
  AObject: TObject);
begin
  JsonObjectToObject(AJsonObject, AObject);
end;

procedure TMTSerializer.DeserializeObject(const AJSON: TJSONStringType;
  AObject: TObject);
var AJSonObject: TJSONObject;
begin
  {$IFDEF FPC}
  AJSonObject := GetJSON(AJSON) as TJSONObject;
  {$Else}
   AJSonObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  {$EndIf}
  try
    DeserializeJsonObject(AJSonObject, AObject);
  finally
    FreeAndNil(AJSonObject);
  end;
end;

destructor TMTSerializer.Destroy;
begin
  FCustomSerializers.Clear;
  FreeAndNil(FCustomSerializers);
  inherited Destroy;
end;

function TMTSerializer.GetSerializer(ATypeInfo: PTypeInfo): TMTCustomPropSerializer;
begin
  if FCustomSerializers.ContainsKey(ATypeInfo) then
   Result := FCustomSerializers[ATypeInfo]
  else
   Result := nil;
end;

procedure TMTSerializer.JsonArrayToObject(const AJsonArray: TJSONArray;
  const AObject: TObject; const APropInfo: {$IfDef FPC}PPropInfo{$Else}TRttiProperty{$EndIf});
var Enum: {$IFDEF FPC}TJSONEnum{$Else}TJSONValue{$EndIf};
    {$IfNDef FPC}
     Value: TValue;
    {$EndIf}
begin
  for Enum in AJsonArray do
  {$IFDEF FPC}
   JsonValueToProperty(Enum.Value, Enum.Key, AObject, APropInfo);
  {$Else}
   begin
     Value := APropInfo.GetValue(AObject);
     JsonValueToProperty(Enum, APropInfo.Name, AObject, Value);
     if not Value.IsEmpty then
      APropInfo.SetValue(AObject, Value);
   end;
  {$EndIf}
end;

procedure TMTSerializer.JsonObjectToObject(const AJsonObject: TJSONObject;
  const AObject: TObject);
{$IfDef FPC}
var ObjType: PTypeInfo;
    Prop: PPropInfo;
    PropList: PPropList;
    I, C: Integer;
begin
  ObjType := PTypeInfo(AObject.ClassInfo);
  PropList:= nil;
  C := GetPropList(ObjType, PropList);
  try
    for i := 0 to Pred(C) do
     begin
       Prop:=PropList^[I];
       JsonValueToProperty(AJSonObject, Prop^.Name, AObject, Prop);
     end;
  finally
    Freemem(PropList);
  end;
end;
{$Else}
var
  LCtx: TRttiContext;
  ObjType: TRttiType;
  Prop: TRttiProperty;
  PropValue: TValue;
begin
  LCtx := TRttiContext.Create;
  try
    ObjType := LCtx.GetType(AObject.ClassType);
    for prop in ObjType.GetProperties do
      if Prop.Visibility = mvPublished then
        begin
          PropValue := Prop.GetValue(AObject);
          JsonValueToProperty(AJsonObject, Prop.Name, AObject, PropValue);
          if not PropValue.IsEmpty  and Prop.IsWritable then
            Prop.SetValue(AObject, PropValue);
        end;
  finally
    LCtx.Free;
  end;
end;
{$EndIf}

procedure TMTSerializer.JsonValueToProperty(const AJsonData: TJSONData; const AName: String;
  const AObject: TObject;  {$IfDef FPC}const APropInfo: PPropInfo{$Else}var AValue: TValue{$EndIf});
var ChildObject: TObject;
    ChildJsonObject: TJSONObject;
    ChildJsonArray: TJSONArray;
    strValue: String;
    jsonData: TJSONData;
    CustomSerializer: TMTCustomPropSerializer;
begin
 if AJsonData is TJSONObject then
 {$IFDEF FPC}
  jsonData := TJSONObject(AJsonData).Find(AName)
 {$ELSE}
  jsonData := TJSONObject(AJsonData).GetValue(AName)
 {$ENDIF}
 else
  jsonData := AJsonData;

{$IfDef FPC}
 if Assigned(AJsonData) and Assigned(APropInfo) then
 begin
   CustomSerializer := GetSerializer(APropInfo^.PropType);
   if Assigned(CustomSerializer) then
     CustomSerializer.JsonValueToProperty(Self, jsonData, AName, AObject, APropInfo)
   else
   if assigned(jsonData) then
   begin
     case APropInfo^.PropType^.Kind of
     {$ifdef fpc}tkAString{$else}tkString, tkLString{$endif}:
               {$ifdef fpc}
                  SetStrProp(AObject, APropInfo, jsonData.AsString);
               {$else}
                  SetAnsiStrProp(AObject, APropInfo, jsonData.{$IfDef FPC}AsString{$Else}Value{$EndIf});
               {$endif}
     {$IfNDef fpc}
     tkWString, tkUString: SetUnicodeStrProp(AObject, APropInfo, jsonData.{$IfDef FPC}AsString{$Else}Value{$EndIf});
     {$Else}
     tkWideString: SetWideStrProp(AObject, APropInfo, jsonData.AsString);
     {$EndIf}
     tkInteger: SetOrdProp(AObject, APropInfo, jsonData.AsInteger);
     tkInt64: SetInt64Prop(AObject, APropInfo, jsonData.AsInt64);
     {$ifdef fpc}
     tkBool: SetOrdProp(AObject, APropInfo, Ord(jsonData.AsBoolean));
     {$endif}
     tkSet: SetOrdProp(AObject, APropInfo, jsonData.AsInteger);

     tkVariant: begin
                  case jsonData.JSONType of
                   jtUnknown: SetVariantProp(AObject, APropInfo, Null);
                   jtNumber: SetVariantProp(AObject, APropInfo, jsonData.AsFloat);
                   jtString: SetVariantProp(AObject, APropInfo, jsonData.AsString);
                   jtBoolean: SetVariantProp(AObject, APropInfo, jsonData.AsBoolean);
                   jtNull: SetVariantProp(AObject, APropInfo, Null);
                   jtArray: {};
                   jtObject: {};
                  end;
                end;
     tkFloat: begin
                if (APropInfo^.PropType = TypeInfo(TDate)) then
                begin
                   strValue:= jsonData.AsString;
                   if strValue <> '' then
                    SetFloatProp(AObject, APropInfo, ISODateToDate(strValue));
                end
                else if (APropInfo^.PropType = TypeInfo(TDateTime)) then
                begin

                 strValue:= jsonData.AsString;
                 if strValue <> '' then
                  SetFloatProp(AObject, APropInfo, ISOTimeStampToDateTime(strValue));
                end else
                  SetFloatProp(AObject, APropInfo, jsonData.AsFloat);
              end;

     tkClass, tkInterface: begin
                              if APropInfo^.PropType^.Kind = tkInterface then
                               ChildObject := GetInterfaceProp(AObject, APropInfo) as TObject
                              else
                               ChildObject := GetObjectProp(AObject, APropInfo);
                              {$IFDEF FPC}
                              if jsonData.JSONType = jtArray then
                              {$Else}
                              if jsonData is TJSONArray then
                              {$ENDIF}
                              begin
                               ChildJsonArray := jsonData as TJSONArray;
                               if assigned(ChildJsonArray) and Assigned(ChildObject) then
                                JsonArrayToObject(ChildJsonArray, ChildObject, APropInfo);
                              end
                              else
                              begin
                               ChildJsonObject := jsonData as TJSONObject;
                               if assigned(ChildJsonObject) and Assigned(ChildObject) then
                                 JsonObjectToObject(ChildJsonObject, ChildObject);
                              end;
                           end;
     end;

   end;
 end;
{$Else}
 if assigned(jsonData) then
  begin
   CustomSerializer := GetSerializer(AValue.TypeInfo);
   if Assigned(CustomSerializer) then
     CustomSerializer.JsonValueToProperty(Self, jsonData, AName, AObject, AValue)
   else
     case AValue.Kind of
       tkUnknown: ;
       tkInteger: AValue := TValue.From<Integer>(jsonData.AsInteger);
       tkInt64: AValue := TValue.From<Integer>(jsonData.AsInt64);
       tkSet: TValue.Make(jsonData.AsInt64, AValue.TypeInfo, AValue);
       tkEnumeration: TValue.Make(jsonData.AsInteger, AValue.TypeInfo, AValue);
       tkFloat: begin
                  if jsonData is TJSONNull then
                   AValue := 0
                  else
                  if AValue.TypeInfo = TypeInfo(TDate) then
                   AValue := TValue.From<TDate>(ISODateToDate(jsonData.AsString))
                  else
                  if AValue.TypeInfo = TypeInfo(TDateTime) then
                   AValue := TValue.From<TDateTime>(ISOTimeStampToDateTime(jsonData.AsString))
                  else
                   AValue := TValue.From<Double>(jsonData.AsFloat);
                end;
       tkChar,
       tkLString,
       tkWString,
       tkWChar,
       tkUString,
       tkString: AValue := TValue.From<String>(jsonData.AsString);
       tkClass,
       tkInterface: begin
                      if AValue.Kind = tkInterface then
                       ChildObject := AValue.AsInterface as TObject
                      else
                       ChildObject := AValue.AsObject;

                       ChildJsonObject := jsonData as TJSONObject;
                       if assigned(ChildJsonObject) and Assigned(ChildObject) then
                       JsonObjectToObject(ChildJsonObject, ChildObject);
                    end;

       tkVariant: ;
       tkArray: ;
       tkDynArray: ;
     end;
   end;
{$EndIf}
end;

procedure TMTSerializer.ObjectToJSONObject(const AObject: TObject; const AJSonObject: TJsonObject);
{$IFDEF FPC}
var ObjType: PTypeInfo;
    Prop: PPropInfo;
    PropList: PPropList;
    I, C: Integer;
begin
  ObjType := PTypeInfo(AObject.ClassInfo);
  PropList:= nil;
  C := GetPropList(ObjType, PropList);
  try
    for i := 0 to Pred(C) do
     begin
       Prop:=PropList^[I];
       PropertyToJsonValue(AJSonObject, Prop^.Name, AObject, Prop);
     end;
  finally
    Freemem(PropList);
  end;
end;
{$Else}
var LCtx: TRttiContext;
    ObjType: TRttiType;
    Prop: TRttiProperty;
begin
  LCtx := TRttiContext.Create;
  try
   ObjType := LCtx.GetType(AObject.ClassType);
   for Prop in ObjType.GetProperties do
     begin
       if Prop.Visibility = mvPublished then
         PropertyToJsonValue(AJSonObject, Prop.Name, AObject, Prop.GetValue(AObject));
     end;
  finally
    LCtx.Free;
  end;
end;
{$EndIf}


class procedure TMTSerializer.AddObject(AJsonData: TJSONData; AName: String; AJsonObject: TJSONData);
begin
 if Assigned(AJsonData) then
  if AJsonData is TJSONObject then
  {$IfDef FPC}
   (AJsonData as TJSONObject).Add(AName, AJsonObject)
  {$Else}
    (AJsonData as TJSONObject).AddPair(TJSONPair.Create(AName, AJsonObject))
  {$EndIf}
  else if (AJsonData is TJSONArray) then
  {$IFDEF FPC}
   (AJsonData as TJSONArray).add(AJsonObject);
  {$ELSE}
    (AJsonData as TJSONArray).AddElement(AJsonObject);
  {$ENDIF}
end;


procedure TMTSerializer.PropertyToJsonValue(const AJsonData: TJsonData;
  const AName: String; const AObject: TObject;{$IFDEF FPC}const APropInfo: PPropInfo{$Else}const AValue: TValue{$EndIf});
var ChildObject: TObject;
    ChildJsonObject: TJsonObject;
    CustomSerializer: TMTCustomPropSerializer;
  {$ifDef FPC}
    AString: String;
    WString: WideString;
    VType: Integer;
    VValue: Variant;
    extValue: Extended;
  {$Else}
    AString: AnsiString;
    UString: UnicodeString;
  {$EndIf}
begin
{$IfDef FPC}
  if Assigned(APropInfo) then
   begin
    CustomSerializer := GetSerializer(APropInfo^.PropType);
    if Assigned(CustomSerializer) then
      CustomSerializer.PropertyToJsonValue(Self, AJsonData, AName, AObject, APropInfo)
    else
    case APropInfo^.PropType^.Kind of
    tkClass, tkInterface: begin
               if APropInfo^.PropType^.Kind = tkInterface then
                ChildObject := GetInterfaceProp(AObject, APropInfo) as TObject
               else
                ChildObject := GetObjectProp(AObject, APropInfo);
               if Assigned(ChildObject) then
                begin
                  ChildJsonObject := TJsonObject.Create;
                  ObjectToJsonObject(ChildObject, ChildJsonObject);
                  AddObject(AJsonData, AName, ChildJsonObject);
                end else
                 if FEmptyOption = mteoNull then
                   AddObject(AJsonData, AName, CreateJson);
             end;
    tkInteger: AddObject(AJsonData, AName, CreateJSON(GetOrdProp(AObject, APropInfo)));
    tkInt64: AddObject(AJsonData, AName, CreateJSON(GetInt64Prop(AObject, APropInfo)));
    {$ifdef fpc}tkAString{$else}tkString, tkLString{$endif}: begin
               {$ifdef fpc}
                  AString:= GetStrProp(AObject, APropInfo);
               {$else}
                  AString := GetAnsiStrProp(AObject, APropInfo);
               {$endif}
               if (AString <> '') then
                 AddObject(AJsonData, AName, CreateJSON(AString))
               else
               if FEmptyOption = mteoNull then
                 AddObject(AJsonData, AName, CreateJSON);
               end;
    {$IfNDef FPC}
    tkWString, tkUString: begin
                               AddObject(AJsonData, AName, CreateJSON(GetUnicodeStrProp(AObject, APropInfo)));
                           end;
    {$else}
    tkWideString: begin
                       WString:= GetWideStrProp(AObject, APropInfo);
                       if WString <> '' then
                        AddObject(AJsonData, AName, CreateJSON(WString))
                       else
                       if FEmptyOption = mteoNull then
                        AddObject(AJsonData, AName, CreateJSON);
                  end;
    {$EndIf}
    tkChar: AddObject(AJsonData, AName, CreateJSON(Char(GetOrdProp(AObject, APropInfo))));

    tkFloat: begin
               extValue:= GetFloatProp(AObject, APropInfo);
               if (APropInfo^.PropType = TypeInfo(TDate)) then
               begin
                 if extValue = 0 then
                  begin
                   if FEmptyOption = mteoNull then
                    AddObject(AJsonData, AName, TJsonNull.Create)
                  end
                 else
                   AddObject(AJsonData, AName, CreateJSON(DateToIsoDate(extValue)));
               end
               else if (APropInfo^.PropType = TypeInfo(TDateTime)) then
               begin
                 if extValue = 0 then
                 begin
                  if FEmptyOption = mteoNull then
                   AddObject(AJsonData, AName, TJsonNull.Create)
                 end
                 else
                   AddObject(AJsonData, AName, CreateJSON(DateTimeToIsoTimeStamp(extValue)));
               end else
                AddObject(AJsonData, AName, CreateJSON(extValue));
             end;
    tkVariant: begin
                 VValue := GetVariantProp(AObject, APropInfo);
                 VType:= tvardata(VValue).vtype;
                 case VType of
                   varempty: {};
                   varnull: if FEmptyOption = mteoNull then  AddObject(AJsonData, AName, CreateJSON);
                   varint64:  AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vint64));
                   varinteger:  AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vinteger));
                   varword: AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vword));
                   varbyte: AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vbyte));
                   vardate:  AddObject(AJsonData, AName, CreateJSON(DateTimeToISOTimeStamp(tvardata(VValue).vdate)));
                   varshortint: AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vshortint));
                   varUString, varstring, varustrarg:  AddObject(AJsonData, AName, CreateJSON(VarToStr(VValue)));
                   vardouble:  AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vdouble));
                   varboolean:  AddObject(AJsonData, AName, CreateJSON(tvardata(VValue).vboolean));

                 end;
               end;
    tkEnumeration: AddObject(AJsonData, AName, CreateJSON(GetOrdProp(AObject, APropInfo)));
    {$IfDef FPC}
    tkBool: AddObject(AJsonData, AName, CreateJSON(GetOrdProp(AObject, APropInfo) <> 0));
    {$EndIf}
    tkRecord: {};
    tkSet: AddObject(AJsonData, AName, CreateJSON(GetOrdProp(AObject, APropInfo)));
    tkDynArray, tkArray: ArrayToJsonValue(AJsonData, AName, AObject, APropInfo);
    end;
  end;
{$Else}
   CustomSerializer := GetSerializer(AValue.TypeInfo);
    if Assigned(CustomSerializer) then
     CustomSerializer.PropertyToJsonValue(Self, AJsonData, AName, AObject, AValue)
    else
    case AValue.Kind of
      tkInteger: AddObject(AJsonData, AName, CreateJSON(AValue.AsInteger));
      tkInt64: AddObject(AJsonData, AName, CreateJSON(AValue.AsInt64));
      tkChar, tkString,
      tkWChar, tkLString,
      tkWString, tkUString:
         begin
          AString := AValue.AsString;
          if AString <> '' then
            AddObject(AJsonData, AName, CreateJSON(AString))
          else
          if FEmptyOption = mteoNull then
            AddObject(AJsonData, AName, CreateJson);
         end;
      tkEnumeration: if AValue.TypeInfo = TypeInfo(boolean) then
                      AddObject(AJsonData, AName, CreateJSon(AValue.AsBoolean))
                     else
                      AddObject(AJsonData, AName, CreateJson(AValue.AsOrdinal));
      tkFloat: begin
                if (AValue.TypeInfo = TypeInfo(TDate)) then
                begin
                  if AValue.AsExtended = 0 then
                  begin
                   if FEmptyOption = mteoNull then
                     AddObject(AJsonData, AName, CreateJson);
                  end
                  else
                    AddObject(AJsonData, AName, CreateJSON(DateToIsoDate(AValue.AsExtended)));
                end
                else if (AValue.TypeInfo = TypeInfo(TDateTime)) then
                begin
                  if AValue.AsExtended = 0 then
                  begin
                   if FEmptyOption = mteoNull then
                     AddObject(AJsonData, AName, CreateJson);
                  end
                  else
                    AddObject(AJsonData, AName, CreateJSON(DateTimeToIsoTimeStamp(AValue.AsExtended)));
                end
                else
                  AddObject(AJsonData, AName, CreateJSON(AValue.AsExtended));
               end;
      tkSet: AddObject(AJsonData, AName, CreateJson(AValue.AsOrdinal));
      tkClass, tkInterface:
               begin
                if AValue.Kind = tkInterface then
                 ChildObject := AValue.AsInterface as TObject
                else
                 ChildObject := AValue.AsObject;
               if Assigned(ChildObject) then
                begin
                  ChildJsonObject := TJsonObject.Create;
                  ObjectToJsonObject(ChildObject, ChildJsonObject);
                  AddObject(AJsonData, AName, ChildJsonObject);
                end else
                 if FEmptyOption = mteoNull then
                   AddObject(AJsonData, AName, CreateJson);
               end;
      tkVariant: ;
      tkRecord: ;
      tkArray: ;
      tkDynArray: ;
    end;
{$EndIf}
end;

procedure TMTSerializer.RegisterCustomSerializer(ATypeInfo: PTypeInfo;
  AClass: TMTCustomPropSerializerClass);
begin
 if not FCustomSerializers.ContainsKey(ATypeInfo) then
   FCustomSerializers.Add(ATypeInfo, AClass.Create);
end;

function TMTSerializer.SerializeInterface(const AInterface: IUnknown; AFormat: Boolean): TJSONStringType;
begin
  Result := SerializeObject(AInterface as TObject, AFormat);
end;

function TMTSerializer.SerializeObject(const AObject: TObject; AFormat: Boolean): TJSONStringType;
var JSONObject: TJSONObject;
begin
  JsonObject := TJsonObject.Create;
  try
    ObjectToJsonObject(AObject, JsonObject);
    if AFormat then
    {$IfDef FPC}
      Result := JSONObject.FormatJSON()
    {$Else}
      Result := TJson.Format(JSONObject)
    {$EndIf}
    else
     Result := JsonObject.{$IfDef FPC}AsJson{$Else}ToJSON{$EndIf};
  finally
    JsonObject.Free;
  end;
end;


{$IFNDEF FPC}
{ TJSONValueHelper }

function TJSONValueHelper.Add(APair: TJSONPair): TJSONObject;
begin
 if Self is TJSONObject then
   TJSONObject(Self).AddPair(APair);
end;

function TJSONValueHelper.Add(AKey: string; AJsonValue: TJSONValue): TJSONObject;
var Pair: TJSONPair;
begin
  Pair := TJSONPair.Create(AKey, AJsonValue);
  Result := Self.Add(Pair);
end;

function TJSONValueHelper.GetAsFloat: Double;
begin
 if Self is TJSONNumber then
   Result := TJSONNumber(Self).AsDouble
 else
   Result := 0;
end;

function TJSONValueHelper.GetAsInt64: Int64;
begin
 if Self is TJSONNumber then
   Result := TJSONNumber(Self).AsInt64
 else
   Result := 0;
end;

function TJSONValueHelper.GetAsInteger: Integer;
begin
 if Self is TJSONNumber then
   Result := TJSONNumber(Self).AsInt
 else
   Result := 0;
end;

function TJSONValueHelper.GetAsString: String;
begin
 Result := Value;
end;

function CreateJSON: TJsonData; overload;
begin
  result := TJSONNull.Create;
end;

function CreateJSON(AValue: Integer): TJsonData; overload;
begin
  Result := TJSONNumber.Create(AValue);
end;

function CreateJSON(AValue: Int64): TJsonData; overload;
begin
  Result := TJSONNumber.Create(AValue);
end;

function CreateJSON(AValue: Double): TJsonData; overload;
begin
  Result := TJSONNumber.Create(AValue);
end;

function CreateJSON(AValue: Boolean): TJsonData; overload;
begin
 if AValue then
  Result := TJSONTrue.Create
 else
  Result := TJSONFalse.Create;
end;


function CreateJSON(AValue: AnsiString): TJsonData; overload;
begin
  Result := TJSONString.Create(AValue);
end;

{$ENDIF}

end.

