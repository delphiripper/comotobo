unit MT.Classes;
{$IfDef FPC}
 {$mode delphi}
{$EndIf}
{$IFNDEF UNIX}
  {$DEFINE WINDOWS}
{$ENDIF}
interface

uses
  {$IfDef FPC}
    SysUtils, Classes, Generics.Collections, fpjson,
  {$Else}
    System.SysUtils, System.Classes, System.Generics.Collections, MT.RTTI,
  {$EndIf}
  TypInfo,
  MT.Serializer,
  MT.Interfaces;

type
  EMTError = class(Exception);

  TMTObject = class(TInterfacedObject, IMTInterface)
  private
  {$IfNDef FPC}
    function GetClassInformation: TMTClassInformation;
  {$EndIf}
  protected
    function GetInstance: TObject;
  end;

  TMTBody = class abstract(TMTObject, IMTBody)
  protected
    function GetMessageType: String; {$IfDef FPC} virtual; {$EndIf}
  end;

  TMTBodyClass = class of TMTBody;

  IMTErrorMessage = interface(IMTBody)
    ['{FAEB9FA8-FC66-4EB1-A004-BA31DF965F5E}']
    function GetMessage: String;
    procedure SetMessage(const Value: String);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);

    property message: String read GetMessage write SetMessage;
    property date: TDateTime read GetDate write SetDate;
  end;

  {$IfNDef FPC}
  [MTMessageType('urn:emptyMessage')]
  [MTAssemblyName('MTD')]
  {$EndIf}
  TMTEmptyMessage = class(TMTBody)

  end;

  {$IfNDef FPC}
  [MTMessageType('urn:errorMessage')]
  [MTAssemblyName('MTD')]
  {$EndIf}
  TMTErrorMessage = class(TMTBody, IMTErrorMessage)
  private
    FMessage: String;
    FDate: TDateTime;
    function GetMessage: String;
    procedure SetMessage(const Value: String);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    property message: String read GetMessage write SetMessage;
    property date: TDateTime read GetDate write SetDate;
  end;

  { THeader }

  TMTHeader = class(TMTObject, IMTHeader)
  private
    FKey: String;
    FValue: Variant;
    function GetKey: String;
    function GetValue: Variant;
    procedure SetKey(AValue: String);
    procedure SetValue(AValue: Variant);
  protected
  public
    procedure Assign(AObject: IMTHeader);
  published
    property Key: String read GetKey write SetKey;
    property Value: Variant read GetValue write SetValue;
  end;

  { THeaders }

  TMTHeaders = class(TMTObject, IMTHeaders, IMTEnumerator<IMTHeader>)
  private
    FHeaderList: TList<IMTHeader>;
    function GetItems(Index: Integer): IMTHeader;
    function GetValue(AName: String): Variant;
    procedure SetValue(AName: String; AValue: Variant);

  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(AKey: String): Integer;
    function Add(AName: String; AValue: Variant): IMTHeader;
    function GetEnumerator: TEnumerator<IMTHeader>;
    property Value[AName: String]: Variant read GetValue write SetValue;
    property Items[Index: Integer]: IMTHeader read GetItems;
  end;


  TMTMessage<T1: IMTBody> = class(TMTObject, IMTMessage<T1>)
  private type
    { THost }
    THost = class(TMTObject, IMTHost)
    private
      Fassembly: String;
      FassemblyVersion: String;
      FframeworkVersion: String;
      FmachineName: String;
      FmassTransitVersion: String;
      FoperationSystemVersion: String;
      FprocessId: UInt64;
      FprocessName: String;
      function Getassembly: String;
      function GetassemblyVersion: String;
      function GetframeworkVersion: String;
      function GetmachineName: String;
      function GetmassTransitVersion: String;
      function GetoperationSystemVersion: String;
      function GetprocessId: UInt64;
      function GetprocessName: String;
      procedure Setassembly(AValue: String);
      procedure SetassemblyVersion(AValue: String);
      procedure SetframeworkVersion(AValue: String);
      procedure SetmachineName(AValue: String);
      procedure SetmassTransitVersion(AValue: String);
      procedure SetoperationSystemVersion(AValue: String);
      procedure SetprocessId(AValue: UInt64);
      procedure SetprocessName(AValue: String);
    public
      constructor Create;
      procedure Clear;
    published
      property machineName: String read GetmachineName write SetmachineName;
      property processName: String read GetprocessName write SetprocessName;
      property processId: UInt64 read GetprocessId write SetprocessId;
      property assembly: String read Getassembly write Setassembly;
      property assemblyVersion: String read GetassemblyVersion
        write SetassemblyVersion;
      property frameworkVersion: String read GetframeworkVersion
        write SetframeworkVersion;
      property massTransitVersion: String read GetmassTransitVersion
        write SetmassTransitVersion;
      property operationSystemVersion: String read GetoperationSystemVersion
        write SetoperationSystemVersion;
    end;


    { TMessageType }

    TMessageType = class(TMTObject, IMTMessageType)
    private
      FList: TStringList;
      function GetMessageType(Index: Integer): String;
      procedure SetmessageType(Index: Integer; AValue: String);
    protected
    public
      procedure Assign(AObject: IMTMessageType);
      constructor Create;
      destructor Destroy; override;
      property messageType[index: Integer]: String read GetMessageType
        write SetmessageType;
      procedure Add(AMessageTypeStr: String);
      function GetEnumerator: TStringsEnumerator;
      procedure Clear;
    end;

  private
    FconversationId: String;
    FdestinationAddress: String;
    Fmessage: T1;
    FMessageId: String;
    FmessageType: IMTMessageType;
    FsourceAddress: String;
    FHeaders: IMTHeaders;
    FHost: IMTHost;
    function GetconversationId: String;
    function GetdestinationAddress: String;
    function Getheaders: IMTHeaders;
    function GetHost: IMTHost;
    function Getmessage: T1;
    function GetMessageId: String;
    function GetMessageType: IMTMessageType;
    function GetsourceAddress: String;
    procedure SetconversationId(AValue: String);
    procedure SetdestinationAddress(AValue: String);
    procedure Setheaders(AValue: IMTHeaders);
    procedure SetHost(AValue: IMTHost);
    procedure Setmessage(AValue: T1);
    procedure SetMessageId(AValue: String);
    procedure SetmessageType(AValue: IMTMessageType);
    procedure SetsourceAddress(AValue: String);
  protected
  public
    procedure Assign(AObject: IMTMessage<T1>);
    constructor Create(AMessage: T1);
    destructor Destroy; override;
    procedure Clear;
  published
    property messageId: String read GetMessageId Write SetMessageId;
    property conversationId: String read GetconversationId
      write SetconversationId;
    property sourceAddress: String read GetsourceAddress write SetsourceAddress;
    property destinationAddress: String read GetdestinationAddress
      write SetdestinationAddress;
    property messageType: IMTMessageType read GetMessageType
      write SetmessageType;
    property message: T1 read Getmessage write Setmessage;
    property headers: IMTHeaders read Getheaders write Setheaders;
    property host: IMTHost read GetHost write SetHost;
  end;


  { TMTHeaderSerializer }

  TMTHeaderSerializer = class(TMTCustomPropSerializer)
  public
    procedure PropertyToJsonValue(const ASerializer: TMTSerializer; const AJsonData: TJSONData;
      const AName: String; const AObject: TObject; const APropInfo: PPropInfo);
      override;
    procedure JsonValueToProperty(const ASerializer: TMTSerializer;
      const AJsonData: TJSONData; const AName: String; const AObject: TObject;
      const APropInfo: PPropInfo); override;
  end;

  { TMTMessageTypeSerializer }

  TMTMessageTypeSerializer = class(TMTCustomPropSerializer)
  public
    procedure PropertyToJsonValue(const ASerializer: TMTSerializer; const AJsonData: TJsonData;
      const AName: String; const AObject: TObject; const APropInfo: PPropInfo);
      override;
    procedure JsonValueToProperty(const ASerializer: TMTSerializer;
  const AJsonData: TJSONData; const AName: String; const AObject: TObject;
  const APropInfo: PPropInfo); override;
  end;


function _GetFileVersion(const AFileName: string; var AMajor, AMinor, ARelease, ABuild: Cardinal): boolean;


implementation

uses
  {$IfDef FPC}
     Variants
  {$IfDef Unix}
     , Unix
     , BaseUnix
     , resource
     , elfreader
     , versiontypes
     , versionresource
  {$Else}
     , Windows
  {$EndIf}
  {$Else}
      System.Variants, System.Rtti, WinApi.Windows
   {$EndIf}
     ;

{ TMTMessageTypeSerializer }

procedure TMTMessageTypeSerializer.JsonValueToProperty(
  const ASerializer: TMTSerializer; const AJsonData: TJSONData;
  const AName: String; const AObject: TObject; const APropInfo: PPropInfo);
var MessageTypes: IMTMessageType;
    Enum: {$IFDEF FPC}TJSONEnum{$ELSE}TJsonData{$EndIf};
begin
  if assigned(AJsonData) and (AJsonData is TJSONArray) then
  begin
    MessageTypes := GetInterfaceProp(AObject, APropInfo) as IMTMessageType;
    for enum in (AJsonData as TJSONArray) do
     {$IFDEF FPC}
      MessageTypes.Add(Enum.Value.AsString);
     {$ELSE}
      MessageTypes.Add(Enum.Value);
     {$ENDIF}
  end;
end;

procedure TMTMessageTypeSerializer.PropertyToJsonValue(const ASerializer: TMTSerializer;
  const AJsonData: TJsonData; const AName: String; const AObject: TObject;
  const APropInfo: PPropInfo);
var MessageTypes: IMTMessageType;
    S: String;
    Arr: TJSONArray;
begin
  MessageTypes := GetInterfaceProp(AObject, APropInfo) as IMTMessageType;
  Arr := TJSONArray.Create;
  for s In MessageTypes do
     Arr.Add(S);
  ASerializer.AddObject(AJsonData, AName, Arr);
end;

{ TMTHeaderSerializer }

procedure TMTHeaderSerializer.JsonValueToProperty(
  const ASerializer: TMTSerializer; const AJsonData: TJSONData;
  const AName: String; const AObject: TObject; const APropInfo: PPropInfo);
var Headers: IMTHeaders;
    Enum: TJSONEnum;
begin
  if assigned(AJsonData) and (AJsonData is TJSONObject) then
  begin
    Headers := GetInterfaceProp(AObject, APropInfo) as IMTHeaders;
    for enum in (AJsonData as TJSONObject) do
    {$IFDEF FPC}
      Headers.Add(Enum.Key, Enum.Value.Value);
    {$ELSE}
      Headers.Add(Enum.JsonString.AsString, Enum.JsonValue.Value);
    {$ENDIF}
  end;
end;

procedure TMTHeaderSerializer.PropertyToJsonValue(const ASerializer: TMTSerializer;
  const AJsonData: TJsonData; const AName: String; const AObject: TObject;
  const APropInfo: PPropInfo);
var Headers: IMTHeaders;
    HeadersEnum: IMTEnumerator<IMTHeader>;
    Header: IMTHeader;
    JObj: TJSONObject;
    k: Integer;
begin
  Headers := GetInterfaceProp(AObject, APropInfo) as IMTHeaders;
  JObj := TJSONObject.Create;
  HeadersEnum := Headers as IMTEnumerator<IMTHeader>;
  for Header in HeadersEnum do
    begin
      k := tvardata(Header.Value).vtype;
      case k of
       varempty: {};
       varnull: JObj.Add(Header.Key, CreateJSON);
       varint64: JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vint64));
       varinteger: JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vinteger));
       varword:JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vword));
       varbyte:JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vbyte));
       vardate: JObj.Add(Header.Key, CreateJSON(DateTimeToISOTimeStamp(tvardata(Header.Value).vdate)));
       varshortint:JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vshortint));
       varstring, varustrarg: JObj.Add(Header.Key, CreateJSON(VarToStr(Header.Value)));
       vardouble: JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vdouble));
       varboolean: JObj.Add(Header.Key, CreateJSON(tvardata(Header.Value).vboolean));
      end;
    end;
  ASerializer.AddObject(AJsonData, AName, JObj);
end;

{ TMasstransitCustomMessage }

{$IfNDef FPC}
function TMTObject.GetClassInformation: TMTClassInformation;
begin
  Result := TMTClassInformation.Create(Self);
end;
{$EndIf}


function TMTBody.GetMessageType: String;
begin
  {$IfNDef FPC}
   Result := GetClassInformation.GetMessageType;
  {$Else}
   Result := '???';
  {$EndIf}
end;


{ TMTMessage<Intf>.THeader }

procedure TMTHeader.Assign(AObject: IMTHeader);
begin
  FKey := AObject.Key;
  Value := AObject.Value;
end;

function TMTHeader.GetKey: String;
begin
  Result := FKey;
end;

function TMTHeader.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TMTHeader.SetKey(AValue: String);
begin
  FKey := AValue;
end;

procedure TMTHeader.SetValue(AValue: Variant);
begin
  FValue := AValue;
end;

{ TMTMessage<T>.THost }

procedure TMTMessage<T1>.THost.Clear;
begin

end;


function _GetFileVersion(const AFileName: string; var AMajor, AMinor, ARelease, ABuild: Cardinal): boolean;
{$IfDef Unix}
begin
  Result := False;
end;

{$Else}
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := False;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);

  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          AMajor := HiWord(FI.dwFileVersionMS);
          AMinor := LoWord(FI.dwFileVersionMS);
          ARelease := HiWord(FI.dwFileVersionLS);
          ABuild := LoWord(FI.dwFileVersionLS);
          Result := True;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$EndIf}

constructor TMTMessage<T1>.THost.Create;
{$If Defined(WINDOWS)}
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
  Mj, Mm, Rl, Bd: Cardinal;
  lOsVersionInfo: OSVERSIONINFO;
{$ElseIf Defined(Unix)}
var
   osInfo: UtsName;
   Mj, Mm, Rl, Bd: Cardinal;
{$EndIf}

begin
 FprocessId := {$IfDef UNIX}GetProcessID{$Else}GetCurrentProcessId{$EndIf};
 FprocessName := ExtractFileName(ParamStr(0));
 Fassembly := FprocessName;
 {$IfDef UNIX}
  FmachineName := GetHostName;
 {$Else}
 Size := 256;
 if GetComputerName(Buffer, Size) then
  FmachineName := Buffer
 else
  FmachineName := '';
 {$EndIf}
 FframeworkVersion := 'RTL '+{$IfNDef FPC}FloatToStr(System.RTLVersion){$Else}'FPC'+{$I %FPCVERSION%}{$EndIf};
 {$IfDef FPC}
  FframeworkVersion := FframeworkVersion + ' ' + {$I %FPCTARGETCPU}+'-'+{$I %FPCTARGETOS%};
 {$EndIf}
 FmassTransitVersion := '3.5.7.1082';
 {$IfNDef FPC}
 FoperationSystemVersion := TOSVersion.ToString;
 {$Else}
  {$IfDef UNIX}
    if FpUname(osInfo) = 0 then
     FoperationSystemVersion := osInfo.Version+'/'+osInfo.Sysname+'/'+osInfo.Release+'/'+osInfo.Machine;
  {$Else}
    FillChar(lOsVersionInfo, sizeOf(lOsVersionInfo), 0);
  lOsVersionInfo.dwOSVersionInfoSize:= sizeof(lOsVersionInfo);
    if GetVersionEx(@lOsVersionInfo) then
     FoperationSystemVersion := 'Windows ' +lOsVersionInfo.dwMajorVersion.ToString + '.'
                             + lOsVersionInfo.dwMinorVersion.ToString + '.'
                             + lOsVersionInfo.dwBuildNumber.ToString + ' '
                             + lOsVersionInfo.szCSDVersion
    else
     FoperationSystemVersion := 'Windows';
  {$EndIf}
 {$EndIf}
 if _GetFileVersion(ParamStr(0), Mj, Mm, Rl, Bd ) then
  FassemblyVersion := Format('%d.%d.%d.%d', [Mj, Mm, Rl, Bd])
 else
  FassemblyVersion := '0.0.0.0';
end;

function TMTMessage<T1>.THost.Getassembly: String;
begin
  Result := Fassembly;
end;

function TMTMessage<T1>.THost.GetassemblyVersion: String;
begin
  Result := FassemblyVersion;
end;

function TMTMessage<T1>.THost.GetframeworkVersion: String;
begin
  Result := FframeworkVersion;
end;

function TMTMessage<T1>.THost.GetmachineName: String;
begin
  Result := FmachineName;
end;

function TMTMessage<T1>.THost.GetmassTransitVersion: String;
begin
  Result := FmassTransitVersion;
end;

function TMTMessage<T1>.THost.GetoperationSystemVersion: String;
begin
  Result := FoperationSystemVersion;
end;

function TMTMessage<T1>.THost.GetprocessId: UInt64;
begin
  Result := FprocessId;
end;

function TMTMessage<T1>.THost.GetprocessName: String;
begin
  Result := FprocessName;
end;

procedure TMTMessage<T1>.THost.Setassembly(AValue: String);
begin
  Fassembly := AValue;
end;

procedure TMTMessage<T1>.THost.SetassemblyVersion(AValue: String);
begin
  FassemblyVersion := AValue;
end;

procedure TMTMessage<T1>.THost.SetframeworkVersion(AValue: String);
begin
  FframeworkVersion := AValue;
end;

procedure TMTMessage<T1>.THost.SetmachineName(AValue: String);
begin
  FmachineName := AValue;
end;

procedure TMTMessage<T1>.THost.SetmassTransitVersion(AValue: String);
begin
  FmassTransitVersion := AValue;
end;

procedure TMTMessage<T1>.THost.SetoperationSystemVersion(AValue: String);
begin
  FoperationSystemVersion := AValue;
end;

procedure TMTMessage<T1>.THost.SetprocessId(AValue: UInt64);
begin
  FprocessId := AValue;
end;

procedure TMTMessage<T1>.THost.SetprocessName(AValue: String);
begin
  FprocessName := AValue;
end;

{ TMTMessage<T>.THeaders }

function TMTHeaders.Add(AName: String; AValue: Variant): IMTHeader;
var k: Integer;
begin
 k := IndexOf(AName);
 if k = -1 then
 begin
  Result := TMTHeader.Create;
  Result.Key := AName;
  FHeaderList.Add(Result);
 end else
  Result := Items[k];
 Result.Value := AValue;
end;

constructor TMTHeaders.Create;
begin
  FHeaderList := TList<IMTHeader>.Create;
end;

destructor TMTHeaders.Destroy;
begin
  FHeaderList.Clear;
  FHeaderList.Free;
  inherited;
end;

function TMTHeaders.GetEnumerator: TEnumerator<IMTHeader>;
begin
 Result := FHeaderList.GetEnumerator;
end;


function TMTHeaders.GetItems(Index: Integer): IMTHeader;
begin
  Result := FHeaderList[Index];
end;

function TMTHeaders.GetValue(AName: String): Variant;
var
  k: Integer;
begin
  k := IndexOf(AName);
  if k > -1 then
    Result := Items[k].Value
  else
    Result := Unassigned;
end;

function TMTHeaders.IndexOf(AKey: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FHeaderList.Count - 1 do
    if Items[i].Key = AKey then
      Exit(i);
end;

procedure TMTHeaders.SetValue(AName: String; AValue: Variant);
begin
 Add(AName, AValue);
end;

{ TMTMessage<T1>.TMessageType }

procedure TMTMessage<T1>.TMessageType.Add(AMessageTypeStr: String);
begin
  if FList.IndexOf(AMessageTypeStr) = -1 then
      FList.Add(AMessageTypeStr)
end;

procedure TMTMessage<T1>.TMessageType.Assign(AObject: IMTMessageType);
var
  s: String;
begin
  if (AObject <> nil) then
  begin
    Clear;
    for s in AObject do
      FList.Add(s);
  end;
end;

procedure TMTMessage<T1>.TMessageType.Clear;
begin
  FList.Clear;
end;

constructor TMTMessage<T1>.TMessageType.Create;
begin
  FList := TStringList.Create;
end;

destructor TMTMessage<T1>.TMessageType.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMTMessage<T1>.TMessageType.GetEnumerator: TStringsEnumerator;
begin
  Result := FList.GetEnumerator;
end;

function TMTMessage<T1>.TMessageType.GetMessageType(Index: Integer): String;
begin
  Result := FList[index];
end;

procedure TMTMessage<T1>.TMessageType.SetmessageType(Index: Integer;
  AValue: String);
begin
  FList[index] := AValue;
end;

{ TMTMessage<T1> }

procedure TMTMessage<T1>.Assign(AObject: IMTMessage<T1>);
begin

end;

procedure TMTMessage<T1>.Clear;
begin

end;

constructor TMTMessage<T1>.Create(AMessage: T1);
begin
  FHeaders := TMTHeaders.Create;
  FHost := THost.Create;
  Fmessage := AMessage;
  {$IfNDef FPC}
  AMessage := nil;
  {$EndIf}
  FmessageType := TMessageType.Create;
  FmessageType.Add(FMessage.GetMessageType);
end;

destructor TMTMessage<T1>.Destroy;
begin
  FmessageType := nil;
  {$IfNDef FPC}
  Fmessage := nil;
  {$EndIf}
  inherited;
end;

function TMTMessage<T1>.GetconversationId: String;
begin
  Result := FconversationId;
end;

function TMTMessage<T1>.GetdestinationAddress: String;
begin
  Result := FdestinationAddress;
end;

function TMTMessage<T1>.Getheaders: IMTHeaders;
begin
  Result := FHeaders;
end;

function TMTMessage<T1>.GetHost: IMTHost;
begin
  Result := FHost;
end;

function TMTMessage<T1>.Getmessage: T1;
begin
  Result := Fmessage;
end;

function TMTMessage<T1>.GetMessageId: String;
begin
  Result := FMessageId;
end;

function TMTMessage<T1>.GetMessageType: IMTMessageType;
begin
  Result := FmessageType;
end;

function TMTMessage<T1>.GetsourceAddress: String;
begin
  Result := FsourceAddress;
end;

procedure TMTMessage<T1>.SetconversationId(AValue: String);
begin
  FconversationId := AValue;
end;

procedure TMTMessage<T1>.SetdestinationAddress(AValue: String);
begin
  FdestinationAddress := AValue;
end;

procedure TMTMessage<T1>.Setheaders(AValue: IMTHeaders);
begin
  FHeaders := AValue;
end;

procedure TMTMessage<T1>.SetHost(AValue: IMTHost);
begin
  FHost := AValue;
end;

procedure TMTMessage<T1>.Setmessage(AValue: T1);
begin
  Fmessage := AValue;
end;

procedure TMTMessage<T1>.SetMessageId(AValue: String);
begin
  FMessageId := AValue;
end;

procedure TMTMessage<T1>.SetmessageType(AValue: IMTMessageType);
begin
  FmessageType := AValue;
end;

procedure TMTMessage<T1>.SetsourceAddress(AValue: String);
begin
  FsourceAddress := AValue;
end;


function TMTObject.GetInstance: TObject;
begin
 Result := Self;
end;


{ TMTErrorMessage }

function TMTErrorMessage.GetDate: TDateTime;
begin
 Result := FDate;
end;

function TMTErrorMessage.GetMessage: String;
begin
 Result := FMessage;
end;

procedure TMTErrorMessage.SetDate(const Value: TDateTime);
begin
 FDate := Value;
end;

procedure TMTErrorMessage.SetMessage(const Value: String);
begin
 FMessage := Value;
end;

initialization

finalization


end.
