unit MT.Interfaces;
{$IfDef FPC}
  {$Mode delphi}
{$EndIf}

interface
uses
  {$IfDef FPC}
    Classes, Generics.Collections
  {$Else}
    System.Classes, System.Generics.Collections, MT.RTTI
  {$EndIf}
  ;

type
  IMTInterface = interface(IInvokable)
    ['{192B52E3-2CE4-4634-B4FE-158C34C7E398}']
   {$IfDef FPC}
    function GetInstance: TObject;
   {$Else}
    function GetClassInformation: TMTClassInformation;
   {$EndIf}
  end;

  IMTBody = interface(IMTInterface)
  ['{EF8ED02E-FC5C-4C67-A15E-87D57ECA4D6C}']
    function GetMessageType: String;
  end;

  { IMSHost }

  IMTHost = interface(IMTInterface)
  ['{76E0465E-096E-4317-A9A3-82113B63C823}']
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
    property machineName: String read GetmachineName write SetmachineName;
    property processName: String read GetprocessName write SetprocessName;
    property processId: UInt64 read GetprocessId write SetprocessId;
    property assembly: String read Getassembly write Setassembly;
    property assemblyVersion: String read GetassemblyVersion write SetassemblyVersion;
    property frameworkVersion: String read GetframeworkVersion write SetframeworkVersion;
    property massTransitVersion: String read GetmassTransitVersion write SetmassTransitVersion;
    property operationSystemVersion: String read GetoperationSystemVersion write SetoperationSystemVersion;
  end;

  IMTEnumerator<T> = interface(IMTInterface)
  ['{8CE2F40D-8B32-472D-8974-DB77BCD17ACF}']
    function GetEnumerator: TEnumerator<T>;
  end;

  { IMSHeader }

  IMTHeader = interface(IMTInterface)
  ['{D1CB131D-AF12-4355-8FFC-A3998FB4FB90}']
    function GetKey: String;
    function GetValue: Variant;
    procedure SetKey(AValue: String);
    procedure SetValue(AValue: Variant);
    property Key: String read GetKey write SetKey;
    property Value: Variant read GetValue write SetValue;
  end;

  { IMSHeaders }

  IMTHeaders = interface(IMTInterface)
  ['{7168EF1D-E4AD-4331-9363-AE18E1BAC694}']
    function GetItems(Index: Integer): IMTHeader;
    function GetValue(AName: String): Variant;
    function IndexOf(AKey: String): Integer;
    function Add(AName: String; AValue: Variant): IMTHeader;
    procedure SetValue(AName: String; AValue: Variant);
    function GetEnumerator: TEnumerator<IMTHeader>;
    property Value[AName: String]: Variant read GetValue write SetValue;
    property Items[Index: Integer]: IMTHeader read GetItems;
  end;

  { IMSMessageType }

  IMTMessageType = interface(IMTInterface)
  ['{43D92E7F-F979-4409-8157-731D47FAE50D}']
    function GetMessageType(index: Integer): String;
    procedure SetmessageType(index: Integer; AValue: String);
    function GetEnumerator: TStringsEnumerator;
    procedure Add(AMessageTypeStr: String);
    property messageType[index: Integer]: String read GetMessageType write SetmessageType;
  end;

  { IMasstransitMsg }
  IMTMessage<T> = interface(IMTInterface)
  ['{DAC2DD41-0BF5-406B-8AFF-D62A1E254696}']
    function GetconversationId: String;
    function GetdestinationAddress: String;
    function Getheaders: IMTHeaders;
    function GetHost: IMtHost;
    function Getmessage: T;
    function GetMessageId: String;
    function GetmessageType: IMTMessageType;
    function GetsourceAddress: String;
    procedure SetconversationId(AValue: String);
    procedure SetdestinationAddress(AValue: String);
    procedure Setheaders(AValue: IMTHeaders);
    procedure SetHost(AValue: IMTHost);
    procedure Setmessage(AValue: T);
    procedure SetMessageId(AValue: String);
    procedure SetmessageType(AValue: IMTMessageType);
    procedure SetsourceAddress(AValue: String);
    property messageId: String read GetMessageId Write SetMessageId;
    property conversationId: String read GetconversationId write SetconversationId;
    property sourceAddress: String read GetsourceAddress write SetsourceAddress;
    property destinationAddress: String read GetdestinationAddress write SetdestinationAddress;
    property messageType: IMTMessageType read GetmessageType write SetmessageType;
    property message: T read Getmessage write Setmessage;
    property headers: IMTHeaders read Getheaders write Setheaders;
    property host: IMTHost read GetHost write SetHost;
  end;

implementation

end.
