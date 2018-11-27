unit MT.RTTI;
interface
type
  MTMessageTypeAttribute = class(TCustomAttribute)
  Private
    FName: String;
  Public
    property Name: String read FName write FName;
    constructor create(AName: String);
  end;

  MTAssemblyNameAttribute = class(MTMessageTypeAttribute);

  MTExchangeNameAttribute = class(MTMessageTypeAttribute);
  MTQueueNameAttribute = class(MTMessageTypeAttribute);

  TMTClassInformation = record
  Private
    FClassName: String;
    FUnitName: String;
    FAssemblyName: String;
    FMessageType: string;
    FQueueName: String;
    FExchangeName: string;
  Public
    Function GetMessageType: String;
    Function GetAssemblyName: String;
    Function GetQueueName(ASubscriberID: String): String;
    Function GetExchangeName: String;
    Function GetUnitName: String;
    Function GetClassName: String;
    Function GetFullyQualifiedClassName: String;
    Constructor Create( Obj: TObject );
  end;

implementation

uses RTTI;

{ MessageTypeAttribute }

constructor MTMessageTypeAttribute.create(AName: String);
begin
 FName := AName;
end;

{ TMTClassInformation }

constructor TMTClassInformation.Create(Obj: TObject);

procedure GetNameFromRTI(Obj: TObject);
var
  ctx: TRttiContext;
  T: TRttiType;
  A: TCustomAttribute;
Begin
  ctx := TRttiContext.Create;
  Try
    T := ctx.GetType( obj.ClassInfo );
    For A in T.GetAttributes do
      if A is MTAssemblyNameAttribute then
        FAssemblyName := MTAssemblyNameAttribute(A).Name
      else if A is MTMessageTypeAttribute then
        FMessageType := MTMessageTypeAttribute(A).Name
      else if A is MTQueueNameAttribute then
        FQueueName := MTQueueNameAttribute(A).Name
      else if A is MTExchangeNameAttribute then
        FExchangeName := MTExchangeNameAttribute(A).Name
  Finally
    ctx.Free;
  End;
End;


begin
 FUnitName := Obj.UnitName;
 FClassName := Obj.ClassName;
 GetNameFromRTI(Obj);
end;

function TMTClassInformation.GetAssemblyName: String;
begin
 if FAssemblyName = '' then
   Result := GetUnitName
 else
  Result := FAssemblyName;
end;

function TMTClassInformation.GetClassName: String;
begin
 Result := FClassName;
end;

function TMTClassInformation.GetExchangeName: String;
begin
 if FExchangeName = '' then
   Result := GetFullyQualifiedClassName + ':' + FAssemblyName
 else
   Result := FExchangeName;
end;

function TMTClassInformation.GetFullyQualifiedClassName: String;
begin
  Result := FUnitName + '.' + FClassName;
end;

function TMTClassInformation.GetMessageType: String;
begin
 if FMessageType = '' then
  Result := FClassName
 else
  Result := FMessageType;
end;

function TMTClassInformation.GetQueueName(ASubscriberID: String): String;
begin
  if FQueueName = '' then
    Result := FUnitName + '_' + FClassName + ':' + FAssemblyName + '_' + ASubscriberID
  else
   Result := FQueueName;
end;

function TMTClassInformation.GetUnitName: String;
begin
 Result := FUnitName;
end;

end.
