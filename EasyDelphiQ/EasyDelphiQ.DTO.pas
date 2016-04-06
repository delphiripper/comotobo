unit EasyDelphiQ.DTO;

interface

Type
  AssemblyNameAttribute = class(TCustomAttribute)
  Private
    FName: String;
  Public
    property Name: String read FName write FName;
    constructor create(AName: String);
  end;

  TClassInformation = record
  Private
    FClassName: String;
    FUnitName: String;
    FAssemblyName: String;
    Function GetAssemblyNameFromRTI( Obj: TObject ): String;
  Public
    Function GetAssemblyName: String;
    Function GetQueueName(ASubscriberID: String): String;
    Function GetExchangeName: String;
    Function GetUnitName: String;
    Function GetClassName: String;
    Function GetFullyQualifiedClassName: String;
    Constructor Create( Obj: TObject );
  end;

implementation

uses
  System.Rtti;

{ AssemblyNameAttribute }

constructor AssemblyNameAttribute.create(AName: String);
begin
  FName := AName;
end;

{ TClassInformation }

Function TClassInformation.GetAssemblyNameFromRTI( Obj: TObject ): String;
var
  ctx: TRttiContext;
  T: TRttiType;
  A: TCustomAttribute;
Begin
  ctx := TRttiContext.Create;
  Try
    T := ctx.GetType( obj.ClassInfo );
    For A in T.GetAttributes do
      if A is AssemblyNameAttribute then
        Result := AssemblyNameAttribute(A).Name;
  Finally
    ctx.Free;
  End;
End;

constructor TClassInformation.Create(Obj: TObject);
begin
  FUnitName     := Obj.UnitName;
  FClassName    := Obj.ClassName;
  FAssemblyName := GetAssemblyNameFromRTI( Obj );
end;

function TClassInformation.GetAssemblyName: String;
begin
  Result := FAssemblyName;
end;

function TClassInformation.GetClassName: String;
begin
  Result := FClassName;
end;

function TClassInformation.GetExchangeName: String;
begin
  Result := GetFullyQualifiedClassName + ':' + FAssemblyName;
end;

function TClassInformation.GetFullyQualifiedClassName: String;
begin
  Result := FUnitName + '.' + FClassName;
end;

function TClassInformation.GetQueueName(ASubscriberID: String): String;
begin
  Result := FUnitName + '_' + FClassName + ':' + FAssemblyName + '_' + ASubscriberID;
end;

function TClassInformation.GetUnitName: String;
begin
  Result := FUnitName;
end;

end.
