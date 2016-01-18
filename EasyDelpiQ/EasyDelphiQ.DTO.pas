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

Function GetAssemblyName( Obj: TObject ): String;
Function GetExchangeName( Obj: TObject ): String;
Function GetQueueeName( Obj: TObject; SubscriberID: String ): String;

implementation

uses
  System.Rtti;

Function GetAssemblyName( Obj: TObject ): String;
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

Function GetExchangeName( Obj: TObject ): String;
Begin
  Result := Obj.UnitName + '_' + Obj.ClassName + ':' + GetAssemblyName( Obj );
End;

Function GetQueueeName( Obj: TObject; SubscriberID: String ): String;
Begin
  Result := Obj.UnitName + '_' + Obj.ClassName + ':' + GetAssemblyName( Obj ) + '_' + SubscriberID;
End;

{ Assembly }

constructor AssemblyNameAttribute.create(AName: String);
begin
  FName := AName;
end;

end.
