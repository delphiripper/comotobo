unit Some.namespace;

interface

Uses
  EasyDelphiQ.DTO;

Type
  [AssemblyName('MyAssembly')]
  TestDTO = Class
  private
    FName: String;
    FID: Integer;
  Public
    Property ID   : Integer read FID   write FID;
    Property Name : String  read FName write FName;
    Constructor Create;
  End;

implementation

{ TestDTO }

constructor TestDTO.Create;
begin
  FID := 0;
  FName := '';
end;

end.
