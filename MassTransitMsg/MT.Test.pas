unit MT.Test;

interface
uses MT.Interfaces, MT.Classes, Korus.Esphere.Message.DocumentEvent, MT.Serializer;

procedure Test;

implementation


procedure Test;
var I: IMTMessage<IDocumentEvent>;
    s: String;
    SS: IMTSerializer;
begin
  i := TMTMessage<IDocumentEvent>.Create(TDocumentEvent.Create);
  i.headers.Add('1', 2);
  with i.message do
   begin
     id := 1980062894;
     eventType := 10;
     documentId := 128375281;
   end;
   SS := TMTSerializer.Create();
   SS.RegisterCustomSerializer(TypeInfo(IMTHeaders), TMTHeaderSerializer);
   SS.RegisterCustomSerializer(TypeInfo(IMTMessageType), TMTMessageTypeSerializer);
   S := SS.SerializeInterface(I);

end;

initialization
  Test;

end.
