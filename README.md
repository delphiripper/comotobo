# comotobo
The "Code Monkeys ToolBox" (CoMoToBo) is a small collection of useful Delphi classes/APIs.

## EasyDelphiQ
An easy-to-use Delphi AIP for RabbitMQ. Inspired by the .Net EasyNetQ client.
Uses the AMQP protocol to send and receive messages.
EasyDelphiQ uses JSON by default for serializing Data Transfer Objects (DTOs) in messages.
DTOs are just simple Delphi classes with standard properties - but they must have a simple, parameterless constructor.

### DTOs

A DTO could look like this:
```Delphi
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

constructor TestDTO.Create;
begin
  FID   := 0;
  FName := '';
end;  
```

The `EasyDelphiQ.DTO` unit in the uses clause is not necessary, but it allows you to use the AssemblyName attribute which makes EasyDelphiQ compatible with EasyNetQ naming conventions.

### Publishing messages

Simple example of how to publish a message:
```Delphi
Bus := RabbitHutch.CreateBus( 'host=localhost;username=TestUser;password=password' );
DTO := TestDTO.Create;
Try
  DTO.ID   := 42;
  DTO.Name := 'Zaphod';
  Bus.Publish( DTO );
Finally
  DTO.Free;
  Bus.Free;
End;	
```

The code above will connect to localhost and publish the DTO to an exchange named "Some.namespace.TestDTO:MyAssembly".
The exchange naming convention is used by EasyNetQ; [Namespace].[Classname]:[Assemblyname]

### Getting a single message from a queue

```Delphi
DTO := Bus.Get<TestDTO>( 'MySubscriberID' );
if DTO = nil then
  Memo.Lines.Add( 'No messages in queue' )
else
Try
  Memo.Lines.Add( 'Received:' );
  Memo.Lines.Add( '  DTO.ID:   ' + DTO.ID.ToString );
  Memo.Lines.Add( '  DTO.Name: ' + DTO.Name );
Finally
  DTO.Free;
End;
```


### Subscribing to a queue

To subscribe to a RabbitMQ queue, simply do this:
```Delphi
  Subscription := Bus.Subscribe<TestDTO>( 'MySubscription', Handler );
```

The handler method could look like this:
```Delphi
procedure TMainForm.Handler(var Msg: TestDTO);
begin
  DoSomethingInteresting( Msg );
end;
```

If no exceptions are raised in the Handler method, then the message is acknowledged and removed from the queue.
The `Msg` object in the example will be destroyed automatically by EasyDelphiQ. 
If you want to keep the object (put it in a list, for example) then set `Msg` to `nil` in the `Handler`.
It is important to know, however, that the `Handler` method is **NOT** called in the main thread context, so the code here **MUST** be thread safe.

Here is a simple example of how to use the DTO in the main thread context:

```Delphi
procedure TMainForm.Handler(var Msg: TestDTO);
var
  DTO: TestDTO;
begin
  DTO := Msg; //Necessary to capture the object in the anonymous method below
  TThread.Queue( nil,
    Procedure
    begin
      Memo1.Lines.Add( 'Received:' );
      Memo1.Lines.Add( '  DTO.ID:   ' + DTO.ID.ToString );
      Memo1.Lines.Add( '  DTO.Name: ' + DTO.Name );
      DTO.Free; //Free the object here - when we are done with it
    end );
  Msg := nil; //Don't free the object here
end;
```


## AMQP
The implementation of the AMQP protocol which RabbitMQ uses for communication.
EasyDelphiQ is built on top of this library.

## DJSON
The DJSON unit makes reading and writing JSON easy (using RTTI).

For example; parsing JSON array of objects directly into a TObjectList<>:
```Delphi
JSON := '[ {"ID":4, "Name":"Tom" }, {"ID":7, "Name":"Julia" } ]';
TJSONParser.Parse( JSON, List );
```

Serializing an object is equally easy;
```Delphi
JSON := TJSONSerializer.Serialize( List );
```

## XPath
This little unit makes parsing HTML files easy.

Given this html:
```Html
<html b="12">
  <p>Not me!</p>
  <div>
    <p>Hello</p>
    <p>Cruel</p>
  </div>
  <div>
    <p>World</p>
  </div>
</html>
```

...is loaded into the Dom viariable (TDom) this line:
```Delphi
TXPath.Eval( Dom, '/html/div//p' );
```

Will return an array containing these 3 "p" nodes:
```Html
    <p>Hello</p>
    <p>Cruel</p>
    <p>World</p>
```

## Examples
Simple examples are included in the source.
