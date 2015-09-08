# comotobo
The Code Monkeys ToolBox for Delphi is a small collections of units for Delphi.

## JSON
The JSON unit makes reading and writing JSON easy (using RTTI).

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
