Unit DTO;

Interface

Uses
  System.Generics.Collections;

Type
  TDatapointsItem = class
  Private
    FStartTimeUTC: String;
    FEndTimeUTC: String;
    FRepresentedCapacity: Integer;
    FValueMWh: Double;
  Public
    Property StartTimeUTC: String read FStartTimeUTC write FStartTimeUTC;
    Property EndTimeUTC: String read FEndTimeUTC write FEndTimeUTC;
    Property RepresentedCapacity: Integer read FRepresentedCapacity write FRepresentedCapacity;
    Property ValueMWh: Double read FValueMWh write FValueMWh;
    Constructor Create;
  End;

  TDTOItem = class
  Private
    FProductionGroupId: Integer;
    FType: String;
    FDatapoints: TObjectList<TDatapointsItem>;
  Public
    Property ProductionGroupId: Integer read FProductionGroupId write FProductionGroupId;
    Property &Type: String read FType write FType;
    Property Datapoints: TObjectList<TDatapointsItem> read FDatapoints;
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

{ TDatapointsItem }

Constructor TDatapointsItem.Create;
Begin
  FStartTimeUTC := '';
  FEndTimeUTC := '';
  FRepresentedCapacity := 0;
  FValueMWh := 0;
End;

{ TDTOItem }

Constructor TDTOItem.Create;
Begin
  FProductionGroupId := 0;
  FType := '';
  FDatapoints := TObjectList<TDatapointsItem>.Create;
End;

Destructor TDTOItem.Destroy;
Begin
  FDatapoints.Free;
  Inherited;
End;

End.

