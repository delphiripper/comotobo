unit TimeseriesDTO;

interface

Uses
  System.Classes, System.Generics.Collections;

Type
  TDatapoint = Class
  private
    FStartTimeUTC: TDateTime;
    FEndTimeUTC: TDateTime;
    FRepresentedCapacity: Double;
    FValueMWh: Double;
  Public
    Property StartTimeUTC: TDateTime read FStartTimeUTC write FStartTimeUTC;
    Property EndTimeUTC: TDateTime read FEndTimeUTC write FEndTimeUTC;
    Property RepresentedCapacity: Double read FRepresentedCapacity write FRepresentedCapacity;
    Property ValueMWh: Double read FValueMWh write FValueMWh;
    Constructor Create;
  End;

  TTimeseries = Class
  private
    FProductionGroupID: Integer;
    FType: String;
    FDatapoints: TObjectList<TDatapoint>;
  Public
    Property ProductionGroupID: Integer read FProductionGroupID write FProductionGroupID;
    Property &Type: String read FType write FType;
    Property Datapoints: TObjectList<TDatapoint> read FDatapoints;
    Constructor Create;
    Destructor Destroy; Override;
  End;

implementation

Uses
  System.Math;

{ TDatapoint }

constructor TDatapoint.Create;
begin
  FStartTimeUTC := 0;
  FEndTimeUTC := 0;
  FRepresentedCapacity := NAN;
  FValueMWh := NAN;
end;

{ TTimeseries }

constructor TTimeseries.Create;
begin
  FType := '';
  FProductionGroupID := 0;
  FDatapoints := TObjectList<TDatapoint>.Create;
end;

destructor TTimeseries.Destroy;
begin
  FDatapoints.Free;
  inherited;
end;

end.
