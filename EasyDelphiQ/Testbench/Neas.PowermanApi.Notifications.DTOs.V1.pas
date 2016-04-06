unit Neas.PowermanApi.Notifications.DTOs.V1;

interface

Uses
  System.Generics.Collections, EasyDelphiQ.DTO;

Type
  {$SCOPEDENUMS ON}
  TDataSerieTypeV1 = ( ConsPlanNoRegMWh, ConsPrognosisMWh,
                       IntegrationTest1, IntegrationTest2, IntegrationTest3,
                       IntradayActivated_MWH_DOWN, IntradayActivated_MWH_UP,
                       IntradayActivated_VALUE_DOWN, IntradayActivated_VALUE_UP,
                       ManRsvActivation_Balance_MWH_DOWN, ManRsvActivation_Balance_MWH_UP,
                       ManRsvActivation_Balance_VALUE_DOWN, ManRsvActivation_Balance_VALUE_UP,
                       ManRsvActivation_Special_MWH_DOWN, ManRsvActivation_Special_MWH_UP,
                       ManRsvActivation_Special_VALUE_DOWN, ManRsvActivation_Special_VALUE_UP,
                       ProdPlanNoRegLowerMWh, ProdPlanNoRegMWh, ProdPlanNoRegUpperMWh,
                       ProdPrognosisLowerMWh, ProdPrognosisMWh, ProdPrognosisUpperMWh,
                       SpotBlockConsPlanMWh, SpotBlockProdPlanMWh,
                       SpotConsPlanMWh, SpotProdPlanMWh,
                       Undefined );

  TUnitTypeV1 = ( DKK, MWh, Undefined );

  TDataStatus = ( Approved, Estimated, Manual, Missing, Undefined );
  {$SCOPEDENUMS OFF}

  TDatapoint = class
  Strict Private
  private
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FStatus: TDataStatus;
    FValue: Double;
  Public
    Property StartTime : TDateTime   read FStartTime write FStartTime;
    Property EndTime   : TDateTime   read FEndTime   write FEndTime;
    Property Status    : TDataStatus read FStatus    write FStatus;
    Property Value     : Double      read FValue     write FValue;
    Constructor Create;
  end;

  TDatapointList = TObjectList<TDataPoint>;

  ProductionGroupDataSerieV1 = Class
  Strict private
    FType              : TDataSerieTypeV1;
    FProductionGroupID : Integer;
    FUnitType          : TUnitTypeV1;
    FUpdateTime        : TDateTime;
    FDataPoints        : TDatapointList;
  public
    Property &Type             : TDataSerieTypeV1 read FType              write FType;
    Property ProductionGroupID : Integer          read FProductionGroupID write FProductionGroupID;
    Property UnitType          : TUnitTypeV1      read FUnitType          write FUnitType;
    Property UpdateTime        : TDateTime        read FUpdateTime        write FUpdateTime;
    Property DataPoints        : TDatapointList   read FDataPoints;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  //Exchange name: {Namespace/UnitName}.{ClassName}:{AssemblyName}
  //  Neas.PowermanApi.Notifications.DTOs.V1.ProductionGroupDataSerieCollectionV1:Neas.PowermanApi.Notifications.DTOs
  [AssemblyName('Neas.PowermanApi.Notifications.DTOs')]
  ProductionGroupDataSerieCollectionV1 = Class( TObjectList<ProductionGroupDataSerieV1> )
  Public
    Constructor Create; Reintroduce;
  End;

implementation

Uses
  System.Math;

{ TDatapointList }

constructor TDatapoint.Create;
begin
  FStartTime := 0;
  FEndTime   := 0;
  FStatus    := TDataStatus.Undefined;
  FValue     := NAN;
end;

{ DataSerieV1 }

constructor ProductionGroupDataSerieV1.Create;
begin
  FType              := TDataSerieTypeV1.Undefined;
  FProductionGroupID := -1;
  FUnitType          := TUnitTypeV1.Undefined;
  FUpdateTime        := 0;
  FDataPoints        := TDatapointList.Create;
end;

destructor ProductionGroupDataSerieV1.Destroy;
begin
  FDataPoints.Free;
  inherited;
end;

{ ProductionGroupDataSerieCollectionV1 }

constructor ProductionGroupDataSerieCollectionV1.Create;
begin
  inherited Create( True );
end;

end.
