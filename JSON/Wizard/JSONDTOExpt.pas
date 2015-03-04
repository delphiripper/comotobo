unit JSONDTOExpt;

interface

procedure Register;

implementation

uses
  System.Classes, System.SysUtils, WinAPI.Windows, VCL.Dialogs, ToolsApi, WizardWin;

{$R JSON.res}

type
  TGxModuleCreatorWizard = class( TNotifierObject, IOTAWizard, IOTARepositoryWizard,
                                  IOTARepositoryWizard60, IOTARepositoryWizard80, IOTAFormWizard)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard80
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
  end;

  TGxModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FSource: String;
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    Constructor Create( ASource: String ); Reintroduce;
  end;

  TGxSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;


{ TGxModuleCreatorWizard }

procedure TGxModuleCreatorWizard.Execute;
var
  Source: String;
begin
  WizardForm := TWizardForm.Create(nil);
  Try
    Source := WizardForm.Execute;
    if Source <> '' then
      (BorlandIDEServices as IOTAModuleServices).CreateModule(TGxModuleCreator.Create(Source));
  Finally
    WizardForm.Free;
  End;
end;

function TGxModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'Jesper B. Christensen';
end;

function TGxModuleCreatorWizard.GetComment: string;
begin
  Result := 'JSON DTO Importer';
end;

function TGxModuleCreatorWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TGxModuleCreatorWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  Category: IOTAGalleryCategory;
  CatManager: IOTAGalleryCategoryManager;
begin
  CatManager := (BorlandIDEServices as IOTAGalleryCategoryManager);
  Assert(Assigned(CatManager));
  Category := CatManager.FindCategory(sCategoryDelphiNewFiles);
  Assert(Assigned(Category));
  Result := Category;
end;

function TGxModuleCreatorWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TGxModuleCreatorWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'JSONICON');
end;

function TGxModuleCreatorWizard.GetIDString: string;
begin
  Result := 'JBC.JSONDTOCreatorWizard';
end;

function TGxModuleCreatorWizard.GetName: string;
begin
  Result := 'JSON DTO Importer';
end;

function TGxModuleCreatorWizard.GetPage: string;
begin
  Result := 'New';
end;

function TGxModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{ TGxModuleCreator }

constructor TGxModuleCreator.Create( ASource: String );
begin
  inherited Create;
  FSource := ASource;
end;

procedure TGxModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

function TGxModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TGxModuleCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sUnit;
end;

function TGxModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TGxModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TGxModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TGxModuleCreator.GetImplFileName: string;
begin
//  Result := 'NewDTO.pas';
  Result := '';
end;

function TGxModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TGxModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TGxModuleCreator.GetOwner: IOTAModule;
//var
//  ModuleServices: IOTAModuleServices;
//  Module: IOTAModule;
//  NewModule: IOTAModule;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  // You may prefer to return the project group's ActiveProject instead
//  Result := nil;
//  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
//  Module := ModuleServices.CurrentModule;
//
//  if Module <> nil then
//  begin
//    if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
//      Result := NewModule
//
//    else if Module.OwnerModuleCount > 0 then
//    begin
//      NewModule := Module.OwnerModules[0];
//      if NewModule <> nil then
//        if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
//          Result := nil;
//    end;
//  end;
end;

function TGxModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TGxModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TGxModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TGxModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TGxModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TGxSourceFile.Create(Format(FSource, [ModuleIdent, FormIdent,
                           AncestorIdent, FormIdent, FormIdent]));
end;

function TGxModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TGxSourceFile }

constructor TGxSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TGxSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TGxSourceFile.GetSource: string;
begin
  Result := FSource;
end;

procedure Register;
begin
  RegisterPackageWizard(TGxModuleCreatorWizard.Create);
end;


initialization
//  InitModuleSources;
finalization
//  DoneModuleSources;
end.
