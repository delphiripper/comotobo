unit WizardWin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TWizardForm = class(TForm)
    ButtonImport: TButton;
    ButtonCancel: TButton;
    EditURL: TEdit;
    ButtonGet: TButton;
    Timer1: TTimer;
    LabelInvalidJSON: TLabel;
    Shape1: TShape;
    EditName: TEdit;
    Label1: TLabel;
    PanelJSON: TPanel;
    MemoJSON: TMemo;
    PanelSource: TPanel;
    MemoSource: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    procedure ButtonImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonGetClick(Sender: TObject);
    procedure MemoJSONChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure EditURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditNameChange(Sender: TObject);
    procedure EditURLKeyPress(Sender: TObject; var Key: Char);
    procedure EditURLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FURL: String;
    FDTOSource: TStringList;
    Function IsValidJSON: Boolean;
    procedure CheckValidJSON;
    procedure NeedCheck;
  public
    Function Execute: String;
  end;

var
  WizardForm: TWizardForm;

implementation

Uses
  JSON, JSON2DTO;

{$R *.dfm}

procedure TWizardForm.ButtonImportClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TWizardForm.ButtonGetClick(Sender: TObject);
begin
  FURL := Trim( EditURL.Text );
  MemoJSON.Text := TJSON.Get( FURL ); //Will start timer
  Timer1.Enabled := False; //Cancel timer
  CheckValidJSON;
end;

procedure TWizardForm.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TWizardForm.EditNameChange(Sender: TObject);
Begin
  NeedCheck;
End;

procedure TWizardForm.NeedCheck;
begin
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TWizardForm.EditURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  Begin
    Key := 0;
    ButtonGet.Click;
  End;
end;

procedure TWizardForm.EditURLKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Key := #0;
end;

procedure TWizardForm.EditURLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Key := 0;
end;

function TWizardForm.Execute: String;
begin
  if (ShowModal = mrOk) and
     (FDTOSource.Count > 0) then
    Result := FDTOSource.Text
  else
    Result := '';
end;

procedure TWizardForm.FormCreate(Sender: TObject);
begin
  FURL := '';
  FDTOSource := TStringList.Create;
  CheckValidJSON;
end;

procedure TWizardForm.FormDestroy(Sender: TObject);
begin
  FDTOSource.Free;
end;

procedure TWizardForm.MemoJSONChange(Sender: TObject);
begin
  NeedCheck;
end;

procedure TWizardForm.Timer1Timer(Sender: TObject);
Begin
  Timer1.Enabled := False;
  CheckValidJSON;
End;

function TWizardForm.IsValidJSON: Boolean;
var
  JSON: TJSON_Element;
begin
  Result := False;
  JSON := nil;
  Try
    JSON := TJSON.ParseText( MemoJSON.Text );
    //Pretty print
    MemoJSON.OnChange := nil;
    MemoJSON.Text := JSON.ToJSON;
    MemoJSON.OnChange := MemoJSONChange;
    Result := True;
  Except
    on E: Exception do
      LabelInvalidJSON.Caption := 'Invalid JSON (' + E.Message + ')';
  End;
  JSON.Free;
end;

procedure TWizardForm.CheckValidJSON;
begin
  FDTOSource.Clear;
  ButtonImport.Enabled     := False;
  LabelInvalidJSON.Visible := True;
  if IsValidJSON then
  Try
    TDTOGenerator.Parse( MemoJSON.Text, EditName.Text, 'DTO.' + EditName.Text, FDTOSource, FURL );
    MemoSource.Lines.Assign( FDTOSource );
    ButtonImport.Enabled     := True;
    LabelInvalidJSON.Visible := False;
  Except
    On E: Exception do
      MemoSource.Lines.Text := E.Message;
  End;
end;

end.
