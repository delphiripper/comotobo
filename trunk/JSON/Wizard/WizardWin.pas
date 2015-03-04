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
    Button3: TButton;
    Timer1: TTimer;
    LabelInvalidJSON: TLabel;
    Shape1: TShape;
    EditName: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    MemoJSON: TMemo;
    procedure ButtonImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure MemoJSONChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FURL: String;
    FDTOSource: TStringList;
    Function IsValidJSON: Boolean;
    procedure CheckValidJSON;
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
var
  DTOGen: TDTOGenerator;
begin
  DTOGen := nil;
  Try
    if IsValidJSON then
    Begin
      DTOGen := TDTOGenerator.Create;
      DTOGen.Parse( EditName.Text, MemoJSON.Text );
      DTOGen.WritePDO( EditName.Text, FDTOSource, FURL );
      Close;
    End;
  Finally
    DTOGen.Free;
  End;
end;

procedure TWizardForm.Button3Click(Sender: TObject);
begin
  FURL := Trim( EditURL.Text );
  MemoJSON.Text := TJSON.Get( FURL );
end;

procedure TWizardForm.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

function TWizardForm.Execute: String;
begin
  ShowModal;
  if FDTOSource.Count > 0 then
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
  Timer1.Enabled := False;
  Timer1.Enabled := True;
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
  ButtonImport.Enabled     := IsValidJSON;
  LabelInvalidJSON.Visible := Not ButtonImport.Enabled;
end;

end.
