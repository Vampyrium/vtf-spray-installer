unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Registry, SprayFileHandler, Vcl.FileCtrl, Winapi.Shellapi,
  ChangelogFrmU, AboutFrmU;

type
  TForm1 = class(TForm)
    SUsernameCmb: TComboBox;
    SUsernameLbl: TLabel;
    GameCmb: TComboBox;
    GameLbl: TLabel;
    GoBtn: TButton;
    Image1: TImage;
    Image2: TImage;
    VTFFileBox: TEdit;
    VTFFileLbl: TLabel;
    VTFOpenBtn: TButton;
    OpenDlg: TOpenDialog;
    SAppsDirBox: TEdit;
    SAppsLbl: TLabel;
    SAppsDirChooseBtn: TButton;
    StatusLbl: TLabel;
    BatchOneTogBtn: TButton;
    VTFFileList: TListBox;
    GoListBtn: TButton;
    BatchPnl: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    VTFFileListDelSelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SanityChange(Sender: TObject);
    procedure VTFOpenBtnClick(Sender: TObject);
    procedure SAppsDirChooseBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure BatchOneTogBtnClick(Sender: TObject);
    procedure VTFFileListDelSelBtnClick(Sender: TObject);
    procedure GoListBtnClick(Sender: TObject);
    procedure VTFFileListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Image2Click(Sender: TObject);
  private
    InBatchMode: boolean;
    procedure SanityCheck;
    procedure AddUsernameToHistory(username: string);
    procedure ToggleBatchMode;
    procedure WMDROPFILES(var msg : TWMDropFiles) ; message WM_DROPFILES;
  public
    SprayInstaller: TSprayFileHandler;
  end;

var
  Form1: TForm1;

const
  VSI_VERSION: integer = 105000000; {mMNNRRBBBB}

implementation

{$R *.dfm}

procedure TForm1.ToggleBatchMode;
begin
  InBatchMode := not InBatchMode;
  if InBatchMode then begin
    VTFFileBox.Visible := false;
    VTFFileLbl.Visible := false;
    VTFOpenBtn.Visible := false;
    SAppsLbl.Top := 101;
    SAppsDirBox.Top := 98;
    SAppsDirChooseBtn.Top := 98;
    GoBtn.Top := 125;
    BatchOneTogBtn.Top := 125;
    BorderStyle := bsSizeable;
    Height := 400;
    Constraints.MinHeight := 400;
    BatchPnl.Visible := true;
    BatchOneTogBtn.Caption := 'Single Mode';
    VTFFileBox.Text := '';
    StatusLbl.Caption := 'Drag and drop VTF files onto this window to continue.';
    BorderIcons := [biSystemMenu,biMinimize,biMaximize];
  end else begin
    VTFFileBox.Visible := true;
    VTFFileLbl.Visible := true;
    VTFOpenBtn.Visible := true;
    SAppsLbl.Top := 128;
    SAppsDirBox.Top := 125;
    SAppsDirChooseBtn.Top := 125;
    GoBtn.Top := 152;
    BatchOneTogBtn.Top := 152;
    Constraints.MinHeight := 0;
    Height := 230;
    Width := 415;
    BatchPnl.Visible := false;
    VTFFileList.Clear;
    WindowState := wsNormal;
    BorderStyle := bsSingle;
    BatchOneTogBtn.Caption := 'Batch Mode';
    GoListBtn.Enabled := false;
    StatusLbl.Caption := 'Input a valid VTF file and Steamapps folder to continue.';
    BorderIcons := [biSystemMenu,biMinimize];
  end;
end;

procedure TForm1.WMDROPFILES(var msg: TWMDropFiles) ;
const
  MAXFILENAME = 4095;
var
  cnt, fileCount : integer;
  fileName : array [0..MAXFILENAME] of char;
begin
  // how many files dropped?
  fileCount := DragQueryFile(msg.Drop, $FFFFFFFF, fileName, MAXFILENAME) ;

  // query for file names
  for cnt := 0 to -1 + fileCount do
  begin
    DragQueryFile(msg.Drop, cnt, fileName, MAXFILENAME) ;

    if not InBatchMode then VTFFileBox.Text := fileName
      else VTFFileList.Items.Append(fileName);
  end;

  if InBatchMode then begin
    GoListBtn.Enabled := true;
    StatusLbl.Caption := 'Ready.';
  end;

  //release memory
  DragFinish(msg.Drop) ;
end;

procedure TForm1.BatchOneTogBtnClick(Sender: TObject);
begin
  ToggleBatchMode;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  regReaderSteam: TRegistry;
  regReaderVamp: TRegistry;
  i: NativeInt;
  LastVersion: integer;
begin
  LastVersion := 0;
  Height := 230;
  InBatchMode := false;
  regReaderSteam := TRegistry.Create();
  regReaderSteam.RootKey := HKEY_CURRENT_USER;
  regReaderVamp := TRegistry.Create();
  regReaderVamp.RootKey := HKEY_CURRENT_USER;
  if regReaderSteam.KeyExists('Software\Valve\Steam') then begin
    regReaderSteam.OpenKeyReadOnly('Software\Valve\Steam');
    SAppsDirBox.Text := IncludeTrailingBackslash(StringReplace(regReaderSteam.ReadString('SteamPath'),'/','\',[rfReplaceAll]))+'steamapps';
  end;
  if regReaderVamp.KeyExists('Software\Vampyrium\SprayInstaller') then begin
    regReaderVamp.OpenKey('Software\Vampyrium\SprayInstaller',false);
    i := 0;
    while regReaderVamp.ValueExists(Format('Username%d',[i])) do begin
      SUsernameCmb.Items.Append(regReaderVamp.ReadString(Format('Username%d',[i])));
      Inc(i);
    end;
    if regReaderVamp.ValueExists('LastUsername') then begin
      SUsernameCmb.Text := regReaderVamp.ReadString('LastUsername');
    end;
    if regReaderVamp.ValueExists('LastVersion') then begin
      LastVersion := regReaderVamp.ReadInteger('LastVersion');
    end;
    if LastVersion < VSI_VERSION then
      regReaderVamp.WriteInteger('LastVersion',VSI_VERSION);
  end;
  regReaderSteam.CloseKey;
  regReaderSteam.Free;
  regReaderVamp.CloseKey;
  regReaderVamp.Free;

  if ParamCount >= 1 then VTFFileBox.Text := ParamStr(1);

  if LastVersion < VSI_VERSION then
    ChangelogFrmU.NewerVersion := true;

  SprayInstaller := TSprayFileHandler.Create;

  SprayInstaller.PopulateStringList(GameCmb.Items);
  GameCmb.ItemIndex := 0;

  DragAcceptFiles(Handle,True);
end;

procedure TForm1.GoBtnClick(Sender: TObject);
var
  procresult: NativeInt;
begin
  procresult := SprayInstaller.ExportFullSpray(VTFFileBox.Text,SAppsDirBox.Text,SUsernameCmb.Text,GameCmb.ItemIndex);
  if procresult = SFH_EC_SUCCESS then begin
    StatusLbl.Color := $00008000;
    StatusLbl.Caption := Format('Successfully added spray to %s.',[GameCmb.Text]);
  end else if procresult = SFH_EC_SPRAY_TOO_LARGE then begin
    StatusLbl.Color := $00800000;
    StatusLbl.Caption := Format('Spray is too big for %s.',[GameCmb.Text]);
  end else if procresult = SFH_EC_INVALID_VTF_FILE then begin
    StatusLbl.Color := $0000407F;
    StatusLbl.Caption := 'This file is not a valid VTF file.';
  end else if procresult = SFH_EC_GAME_NOT_INSTALLED then begin
    StatusLbl.Color := $00808080;
    StatusLbl.Caption := Format('%s is not installed.',[GameCmb.Text]);
  end else begin
    StatusLbl.Color := $00000080;
    StatusLbl.Caption := Format('Failed to add spray to %s.',[GameCmb.Text]);
  end;
  AddUsernameToHistory(SUsernameCmb.Text);
end;

procedure TForm1.GoListBtnClick(Sender: TObject);
var
  successes,failures,toobigs,invformats,i,procresult: NativeInt;
begin
  successes := 0;
  failures := 0;
  toobigs := 0;
  invformats := 0;
  for i := 0 to VTFFileList.Count - 1 do begin
    procresult := SprayInstaller.ExportFullSpray(VTFFileList.Items.Strings[i],SAppsDirBox.Text,SUsernameCmb.Text,GameCmb.ItemIndex);
    if procresult = SFH_EC_SUCCESS then
      Inc(successes)
    else if procresult = SFH_EC_SPRAY_TOO_LARGE then
      Inc(toobigs)
    else if procresult = SFH_EC_INVALID_VTF_FILE then
      Inc(invformats)
    else if procresult = SFH_EC_GAME_NOT_INSTALLED then begin
      StatusLbl.Color := $00808080;
      StatusLbl.Caption := Format('%s is not installed.',[GameCmb.Text]);
      exit;
    end else
      Inc(failures);
    StatusLbl.Caption := Format('%d/%d sprays processed.',[i+1,VTFFileList.Count]);
    StatusLbl.Repaint;
  end;
  if failures = 0 then begin
    StatusLbl.Color := $00008000;
    StatusLbl.Caption := Format('Successfully added %d spray(s) to %s.',[successes,GameCmb.Text]);
  end else if successes > 0 then begin
    StatusLbl.Color := $00008080;
    StatusLbl.Caption := Format('Successfully added %d spray(s) to %s; %d failed.',[successes,GameCmb.Text,failures]);
  end else begin
    StatusLbl.Color := $00000080;
    StatusLbl.Caption := Format('Failed to add %d spray(s) to %s.',[failures,GameCmb.Text]);
  end;
  if (failures = 0) and (successes = 0) then StatusLbl.Color := $00000000;
  if toobigs > 0 then begin
    StatusLbl.Color := StatusLbl.Color + $00800000;
    StatusLbl.Caption := StatusLbl.Caption + Format(' %d spray(s) were too big for the game.',[toobigs]);
  end;
  if invformats > 0 then begin
    StatusLbl.Color := StatusLbl.Color + $0000407F;
    StatusLbl.Caption := StatusLbl.Caption + Format(' %d file(s) were in the wrong format.',[invformats]);
  end;
  VTFFileList.Clear;
  GoListBtn.Enabled := false;
  AddUsernameToHistory(SUsernameCmb.Text);
end;

procedure TForm1.Image2Click(Sender: TObject);
begin
  AboutFrm.ShowModal;
end;

procedure TForm1.SanityCheck;
begin
  StatusLbl.Color := $00252525;
  GoBtn.Enabled := FileExists(VTFFileBox.Text) and DirectoryExists(SAppsDirBox.Text);
  if GoBtn.Enabled then begin
    StatusLbl.Caption := 'Ready.';
  end else begin
    StatusLbl.Caption := 'Input a valid VTF file and Steamapps folder to continue.';
  end;
end;

procedure TForm1.AddUsernameToHistory(username: string);
var
  NameInList: boolean;
  i: NativeInt;
  regWriter: TRegistry;
  ValueName: string;
begin
  i := 0;
  NameInList := false;

  while i < SUsernameCmb.Items.Count do begin
    if username = SUsernameCmb.Items.Strings[i] then
      NameInList := true;
    Inc(i);
  end;

  regWriter := TRegistry.Create;
  regWriter.RootKey := HKEY_CURRENT_USER;
  regWriter.OpenKey('Software\Vampyrium\SprayInstaller',true);

  if SUsernameCmb.Text <> '' then begin
    if NameInList = false then begin
      ValueName := Format('Username%d',[SUsernameCmb.Items.Count]);
      regWriter.WriteString(ValueName,SUsernameCmb.Text);
      SUsernameCmb.Items.Append(SUsernameCmb.Text);
    end;

    regWriter.WriteString('LastUsername',SUsernameCmb.Text);
  end;

  regWriter.CloseKey;
  regWriter.Free;
end;

procedure TForm1.SAppsDirChooseBtnClick(Sender: TObject);
var
  SDir: string;
begin
  if SelectDirectory('Browse for the Steamapps folder.','',SDir) then
    SAppsDirBox.Text := SDir;
end;

procedure TForm1.VTFFileListDelSelBtnClick(Sender: TObject);
begin
  VTFFileList.DeleteSelected;
  if VTFFileList.Count = 0 then GoListBtn.Enabled := false;
end;

procedure TForm1.VTFFileListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ([ssShift, ssAlt] * Shift = []) and (ssCtrl in Shift) and (Key = $41{A}) then
    VTFFileList.SelectAll;
end;

procedure TForm1.VTFOpenBtnClick(Sender: TObject);
begin
  if OpenDlg.Execute then VTFFileBox.Text := OpenDlg.FileName;
end;

procedure TForm1.SanityChange(Sender: TObject);
begin
  SanityCheck;
end;

end.
