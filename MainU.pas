unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, {System.Variants,} System.Classes, {Vcl.Graphics,}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, SprayFileHandler, Registry,
  Winapi.Shellapi, StatusFrmU, GameFolderHintFrmU, AboutFrmU, Vcl.FileCtrl, VTFPrevFrmU;

type
  TSprayErrorTypeCount = record
    Successes: NativeInt;
    GameNotInstalled: NativeInt;
    SprayTooLarge: NativeInt;
    InvalidVTFFile: NativeInt;
    IOError: NativeInt;
  end;

type
  TMain = class(TForm)
    GameDropPanel: TPanel;
    GameLabel: TLabel;
    GameComboBox: TComboBox;
    GameStatusLbl: TLabel;
    GamePathSelBtn: TButton;
    MainPnl: TPanel;
    ExistingSprayList: TListBox;
    ImportSpraysList: TListBox;
    ExistingSprayPnl: TPanel;
    ExistingSpraysLbl: TLabel;
    Splitter1: TSplitter;
    ImportSpraysPnl: TPanel;
    ImportSpraysLbl: TLabel;
    DeleteSprayBtn: TButton;
    ImportSpraysBtnPnl: TPanel;
    RemoveSpraysBtn: TButton;
    ImportSelBtn: TButton;
    ImportAllBtn: TButton;
    AddSpraysBtn: TButton;
    SprayImportDlg: TOpenDialog;
    ClearGamePathBtn: TButton;
    AboutBtnPnl: TPanel;
    AboutBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GameComboBoxChange(Sender: TObject);
    procedure ExistingSprayListClick(Sender: TObject);
    procedure ExistingSprayListKeyPress(Sender: TObject; var Key: Char);
    procedure DeleteSprayBtnClick(Sender: TObject);
    procedure AddSpraysBtnClick(Sender: TObject);
    procedure RemoveSpraysBtnClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ImportSelBtnClick(Sender: TObject);
    procedure ImportSpraysListClick(Sender: TObject);
    procedure ImportSpraysListKeyPress(Sender: TObject; var Key: Char);
    procedure ImportAllBtnClick(Sender: TObject);
    procedure ClearGamePathBtnClick(Sender: TObject);
    procedure GamePathSelBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure ExistingSprayListDblClick(Sender: TObject);
    procedure ExistingSprayListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    StatusFrm: TStatusFrm;
    procedure GameBoxChangeStatus;
    procedure ExistingSpraysListPopulate(const path: string);
    procedure PreviewSpray(index: NativeInt);
    procedure DeleteBtnDisablerEnabler;
    procedure ImportBtnsDisablerEnabler;
    procedure DeleteSpray;
    procedure AddSprayToImportList(const FileName: string);
    procedure RemoveSpraysFromImportList;
    function ImportSpray(index: NativeInt; var ErrorStruct: TSprayErrorTypeCount): NativeInt;
    procedure ImportSelectedSprays;
    procedure ImportAllSprays;
    procedure SprayStatusUpdate(const ErrorStruct: TSprayErrorTypeCount);
    function VTFValidate(const FileName: string): Boolean;
    function SelectGameDirectory(index: NativeInt): Boolean;
    procedure ClearGameDirectory(index: NativeInt);
    procedure ShowAboutForm;
    procedure WMDROPFILES(var msg : TWMDropFiles); message WM_DROPFILES;
  public
    InfoHandler: TSprayFileHandler;
  end;

var
  Main: TMain;
const
  clWindowText: Integer = -16777208;
  clBtnFace: Integer = -16777201;

implementation

{$R *.dfm}



procedure TMain.FormCreate(Sender: TObject);
var
  regReaderSteam: TRegistry;
  SteamApps: string;
  i: NativeInt;
begin
  StatusFrm := nil;
  regReaderSteam := TRegistry.Create;
  regReaderSteam.RootKey := HKEY_CURRENT_USER;
  if regReaderSteam.KeyExists('Software\Valve\Steam') then begin
    regReaderSteam.OpenKeyReadOnly('Software\Valve\Steam');
    SteamApps := IncludeTrailingPathDelimiter(StringReplace(regReaderSteam.ReadString('SteamPath'),'/','\',[rfReplaceAll]))+'steamapps\';
  end else begin
    ShowMessage('Steam is not installed on this computer.');
    Close;
  end;

  regReaderSteam.CloseKey;
  regReaderSteam.Free;

  InfoHandler := TSprayFileHandler.Create(SteamApps);

  InfoHandler.PopulateStringList(GameComboBox.Items);

  DragAcceptFiles(Handle,True);

  for i := 1 to ParamCount do begin
    ImportSpraysList.Items.Add(ParamStr(i));
  end;

end;

procedure TMain.PreviewSpray(index: NativeInt);
begin
  VTFPrevForm.LoadImage(InfoHandler.GetMainSprayPath(GameComboBox.ItemIndex) + ExistingSprayList.Items.Strings[index]);
  VTFPrevForm.Show;
  Show;
end;

procedure TMain.GameBoxChangeStatus;
begin
  ExistingSprayList.Clear;

  GamePathSelBtn.Enabled := (GameComboBox.ItemIndex >= 0);
  ClearGamePathBtn.Enabled := GamePathSelBtn.Enabled;

  if InfoHandler.GetGamePath(GameComboBox.ItemIndex) = '' then begin
    GameStatusLbl.Caption := 'Game is not installed, or you need to select its path.';
    GameStatusLbl.Font.Color := $00FFFFFF;
    GameStatusLbl.Color := $000000FF;
  end else begin
    GameStatusLbl.Caption := 'Game is installed. Its currently-imported sprays are listed on the right.';
    GameStatusLbl.Font.Color := clWindowText;
    GameStatusLbl.Color := clBtnFace;
    ExistingSpraysListPopulate(InfoHandler.GetMainSprayPath(GameComboBox.ItemIndex));
    ImportAllBtn.Hint := Format('Import all Sprays To Import into %s',[InfoHandler.GetGameShortName(GameComboBox.ItemIndex)]);
    ImportSelBtn.Hint := Format('Import selected sprays into %s',[InfoHandler.GetGameShortName(GameComboBox.ItemIndex)]);
  end;

  DeleteBtnDisablerEnabler;
  ImportBtnsDisablerEnabler;
end;

procedure TMain.ExistingSprayListClick(Sender: TObject);
begin
  DeleteBtnDisablerEnabler;
end;

procedure TMain.ExistingSprayListDblClick(Sender: TObject);
begin
  PreviewSpray(ExistingSprayList.ItemIndex);
end;

procedure TMain.ExistingSprayListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ListKeyDown(Sender,Key,Shift);
  if (Key = $0D {Enter}) and (not ( ssCtrl in Shift)) and (not (ssShift in Shift))
    and (not (ssAlt in Shift)) then
    PreviewSpray(ExistingSprayList.ItemIndex);
end;

procedure TMain.ExistingSprayListKeyPress(Sender: TObject; var Key: Char);
begin
  DeleteBtnDisablerEnabler
end;

procedure TMain.ExistingSpraysListPopulate(const path: string);
var
  VTFFiles: TSearchRec;
begin
  if DirectoryExists(path) then begin
    if FindFirst(Path+'*.vtf', faAnyFile, VTFFiles) = 0 then
      repeat
        ExistingSprayList.Items.Add(ExtractFileName(VTFFiles.Name));
      until FindNext(VTFFiles) <> 0;
    FindClose(VTFFiles);
  end;
end;

procedure TMain.GameComboBoxChange(Sender: TObject);
begin
  GameBoxChangeStatus;
end;

procedure TMain.GamePathSelBtnClick(Sender: TObject);
begin
  SelectGameDirectory(GameComboBox.ItemIndex);
end;

procedure TMain.DeleteBtnDisablerEnabler;
begin
  if ExistingSprayList.ItemIndex >= 0 then
    DeleteSprayBtn.Enabled := true
  else
    DeleteSprayBtn.Enabled := false;
end;

procedure TMain.ImportBtnsDisablerEnabler;
begin
  ImportAllBtn.Enabled := (ImportSpraysList.Count > 0) and (GameComboBox.ItemIndex >= 0)
    and (InfoHandler.GetGamePath(GameComboBox.ItemIndex) <> '');
  ImportSelBtn.Enabled := (ImportAllBtn.Enabled) and (ImportSpraysList.SelCount > 0);
end;

procedure TMain.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = $41 {A}) and (ssCtrl in Shift) and (not (ssShift in Shift))
    and (not (ssAlt in Shift)) then
    TListBox(Sender).SelectAll;
end;

procedure TMain.DeleteSpray;
var
  i: NativeInt;
begin
  if MessageBox(Handle,'Do you wish to delete the selected sprays from this game?','Spray Deletion',
    MB_YESNO or MB_ICONQUESTION) = IDYES then begin
    for i := 0 to ExistingSprayList.Count - 1 do
      if ExistingSprayList.Selected[i] then
        InfoHandler.DeleteFullSpray(ChangeFileExt(ExistingSprayList.Items.Strings[i],''),
          GameComboBox.ItemIndex);
    ExistingSprayList.DeleteSelected;
  end;
end;

procedure TMain.DeleteSprayBtnClick(Sender: TObject);
begin
  DeleteSpray;
end;

procedure TMain.AddSpraysBtnClick(Sender: TObject);
var
  i: NativeInt;
begin
  if SprayImportDlg.Execute then
    for i := 0 to SprayImportDlg.Files.Count - 1 do
      AddSprayToImportList(SprayImportDlg.Files.Strings[i]);
end;

procedure TMain.AddSprayToImportList(const FileName: string);
begin
  if FileExists(FileName) and VTFValidate(FileName) and (ImportSpraysList.Items.IndexOf(FileName) = -1) then
    ImportSpraysList.Items.Add(FileName);

  ImportBtnsDisablerEnabler;
end;

procedure TMain.AboutBtnClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TMain.RemoveSpraysBtnClick(Sender: TObject);
begin
  RemoveSpraysFromImportList;
end;

procedure TMain.RemoveSpraysFromImportList;
begin
  ImportSpraysList.DeleteSelected;
  ImportBtnsDisablerEnabler;
end;

function TMain.VTFValidate(const FileName: string): Boolean;
var
  VTFFile: File;
  TestVar: Cardinal;
begin
  FileMode := fmOpenRead;
  AssignFile(VTFFile,FileName);
  Reset(VTFFile,4);

  if not eof(VTFFile) then
    BlockRead(VTFFile,TestVar,1)
  else
    TestVar := 0;

  CloseFile(VTFFile);

  Result := (TestVar = VTF_MAGIC);
end;

procedure TMain.WMDROPFILES(var msg: TWMDropFiles) ;
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

    AddSprayToImportList(fileName);
  end;

  //release memory
  DragFinish(msg.Drop);
end;

function TMain.ImportSpray(index: NativeInt; var ErrorStruct: TSprayErrorTypeCount): NativeInt;
begin
  if StatusFrm = nil then
    StatusFrm := TStatusFrm.Create(Self);

  Result := InfoHandler.ExportFullSpray(ImportSpraysList.Items.Strings[index],GameComboBox.ItemIndex);
  if Result = SFH_EC_SUCCESS then begin
    Inc(ErrorStruct.Successes);
    StatusFrm.StatusMemo.Lines.Append(Format('%s: import successful!',[ImportSpraysList.Items.Strings[index]]));
  end else if Result = SFH_EC_GAME_NOT_INSTALLED then begin
    Inc(ErrorStruct.GameNotInstalled);
    StatusFrm.StatusMemo.Lines.Append(Format('(!) %s: game not installed',[ImportSpraysList.Items.Strings[index]]));
  end else if Result = SFH_EC_SPRAY_TOO_LARGE then begin
    Inc(ErrorStruct.SprayTooLarge);
    StatusFrm.StatusMemo.Lines.Append(Format('(!) %s: spray too large',[ImportSpraysList.Items.Strings[index]]));
  end else if Result = SFH_EC_INVALID_VTF_FILE then begin
    Inc(ErrorStruct.InvalidVTFFile);
    StatusFrm.StatusMemo.Lines.Append(Format('(!) %s: invalid VTF file',[ImportSpraysList.Items.Strings[index]]));
  end else if Result = SFH_EC_IO_ERROR then begin
    Inc(ErrorStruct.IOError);
    StatusFrm.StatusMemo.Lines.Append(Format('(!) %s: I/O error',[ImportSpraysList.Items.Strings[index]]));
  end;
end;

procedure TMain.ImportSpraysListClick(Sender: TObject);
begin
  ImportBtnsDisablerEnabler;
end;

procedure TMain.ImportSpraysListKeyPress(Sender: TObject; var Key: Char);
begin
  ImportBtnsDisablerEnabler;
end;

procedure TMain.ImportSelBtnClick(Sender: TObject);
begin
  ImportSelectedSprays;
end;

procedure TMain.ImportSelectedSprays;
var
  Errors: TSprayErrorTypeCount;
  i: NativeInt;
begin
  // initialize error counts
  Errors.Successes := 0;
  Errors.GameNotInstalled := 0;
  Errors.SprayTooLarge := 0;
  Errors.InvalidVTFFile := 0;
  Errors.IOError := 0;

  // clear status window if it exists
  if StatusFrm <> nil then
    StatusFrm.StatusMemo.Clear;

  // Go!
  for i := 0 to ImportSpraysList.Count - 1 do begin
    if ImportSpraysList.Selected[i] then begin
      ImportSpray(i,Errors);
    end;
  end;

  // Update status label
  SprayStatusUpdate(Errors);
end;

procedure TMain.ImportAllBtnClick(Sender: TObject);
begin
  ImportAllSprays;
end;

procedure TMain.ImportAllSprays;
var
  Errors: TSprayErrorTypeCount;
  i: NativeInt;
begin
  // initialize error counts
  Errors.Successes := 0;
  Errors.GameNotInstalled := 0;
  Errors.SprayTooLarge := 0;
  Errors.InvalidVTFFile := 0;
  Errors.IOError := 0;

  // clear status window if it exists
  if StatusFrm <> nil then
    StatusFrm.StatusMemo.Clear;

  // Go!
  for i := 0 to ImportSpraysList.Count - 1 do begin
    ImportSpray(i,Errors);
  end;

  // Update status label
  SprayStatusUpdate(Errors);
end;

procedure TMain.SprayStatusUpdate(const ErrorStruct: TSprayErrorTypeCount);
begin
  if ErrorStruct.Successes = 0 then begin
    GameStatusLbl.Color := $000000FF;
    GameStatusLbl.Font.Color := $00FFFFFF;
    GameStatusLbl.Caption := 'No successful spray imports. Details in status window.';
    if StatusFrm <> nil then
      StatusFrm.Show;
  end else if (ErrorStruct.GameNotInstalled > 0) or
    (ErrorStruct.SprayTooLarge > 0) or (ErrorStruct.InvalidVTFFile > 0) or
    (ErrorStruct.IOError > 0) then begin
    GameStatusLbl.Color := $0000FFFF;
    GameStatusLbl.Font.Color := $00000000;
    GameStatusLbl.Caption := 'Some sprays did not import properly. Details in status window.';
    ExistingSprayList.Clear;
    ExistingSpraysListPopulate(InfoHandler.GetMainSprayPath(GameComboBox.ItemIndex));
    if StatusFrm <> nil then
      StatusFrm.Show;
  end else begin
    GameStatusLbl.Color := $0000FF00;
    GameStatusLbl.Font.Color := $00000000;
    GameStatusLbl.Caption := 'All sprays were imported successfully!';
    FreeAndNil(StatusFrm);
    ExistingSprayList.Clear;
    ExistingSpraysListPopulate(InfoHandler.GetMainSprayPath(GameComboBox.ItemIndex));
  end;
end;

function TMain.SelectGameDirectory(index: NativeInt): Boolean;
var
  DSCaption,Dir: string;
  HintFrm: TGameFolderHintFrm;
begin
  HintFrm := TGameFolderHintFrm.Create(Self);
  HintFrm.Show;
  DSCaption := Format('Game Directory for %s',[InfoHandler.GetGameShortName(index)]);

  if SelectDirectory(DSCaption,'',Dir,[],Self) then begin
    Dir := IncludeTrailingPathDelimiter(Dir);
    Result := InfoHandler.SetGamePath(index,Dir);
    if Result then begin
      GameBoxChangeStatus;
      GameStatusLbl.Caption := Format('Game install path changed to %s',[Dir]);
      GameStatusLbl.Color := $00FF0000;
      GameStatusLbl.Font.Color := $00FFFFFF;
    end else begin
      GameStatusLbl.Caption := Format('Invalid directory. Game install path not changed.',[Dir]);
      GameStatusLbl.Color := $00800000;
      GameStatusLbl.Font.Color := $00FFFFFF;
    end;
  end else begin
    Result := false;
  end;

  HintFrm.Free;
end;

procedure TMain.ClearGameDirectory(index: NativeInt);
begin
  InfoHandler.ClearGamePath(index);
  GameBoxChangeStatus;
  GameStatusLbl.Caption := 'Game install path information cleared from application. Select a different path or restart application to rescan.';
  GameStatusLbl.Color := $00808080;
  GameStatusLbl.Font.Color := $00FFFFFF;
end;

procedure TMain.ClearGamePathBtnClick(Sender: TObject);
begin
  ClearGameDirectory(GamecomboBox.ItemIndex);
end;

procedure TMain.ShowAboutForm;
var
  AboutForm: TAboutFrm;
begin
  AboutForm := TAboutFrm.Create(Self);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

end.
