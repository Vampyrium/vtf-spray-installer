unit AboutFrmU;

interface

uses
  Winapi.Windows, {Winapi.Messages,} System.SysUtils, {System.Variants,} System.Classes, {Vcl.Graphics,}
  Vcl.Controls, Vcl.Forms, {Vcl.Dialogs,} Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Winapi.Shellapi;

type
  TAboutFrm = class(TForm)
    AboutBkgImg: TImage;
    VersionLbl: TLabel;
    CopyrightLbl: TLabel;
    LinkLbl: TLabel;
    AboutOKBtn: TButton;
    procedure LinkLblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AboutOKBtnClick(Sender: TObject);
  private
    function Sto_GetFmtFileVersion(const FileName: String = '';
      const Fmt: String = '%d.%d.%d.%d'): String;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

// From http://www.martinstoeckli.ch/delphi/delphi.html#AppVersion
function TAboutFrm.Sto_GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;
var
  sFileName: String;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  sFileName := FileName;
  if (sFileName = '') then
  begin
    // prepare buffer for path and terminating #0
    SetLength(sFileName, MAX_PATH + 1);
    SetLength(sFileName,
      GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
  end;
  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
    // get fixed file info (language independent)
    GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
    VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
    // read version blocks
    iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
    // format result string
    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;

procedure TAboutFrm.AboutOKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutFrm.FormCreate(Sender: TObject);
begin
  VersionLbl.Caption := Format(VersionLbl.Caption,[Sto_GetFmtFileVersion]);
end;

procedure TAboutFrm.LinkLblClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(LinkLbl.Caption), '', '', SW_SHOWNORMAL);
end;

end.
