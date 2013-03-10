unit AboutFrmU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Winapi.Shellapi, mgWebUpdater;

type
  TAboutFrm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    VersionLbl: TLabel;
    LinkLbl: TLabel;
    Button1: TButton;
    UpdCheckBtn: TButton;
    MainWebUpdater: TmgWebUpdater;
    procedure Button1Click(Sender: TObject);
    procedure LinkLblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdCheckBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateCheck;
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.dfm}

procedure TAboutFrm.UpdateCheck;
begin
  if MainWebUpdater.CheckForUpdates then begin
    MainWebUpdater.DownloadUpdates;
  end;
end;

procedure TAboutFrm.UpdCheckBtnClick(Sender: TObject);
begin
  UpdateCheck;
end;

procedure TAboutFrm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutFrm.FormCreate(Sender: TObject);
begin
  // Updater Component Information - CHANGE THIS!
  MainWebUpdater.CheckUrl := 'http://vampyrium.net/updCheck/SprayInstaller.php';
  MainWebUpdater.DownloadUrl := 'http://dl.dropbox.com/u/91565886/vampyrium_vtfsi_updpack.upd';
end;

procedure TAboutFrm.LinkLblClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(LinkLbl.Caption), '', '', SW_SHOWNORMAL);
end;

end.
