unit AboutFrmU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Winapi.Shellapi;

type
  TAboutFrm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    VersionLbl: TLabel;
    LinkLbl: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure LinkLblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.dfm}

procedure TAboutFrm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutFrm.LinkLblClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(LinkLbl.Caption), '', '', SW_SHOWNORMAL);
end;

end.
