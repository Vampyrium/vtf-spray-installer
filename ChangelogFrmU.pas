unit ChangelogFrmU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TChangelogFrm = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChangelogFrm: TChangelogFrm;
  NewerVersion: boolean = false;

implementation

{$R *.dfm}

procedure TChangelogFrm.FormCreate(Sender: TObject);
begin
  if NewerVersion then ShowModal;
end;

procedure TChangelogFrm.OKBtnClick(Sender: TObject);
begin
  Close;
end;

end.
