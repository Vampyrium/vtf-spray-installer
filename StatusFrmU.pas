unit StatusFrmU;

interface

uses
  {Winapi.Windows,} {Winapi.Messages,} {System.SysUtils,} {System.Variants,} System.Classes, {Vcl.Graphics,}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TStatusFrm = class(TForm)
    StatusMemo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TStatusFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

end.
