unit GameFolderHintFrmU;

interface

uses
  {Winapi.Windows,} {Winapi.Messages,} {System.SysUtils,} {System.Variants,} System.Classes, {Vcl.Graphics,}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TGameFolderHintFrm = class(TForm)
    HintLbl: TLabel;
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TGameFolderHintFrm.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := $00000000;
  Canvas.Pen.Color := $00000000;
  Canvas.FillRect(ClientRect);
end;

end.
