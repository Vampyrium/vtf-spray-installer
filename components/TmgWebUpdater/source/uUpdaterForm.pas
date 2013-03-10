unit uUpdaterForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmUpdater = class(TForm)
    prb1: TProgressBar;
    lblProgress: TLabel;
    img1: TImage;
    lblTitle: TLabel;
    lblChecking: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUpdater: TfrmUpdater;

implementation


{$R *.dfm}

end.
