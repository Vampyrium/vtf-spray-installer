program updpack;

{$R 'update.res' 'update\update.rc'}

uses
  Vcl.Forms,
  UpdMainU in 'UpdMainU.pas' {Form2},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vampyrium UpdatePak - Spray Installer';
  TStyleManager.TrySetStyle('Ruby Graphite');
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
