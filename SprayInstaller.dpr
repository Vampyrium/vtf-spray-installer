program SprayInstaller;

{$R 'gamedata.res' 'gamedata\gamedata.rc'}

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Form1},
  SprayFileHandler in 'SprayFileHandler.pas',
  Vcl.Themes,
  Vcl.Styles,
  ChangelogFrmU in 'ChangelogFrmU.pas' {ChangelogFrm},
  AboutFrmU in 'AboutFrmU.pas' {AboutFrm},
  SprayFileHandlerClasses in 'SprayFileHandlerClasses.pas',
  SprayFileHandlerExceptions in 'SprayFileHandlerExceptions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vampyrium Spray Installer';
  TStyleManager.TrySetStyle('Ruby Graphite');
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TChangelogFrm, ChangelogFrm);
  Application.CreateForm(TAboutFrm, AboutFrm);
  Application.Run;
end.
