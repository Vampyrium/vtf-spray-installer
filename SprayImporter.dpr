program SprayImporter;

{$R 'gamedata.res' 'gamedata\gamedata.rc'}

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Main},
  SprayFileHandler in 'SprayFileHandler.pas',
  SprayFileHandlerClasses in 'SprayFileHandlerClasses.pas',
  SprayFileHandlerExceptions in 'SprayFileHandlerExceptions.pas',
  Vcl.Themes,
  Vcl.Styles,
  StatusFrmU in 'StatusFrmU.pas' {StatusFrm},
  GameFolderHintFrmU in 'GameFolderHintFrmU.pas' {GameFolderHintFrm},
  AboutFrmU in 'AboutFrmU.pas' {AboutFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
