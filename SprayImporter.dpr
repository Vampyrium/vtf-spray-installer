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
  AboutFrmU in 'AboutFrmU.pas' {AboutFrm},
  VTFLib in 'lib\VTFLib.pas',
  VTFPrevFrmU in 'VTFPrevFrmU.pas' {VTFPrevForm};

{$R *.res}

begin
  VTFLib.vlInitialize;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TVTFPrevForm, VTFPrevForm);
  Application.Run;
  VTFLib.vlShutdown;
end.
