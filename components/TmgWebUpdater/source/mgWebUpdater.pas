 {*------------------------------------------------------------------------------
   This component allow to imnplement auto-update in your application.
   It requires Indy components installed in the IDE.
   It is provided as it is, for free and without any warranty.
   You use it at your own risk.
 -------------------------------------------------------------------------------}  

unit mgWebUpdater;

interface
//link for TmxStorage http://www.torry.net/authorsmore.php?id=3079
uses
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, IdCookieManager, StrUtils, Windows, Forms, ShellAPI,
  Controls, ExtCtrls, uUpdaterForm, Dialogs, jpeg, Graphics, buildinf;

type
  {Update mode}
  TUpdateMode = (umAuto, umManual);

  {*------------------------------------------------------------------------------
  TmgDialogOptions is the class which let you customize the Dialog appearing to
  the user while checking and downloading updates: here you can set several pro-
  perties to integrate this visual element of TmgWebUpdater within your application.
  You can set window's color, Font's properties (you can set three labels' fonts
  individually) and you can also eventually add a background image.
  -------------------------------------------------------------------------------}
  TmgDialogOptions = class(TPersistent)
  private
    FPicture: TFileName;
    FDialogColor: TColor;
    FlblProgressFont: TFont;
    FlblTitleFont: TFont;
    FlblCheckingFont: TFont;
  protected
    procedure SetPicture(Value: TFileName); virtual;
    procedure SetColor(Value: TColor); virtual;
    procedure SetlblProgressFont(Value: TFont); virtual;
    procedure SetlblTitleFont(Value: TFont); virtual;
    procedure SetlblCheckingFont(Value: TFont); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Picture: TFileName read FPicture write SetPicture;
    property DialogColor: TColor read FDialogColor write SetColor default clBtnFace;
    property ProgressLabelFont: TFont read FlblProgressFont write SetlblProgressFont;
    property TitleLabelFont: TFont read FlblTitleFont write SetlblTitleFont;
    property CheckingLabelFont: TFont read FlblCheckingFont write SetlblCheckingFont;
  end;

  {*------------------------------------------------------------------------------
    TmgWebUpdater makes easy as never before implementing automatic web update
    feature within your application. To use it, you MUST enable Delphi versioning
    system for project you want to be able to auto-update (Project->Options->
    Version Info: be sure 'Include version information in project' checkbox is checked).
    TmgWebUpdater will automatically retrieve the version number of your application.
    Then, using TidHttp component (which is part of the Indy suite,
    http://www.indyproject.org/download/Files/Indy9.html), it will connect
    to a file hosted in the specified web server (this file can be created calling
    CreatePhpScript procedure): this php file contains the new version number of
    your app. When TmgWebUpdater contact the server it comunicates the actual
    version number to the php script which will compare it to the new version
    number and depending on the result of this comparation it will respond to
    TmgWebUpdater if an update is available. If this is the case, TmgWebUpdater
    will downloads updates displaying a dialog to inform user about download progess.
    When download has finished, application will be treminated and the downloaded
    file will be executed.
    The update file must then be an executable file which, once executed, will
    install updates: it can be a setup program or another type of program able to
    install new files in the right directory.

    In order to make it running you have to:
     - create the program which will be downloaded and which will install updates
     - upload this file to your web server
     - upload to web server the file checkUpdates.php (you can rename it, modify it
       or leave it as it is: the only thing you must leave untouched is the value
       of the variable $newVersion
     - Set DownloadUrl and CheckUrl properties with your data:
       CheckUrl: http://www.yourserver.com/yourfolderupdates/checkUpdates.php
       DownloadUrl: http://www.yourserver.com/yourfolderupdates/updateexe.upd
     - Set property Active to true
    That's all: everytime your application will start, it will check if there are
    updates available and i will download them.
  -------------------------------------------------------------------------------}
  TmgWebUpdater = class(TComponent)
  private
    FUpdateMode: TUpdateMode;
    FIdHttp: TIdHTTP;
    FIdCookie: TIdCookieManager;
    FActive: Boolean;
    FDownloadUrl: string;
    FCheckUrl: string;
    FActualVersionNumber: string;
    FInternalActualVersionNumber: string;
    FRunning: Boolean;
    FDialogOptions: TmgDialogOptions;
  protected
    procedure SetActive(Value: boolean); virtual;
    procedure SetDownloadUrl(Value: string); virtual;
    procedure SetCheckUrl(Value: string); virtual;
    procedure SetUpdateMode(Value: TUpdateMode); virtual;
    procedure HttpWork1(ASender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure HttpWorkBegin1(ASender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
    procedure HttpWork3(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Integer);
    procedure HttpWorkBegin3(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Integer);
    procedure HttpWork2(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure HttpWorkBegin2(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    {*------------------------------------------------------------------------------
      Following two procedure are internal and they are called automatically by
      TmgWebUpdater if UpdateMode is set to umAuto
    -------------------------------------------------------------------------------}
    procedure InternalCheckForUpdates;
    procedure InternalDownloadUpdates;
  public
    {*------------------------------------------------------------------------------
      If UpdateMode is set to umManual or if you want for instance place a
      button 'Check for updates now!' in your application, you can use CheckForUpdates
      function and DownloadUpdates procedure this way:
         if CheckForUpdates then DownloadUpdates;
    -------------------------------------------------------------------------------}
    function CheckForUpdates: Boolean;
    procedure DownloadUpdates;
    {*------------------------------------------------------------------------------
      Call this function at runtime to create the Php script to upload to your
      web server. Personally I suggest to place a button somewhere in your form
      to call this procedure and then set its Visible property to False to distribute
      your application.
    -------------------------------------------------------------------------------}  
    procedure CreateScript(Lang: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {*------------------------------------------------------------------------------
      You can use ActualVersionNumber to see... the actual version number of your
      application and publish it in the About box
    -------------------------------------------------------------------------------}
    property ActualVersionNumber: string read FActualVersionNumber;
    {*------------------------------------------------------------------------------
      If True, Active makes TmgWebUpdater check for updates and download them,
      obviousy if UpodateMode is set to umAuto. If UpdateMode is set to umManual,
      Active doesn't take effect.
    -------------------------------------------------------------------------------}
    property Active: boolean read FActive write SetActive default False;
  published
    {*------------------------------------------------------------------------------
      Must contain, protocol, domain, directories (if any) and the file name to look
      for downloading
    -------------------------------------------------------------------------------}
    property DownloadUrl: string read FDownloadUrl write SetDownloadUrl;
    {*------------------------------------------------------------------------------
      Must contain, protocol, domain, directories (if any) and the file name to look
      for checking version: for instance, checkUpdates.php
    -------------------------------------------------------------------------------}  
    property CheckUrl: string read FCheckUrl write SetCheckUrl;
    {*------------------------------------------------------------------------------
      If UpdateMode is set to umAuto, TmgWebUpdate will check for updates when it
      will be created. If you want check for updates only in a second moment you
      have to set UpdateMode to umManual and call right procedures manually
    -------------------------------------------------------------------------------}  
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode;
    {*------------------------------------------------------------------------------
      DialogOptions provide an easy way to set both at design-time and at run-time
      the look'n'feel of dialog TmgWebUpdater shows to user to inform him about
      checking and downloading progress.
    -------------------------------------------------------------------------------}  
    property DialogOptions: TmgDialogOptions read FDialogOptions write FDialogOptions;
  end;


implementation

{ TmgWebUpdater }

procedure TmgWebUpdater.InternalCheckForUpdates;
var
  FResponse, FVersion: string;
begin
  if FRunning then
  begin
    frmUpdater := TfrmUpdater.Create(Application);
    try
      frmUpdater.lblChecking.Visible := True;
      frmUpdater.lblChecking.Font.Assign(FDialogOptions.FlblCheckingFont);
      frmUpdater.Color := FDialogOptions.FDialogColor;
      if FDialogOptions.FPicture <> '' then
      begin
        frmUpdater.img1.Picture.LoadFromFile(FDialogOptions.FPicture);
        frmUpdater.img1.Visible := True;
      end
      else
      begin
        frmUpdater.img1.Picture.Assign(nil);
        frmUpdater.img1.Visible := False;
      end;
      frmUpdater.Show;
      frmUpdater.Update;
      FVersion := AnsiReplaceText(FInternalActualVersionNumber, '.', '');
      try
        FResponse := FIdHttp.Get(FCheckUrl+'?ver=' + FVersion);
      except
        MessageBox(0,
          'Cannot contact the server to check for updates. Please make sure you are connected to the Internet and try again.',
        'Vampyrium Updater', MB_OK + MB_ICONSTOP);
      end;
      frmUpdater.lblChecking.Visible := False;
      if AnsiContainsText(FResponse, 'Outdated') then
        InternalDownloadUpdates
      else
        FActive := False;
    finally
      FreeAndNil(frmUpdater);
    end;
  end;
end;

procedure TmgWebUpdater.InternalDownloadUpdates;
var
  FName: string;
  fs: TFileStream;
  Handle: THandle;
begin
  frmUpdater := TfrmUpdater.Create(Application);
  try
    frmUpdater.lblProgress.Visible := True;
    frmUpdater.lblTitle.Visible := True;
    frmUpdater.prb1.Visible := True;
    frmUpdater.lblProgress.Font.Assign(FDialogOptions.FlblProgressFont);
    frmUpdater.lblTitle.Font.Assign(FDialogOptions.FlblTitleFont);
    frmUpdater.Color := FDialogOptions.FDialogColor;
    if FDialogOptions.FPicture <> '' then
    begin
      frmUpdater.img1.Picture.LoadFromFile(FDialogOptions.FPicture);
      frmUpdater.img1.Visible := True;
    end
    else
    begin
      frmUpdater.img1.Picture.Assign(nil);
      frmUpdater.img1.Visible := False;
    end;
    frmUpdater.Show;
    frmUpdater.Update;
    FName := ExtractFilePath(Application.ExeName) + 'vampyrium_vtfsi_updpack.exe';
    fs := TFileStream.Create(FName, fmCreate);
    try
      try
        FIdHttp.Get(FDownloadUrl, fs);
        MessageBox(0, 'All files have been downloaded successfully.', 'Vampyrium Updater',
          MB_OK + MB_ICONINFORMATION);
      except
        MessageBox(0, 'An update was found, but it cannot be downloaded. Please make sure you are connected to the Internet and try again.', 'Vampyrium Updater', MB_OK + MB_ICONSTOP);
        Exit;
      end;
    finally
      fs.free;
    end;
    Handle := Application.Handle;
    if FileExists(FName) then
    begin
      ShellExecute(Handle, 'open', PChar(FName), nil, nil, SW_SHOWNORMAL);
      frmUpdater.lblProgress.Visible := False;
      frmUpdater.lblTitle.Visible := False;
      frmUpdater.prb1.Visible := False;
      FActive := False;
      Application.Terminate;
    end
  finally
    frmUpdater.lblProgress.Visible := False;
    frmUpdater.lblTitle.Visible := False;
    frmUpdater.prb1.Visible := False;
    FActive := False;
    FreeAndNil(frmUpdater);
  end;
end;

function TmgWebUpdater.CheckForUpdates: Boolean;
var
  FResponse, FVersion: string;
begin
  Result := False;
  if FUpdateMode = umManual then
  begin
    if FRunning then
    begin
      frmUpdater := TfrmUpdater.Create(Application);
      try
        frmUpdater.lblChecking.Visible := True;
        frmUpdater.lblChecking.Font.Assign(FDialogOptions.FlblCheckingFont);
        frmUpdater.Color := FDialogOptions.FDialogColor;
        if FDialogOptions.FPicture <> '' then
        begin
          frmUpdater.img1.Picture.LoadFromFile(FDialogOptions.FPicture);
          frmUpdater.img1.Visible := True;
        end
        else
        begin
          frmUpdater.img1.Picture.Assign(nil);
          frmUpdater.img1.Visible := False;
        end;
        frmUpdater.Show;
        frmUpdater.Update;
        FVersion := AnsiReplaceText(FInternalActualVersionNumber, '.', '');
        try
          FResponse := FIdHttp.Get(FCheckUrl+'?ver=' + FVersion);
        except
          MessageBox(0,
            'Cannot contact the server to check for updates. Please make sure you are connected to the Internet and try again.',
          'Vampyrium Updater', MB_OK + MB_ICONSTOP);
        end;
        frmUpdater.lblChecking.Visible := False;
        if AnsiContainsText(FResponse, 'Outdated') then
          Result := True
        else
          FActive := False;
      finally
        FreeAndNil(frmUpdater);
      end;
    end;
  end;
end;

procedure TmgWebUpdater.DownloadUpdates;
var
  FName: string;
  fs: TFileStream;
  Handle: THandle;
begin
  if FUpdateMode = umManual then
  begin
    frmUpdater := TfrmUpdater.Create(Application);
    try
      frmUpdater.lblProgress.Visible := True;
      frmUpdater.lblTitle.Visible := True;
      frmUpdater.prb1.Visible := True;
      frmUpdater.lblProgress.Font.Assign(FDialogOptions.FlblProgressFont);
      frmUpdater.lblTitle.Font.Assign(FDialogOptions.FlblTitleFont);
      frmUpdater.Color := FDialogOptions.FDialogColor;
      if FDialogOptions.FPicture <> '' then
      begin
        frmUpdater.img1.Picture.LoadFromFile(FDialogOptions.FPicture);
        frmUpdater.img1.Visible := True;
      end
      else
      begin
        frmUpdater.img1.Picture.Assign(nil);
        frmUpdater.img1.Visible := False;
      end;
      frmUpdater.Show;
      frmUpdater.Update;
      FName := ExtractFilePath(Application.ExeName) + 'vampyrium_vtfsi_updpack.exe';
      fs := TFileStream.Create(FName, fmCreate);
      try
        try
          FIdHttp.Get(FDownloadUrl, fs);
          MessageBox(0, 'All files have been downloaded successfully. The update will be performed after pressing OK.', 'Vampyrium Updater',
            MB_OK + MB_ICONINFORMATION);
        except
          MessageBox(0, 'An update was found, but it cannot be downloaded. Please make sure you are connected to the Internet and try again.', 'Vampyrium Updater', MB_OK + MB_ICONSTOP);
          Exit;
        end;
      finally
        fs.free;
      end;
      Handle := Application.Handle;
      if FileExists(FName) then
      begin
        ShellExecute(Handle, 'open', PChar(FName), nil, nil, SW_SHOWNORMAL);
        frmUpdater.lblProgress.Visible := False;
        frmUpdater.lblTitle.Visible := False;
        frmUpdater.prb1.Visible := False;
        FActive := False;
        Application.Terminate;
      end
    finally
      frmUpdater.lblProgress.Visible := False;
      frmUpdater.lblTitle.Visible := False;
      frmUpdater.prb1.Visible := False;
      FActive := False;
      FreeAndNil(frmUpdater);
    end;
  end;
end;

constructor TmgWebUpdater.Create(AOwner: TComponent);
var
  Info: string;
begin
  inherited;
  DialogOptions := TmgDialogOptions.Create;
  FRunning := False;
  if not (csDesigning in AOwner.ComponentState) then
  begin
    Info := GetBuildInfoString(Application.ExeName);
    FActualVersionNumber := Info;
    Info := AnsiReplaceText(Info,'(', '');
    Info := AnsiReplaceText(Info,')', '');
    Info := AnsiReplaceText(Info,'.', '');
    Info := AnsiReplaceText(Info,'build', '');
    Info := AnsiReplaceText(Info,' ', '');
    FInternalActualVersionNumber := Info;
    FIdCookie := TIdCookieManager.Create(Self);
    FIdCookie.Name := 'InternalCookieMgr';
    FIdHttp := TIdHTTP.Create(Self);
    with FIdHttp do
    begin
      Name := 'InternalHttp';
      CookieManager := TIdCookieManager(FindComponent('InternalCookieMgr'));
      Request.UserAgent := 'Mozilla/3.0';
      ConnectTimeout := 10000;
    {$IFDEF VER150}
      OnWork := HttpWork1;
      OnWorkBegin := HttpWorkBegin1;
    {$ENDIF}
    {$IFDEF VER160}
      OnWork := HttpWork3;
      OnWorkBegin := HttpWorkBegin3;
    {$ENDIF}
    {$IFDEF VER170}
      OnWork := HttpWork1;
      OnWorkBegin := HttpWorkBegin1;
    {$ENDIF}
    {$IFDEF VER180}
      OnWork := HttpWork1;
      OnWorkBegin := HttpWorkBegin1;
    {$ENDIF}
    {$IFDEF VER185}
      OnWork := HttpWork1;
      OnWorkBegin := HttpWorkBegin1;
    {$ENDIF}
	 {$IFDEF VER200}
      OnWork := HttpWork2;
      OnWorkBegin := HttpWorkBegin2;
    {$ENDIF}
	 {$IFDEF VER210}
      OnWork := HttpWork2;
      OnWorkBegin := HttpWorkBegin2;
    {$ENDIF}
	 {$IFDEF VER220}
      OnWork := HttpWork2;
      OnWorkBegin := HttpWorkBegin2;
    {$ENDIF}
	 {$IFDEF VER230}
      OnWork := HttpWork2;
      OnWorkBegin := HttpWorkBegin2;
    {$ENDIF}
    end;
    FRunning := True;
  end;
end;

destructor TmgWebUpdater.Destroy;
begin
  if Assigned(FIdCookie) then
    FreeAndNil(FIdCookie);
  if Assigned(FIdHttp) then
    FreeAndNil(FIdHttp);
  FDialogOptions.Free;
  inherited;
end;

procedure TmgWebUpdater.HttpWork1(ASender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  if Assigned(frmUpdater) then
  begin
    frmUpdater.prb1.Position := AWorkCount;
    frmUpdater.prb1.Update;
    frmUpdater.lblProgress.Caption := IntToStr(AWorkCOunt)+' of '+ IntToStr(frmUpdater.prb1.Max)+' bytes';
    frmUpdater.lblProgress.Update;
  end;
end;

procedure TmgWebUpdater.HttpWorkBegin1(ASender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  if Assigned(frmUpdater) then
    frmUpdater.prb1.Max := AWorkCountMax;
end;

procedure TmgWebUpdater.HttpWork2(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if Assigned(frmUpdater) then
  begin
    frmUpdater.prb1.Position := AWorkCount;
    frmUpdater.prb1.Update;
    frmUpdater.lblProgress.Caption := IntToStr(AWorkCOunt)+' of '+ IntToStr(frmUpdater.prb1.Max)+' bytes';
    frmUpdater.lblProgress.Update;
  end;
end;

procedure TmgWebUpdater.HttpWorkBegin2(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  if Assigned(frmUpdater) then
    frmUpdater.prb1.Max := AWorkCountMax;
end;

procedure TmgWebUpdater.HttpWork3(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Integer);
begin
  if Assigned(frmUpdater) then
  begin
    frmUpdater.prb1.Position := AWorkCount;
    frmUpdater.prb1.Update;
    frmUpdater.lblProgress.Caption := IntToStr(AWorkCOunt)+' of '+ IntToStr(frmUpdater.prb1.Max)+' bytes';
    frmUpdater.lblProgress.Update;
  end;
end;

procedure TmgWebUpdater.HttpWorkBegin3(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Integer);
begin
  if Assigned(frmUpdater) then
    frmUpdater.prb1.Max := AWorkCountMax;
end;

procedure TmgWebUpdater.SetActive(Value: boolean);
begin
  if FRunning then
  begin
    if (FCheckUrl <> '') and (FDownloadUrl <> '') then
    begin
      FActive := Value;
      if FActive and (FUpdateMode = umAuto) then
        InternalCheckForUpdates;
    end;
  end
  else
  begin
    if FCheckUrl = '' then
      MessageBox(0, 'CheckUrl property cannot be empty.', 'WebUpdater', MB_OK +
      MB_ICONSTOP)
    else if FDownloadUrl = '' then
      MessageBox(0, 'DownloadUrl property cannot be empty.', 'WebUpdater', MB_OK +
        MB_ICONSTOP);
  end;
end;

procedure TmgWebUpdater.SetDownloadUrl(Value: string);
begin
  if not AnsiEndsText('.upd', Value) then
    MessageBox(0,
      'CheckUrl must end with a file name having ''.upd'' extension.',
      'mgWebUpdater', MB_OK + MB_ICONSTOP)
  else
    FDownloadUrl := Value;
end;

procedure TmgWebUpdater.SetCheckUrl(Value: string);
begin
  if not AnsiEndsText('.php', Value) then
    MessageBox(0,
      'CheckUrl must end with a file name having ''.php'' extension.',
      'mgWebUpdater', MB_OK + MB_ICONSTOP)
  else
    FCheckUrl := Value;
end;

procedure TmgWebUpdater.SetUpdateMode(Value: TUpdateMode);
begin
  FUpdateMode := Value;
end;

procedure TmgWebUpdater.CreateScript(Lang: string);
var
  Script: TStringList;
  Msg, SName: string;
begin
  Script := TStringList.Create;
  try
    try
      if Lang = 'PHP' then
      begin
        with Script do
        begin
          Add('<?php');
          Add('$newVersion = "' + FInternalActualVersionNumber + '";');
          Add('$actualVersion = $_GET["ver"];');
          Add('if ($actualVersion < $newVersion || $actualVersion == "") {');
          Add('    echo "Outdated";');
          Add('}else{');
          Add('    echo "Updated";');
          Add('}');
          Add('?>');
          SaveToFile(ExtractFilePath(Application.ExeName)+'checkUpdates.php');
          SName := 'checkUpdates.php';
        end;
      end
      else if Lang = 'ASP' then
      begin
        with Script do
        begin
          Add('<%');
          Add('Dim newVersion, actualVersion');
          Add('newVersion = "' + FInternalActualVersionNumber + '";');
          Add('actualVersion = Request.QueryString("ver")');
          Add('if (actualVersion < newVersion OR actualVersion = "") then');
          Add('    Response.Write "Outdated"');
          Add('else');
          Add('    Response.Write "Updated"');
          Add('end if');
          Add('%>');
          SaveToFile(ExtractFilePath(Application.ExeName)+'checkUpdates.asp');
          SName := 'checkUpdates.asp';
        end;
      end;
    except
      MessageBox(0, 'Something has gone wrong creating Php script.' + #13#10 +
        'No file has been created.', 'Application.Title', MB_OK + MB_ICONSTOP);
      Script.Free;
    end;
  finally
    Msg := 'The script ' + SName + ' has been correcly created in ' + ExtractFilePath(Application.ExeName);
    MessageBox(0, PChar(Msg), 'Application.Title', MB_OK + MB_ICONINFORMATION);
    Script.Free;
  end;
end;

{ TmgDialogOptions }

procedure TmgDialogOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

constructor TmgDialogOptions.Create;
begin
  FDialogColor := clBtnFace;
  FlblProgressFont := TFont.Create;
  FlblTitleFont := TFont.Create;
  FlblCheckingFont := TFont.Create;
  inherited Create;
end;

destructor TmgDialogOptions.Destroy;
begin
  FreeAndNil(FlblProgressFont);
  FreeAndNil(FlblTitleFont);
  FreeAndNil(FlblCheckingFont);
  inherited;
end;

procedure TmgDialogOptions.SetColor(Value: TColor);
begin
  FDialogColor := Value;
end;

procedure TmgDialogOptions.SetlblCheckingFont(Value: TFont);
begin
  FlblCheckingFont.Assign(Value);
end;

procedure TmgDialogOptions.SetlblProgressFont(Value: TFont);
begin
  FlblProgressFont.Assign(Value);
end;

procedure TmgDialogOptions.SetlblTitleFont(Value: TFont);
begin
  FlblTitleFont.Assign(Value);
end;

procedure TmgDialogOptions.SetPicture(Value: TFileName);
begin
  FPicture := Value;
end;

end.
