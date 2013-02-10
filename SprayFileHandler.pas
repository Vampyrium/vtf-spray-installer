unit SprayFileHandler;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, SprayFileHandlerClasses,
  SprayFileHandlerExceptions;

const
  SFH_GAME_RSRC: string = 'GAMESINFO';
  SFH_LAYOUT_RSRC: string = 'LAYOUTSINFO';
  VTF_MAGIC: Cardinal = $00465456; //Little endian, 'VTF'#0
  // Error codes for spray exporting
  SFH_EC_SUCCESS: NativeInt = 0;
  SFH_EC_GAME_NOT_INSTALLED: NativeInt = 1;
  SFH_EC_SPRAY_TOO_LARGE: NativeInt = 2;
  SFH_EC_INVALID_VTF_FILE: NativeInt = 3;
  SFH_EC_IO_ERROR: NativeInt = 4;

type TSprayFileHandler = class
  private
    FLayouts: array of TSprayFileLayout;
    FGames: array of TSprayGameInfo;
    procedure ReadAllGameAndLayoutInfo;
    procedure DebugDumpLayoutInfo(FileName: string);
    procedure DebugDumpGameInfo(FileName: string);
  public
    constructor Create;
    destructor Destroy; override;
    function ExportFullSpray(SprayFileName, SteamappsDir, Username: string; GameIndex: NativeInt): NativeInt;
    procedure PopulateStringList(AList: TStrings);
end;

implementation

{ TSprayFileHandler }

constructor TSprayFileHandler.Create;
begin
  ReadAllGameAndLayoutInfo;
end;

destructor TSprayFileHandler.Destroy;
var
  i: NativeInt;
begin
  for i := 0 to Length(FLayouts) do
    FLayouts[i].Free;
  for i := 0 to Length(FGames) do
    FGames[i].Free;
  SetLength(FLayouts,0);
  SetLength(FGames,0);
end;

function TSprayFileHandler.ExportFullSpray(SprayFileName, SteamappsDir, Username: string; GameIndex: NativeInt): NativeInt;
var
  VTFFile,OutFile: TFileStream;
  RsrcAccess: TResourceStream;
  VMTBuffer: TStringList;
  MNBuf: Cardinal;
  i,j: NativeInt;
  SteamappsPath,GameRelPath,SprayName,PathOut: string;
begin
  try
    VMTBuffer := TStringList.Create;
    VTFFile := TFileStream.Create(SprayFileName,fmOpenRead);
    SteamappsPath := IncludeTrailingBackslash(SteamappsDir);
    GameRelPath := FGames[GameIndex].GetGameRelPath(Username);

    // check for the VTF magic number (if it's not there, the file is DEFINITELY
    // NOT a valid VTF texture!)
    VTFFile.ReadBuffer(MNBuf,4);
    if MNBuf <> VTF_MAGIC then begin
      Result := SFH_EC_INVALID_VTF_FILE;
      exit;
    end;

    // Check to see if the game is even installed, so we know if it's even
    // feasible to install the spray to it.
    if not DirectoryExists(SteamappsPath + GameRelPath) then begin
      Result := SFH_EC_GAME_NOT_INSTALLED;
      exit;
    end;

    // Check file size of the spray to see if it's small enough for the game
    if VTFFile.Size > FGames[GameIndex].SizeSupport then begin
      Result := SFH_EC_SPRAY_TOO_LARGE;
      exit;
    end;

    // If the game is installed, it's now time for the exporting.
    // Get the SPRAYNAME first...
    SprayName := ExtractFileName(SprayFileName);
    SprayName := ChangeFileExt(SprayName,'');

    // then export the VTF file to the right locations...
    i := 0;
    PathOut := FGames[GameIndex].Paths.GetVTFRelPath(i,Username,SprayName);
    while PathOut <> SFHC_OOB do begin
      ForceDirectories(SteamappsPath + GameRelPath + ExtractFileDir(PathOut));
      VTFFile.Seek(0,soFromBeginning);
      try
        OutFile := TFileStream.Create(SteamappsPath + GameRelPath +
          FGames[GameIndex].Paths.GetVTFRelPath(i,Username,SprayName),fmCreate);
        OutFile.CopyFrom(VTFFile,VTFFile.Size);
      except
        on Exception do begin
          OutFile.Free;
          Result := SFH_EC_IO_ERROR;
        end;
      end;
      OutFile.Free;
      Inc(i);
      PathOut := FGames[GameIndex].Paths.GetVTFRelPath(i,Username,SprayName);
    end;

    // then process and export VMT files...
    i := 0;
    PathOut := FGames[GameIndex].Paths.GetVMTRelPath(i,Username,SprayName);
    while PathOut <> SFHC_OOB do begin
      try
        ForceDirectories(SteamappsPath + GameRelPath + ExtractFileDir(PathOut));
        VMTBuffer.Clear;
        RsrcAccess := TResourceStream.Create(HInstance,FGames[GameIndex].Paths.GetVMTResourceName(i),RT_RCDATA);
        VMTBuffer.LoadFromStream(RsrcAccess,TEncoding.ANSI);
        RsrcAccess.Free;
        for j := 0 to VMTBuffer.Count - 1 do
          VMTBuffer.Strings[j] := StringReplace(VMTBuffer.Strings[j],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
        VMTBuffer.SaveToFile(SteamappsPath + GameRelPath +
          FGames[GameIndex].Paths.GetVMTRelPath(i,Username,SprayName));
      except
        on Exception do begin
          OutFile.Free;
          Result := SFH_EC_IO_ERROR;
        end;
      end;
      Inc(i);
      PathOut := FGames[GameIndex].Paths.GetVMTRelPath(i,Username,SprayName);
    end;

  finally
    VMTBuffer.Free;
    VTFFile.Free;
  end;
end;

procedure TSprayFileHandler.PopulateStringList(AList: TStrings);
var
  i: NativeInt;
begin
  for i := 0 to Length(FGames) - 1 do
    AList.Append(FGames[i].GameName);
end;

procedure TSprayFileHandler.ReadAllGameAndLayoutInfo;
var
  LayoutInfo, GameInfo: TStringList;
  LayoutRsrc, GameRsrc: TResourceStream;
  LayoutNames: array of string;
  ParsePrtStr,ParsePrtStr2: string;
  CurIndex,i,j: NativeInt;
begin
  LayoutInfo := TStringList.Create;
  GameInfo := TStringList.Create;

  LayoutRsrc := TResourceStream.Create(HInstance,SFH_LAYOUT_RSRC,RT_RCDATA);
  GameRsrc := TResourceStream.Create(HInstance,SFH_GAME_RSRC,RT_RCDATA);

  LayoutInfo.LoadFromStream(LayoutRsrc);
  GameInfo.LoadFromStream(GameRsrc);

  LayoutRsrc.Free;
  GameRsrc.Free;

  try
    { process layout info }
    CurIndex := -1;
    // String Processing Section:
    for i := 0 to LayoutInfo.Count - 1 do begin
      // get cmdstr
      j := 1;
      ParsePrtStr := '';
      while (j <= Length(LayoutInfo.Strings[i])) and (LayoutInfo.Strings[i][j] <> ':') do begin
        ParsePrtStr := ParsePrtStr + LayoutInfo.Strings[i][j];
        Inc(j);
      end;
      // empty cmdstr: new layout
      if (ParsePrtStr = '') and (LayoutInfo.Strings[i][1] = ':') then begin
        Inc(CurIndex);
        SetLength(FLayouts,CurIndex + 1);
        SetLength(LayoutNames,CurIndex + 1);
        FLayouts[CurIndex] := TSprayFileLayout.Create;
        j := 2;
        while (j <= Length(LayoutInfo.Strings[i])) do begin
          LayoutNames[CurIndex] := LayoutNames[CurIndex] + LayoutInfo.Strings[i][j];
          Inc(j);
        end;
      // vtfout: VTF directory information
      end else if ParsePrtStr = 'vtfout' then begin
        if CurIndex = -1 then
          raise Exception.CreateFmt(SFH_E_LAYOUT_OUTSIDE,[i,'vtfout'])
        else begin
          ParsePrtStr := '';
          j := 8;
          while (j <= Length(LayoutInfo.Strings[i])) and (LayoutInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr := ParsePrtStr + LayoutInfo.Strings[i][j];
            Inc(j);
          end;
          if j <= Length(LayoutInfo.Strings[i]) then
            raise Exception.CreateFmt(SFH_E_TOO_MANY_ARGS,[i,'vtfout']);
          FLayouts[CurIndex].AddVTFRelPath(ParsePrtStr);
        end;
      // vmtout: VMT resource and directory inforrmation
      end else if ParsePrtStr = 'vmtout' then begin
        if CurIndex = -1 then
          raise Exception.CreateFmt(SFH_E_LAYOUT_OUTSIDE,[i,'vmtout'])
        else begin
          ParsePrtStr := '';
          ParsePrtStr2 := '';
          j := 8;
          while (j <= Length(LayoutInfo.Strings[i])) and (LayoutInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr := ParsePrtStr + LayoutInfo.Strings[i][j];
            Inc(j);
          end;
          Inc(j);
          while (j <= Length(LayoutInfo.Strings[i])) and (LayoutInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr2 := ParsePrtStr2 + LayoutInfo.Strings[i][j];
            Inc(j);
          end;
          if j <= Length(LayoutInfo.Strings[i]) then
            raise Exception.CreateFmt(SFH_E_TOO_MANY_ARGS,[i,'vmtout']);
          FLayouts[CurIndex].AddVMTRelPath(ParsePrtStr,ParsePrtStr2);
        end;
      end;
    end;

    { process game info }
    CurIndex := -1;
    // String Processing Section:
    for i := 0 to GameInfo.Count - 1 do begin
      // get cmdstr
      j := 1;
      ParsePrtStr := '';
      while (j <= Length(GameInfo.Strings[i])) and (GameInfo.Strings[i][j] <> ':') do begin
        ParsePrtStr := ParsePrtStr + GameInfo.Strings[i][j];
        Inc(j);
      end;
      // empty cmdstr: new game
      if (ParsePrtStr = '') and (GameInfo.Strings[i][1] = ':') then begin
        Inc(CurIndex);
        SetLength(FGames,CurIndex + 1);
        FGames[CurIndex] := TSprayGameInfo.Create;
        j := 2;
        ParsePrtStr := '';
        while (j <= Length(GameInfo.Strings[i])) do begin
          ParsePrtStr := ParsePrtStr + GameInfo.Strings[i][j];
          Inc(j);
        end;
        FGames[CurIndex].GameName := ParsePrtStr;
      // sizesupport: Max file size of VTF texture
      end else if ParsePrtStr = 'sizesupport' then begin
        if CurIndex = -1 then
          raise Exception.CreateFmt(SFH_E_GAME_OUTSIDE,[i,'sizesupport'])
        else begin
          ParsePrtStr := '';
          j := 13;
          while (j <= Length(GameInfo.Strings[i])) and (GameInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr := ParsePrtStr + GameInfo.Strings[i][j];
            Inc(j);
          end;
          if j <= Length(GameInfo.Strings[i]) then
            raise Exception.CreateFmt(SFH_E_TOO_MANY_ARGS,[i,'sizesupport']);
          FGames[CurIndex].SizeSupport := StrToInt(ParsePrtStr);
        end;
      // filelayout: Name of file organization layout from LayoutInfo
      end else if ParsePrtStr = 'filelayout' then begin
        if CurIndex = -1 then
          raise Exception.CreateFmt(SFH_E_GAME_OUTSIDE,[i,'filelayout'])
        else begin
          ParsePrtStr := '';
          j := 12;
          while (j <= Length(GameInfo.Strings[i])) and (GameInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr := ParsePrtStr + GameInfo.Strings[i][j];
            Inc(j);
          end;
          if j <= Length(GameInfo.Strings[i]) then
            raise Exception.CreateFmt(SFH_E_TOO_MANY_ARGS,[i,'filelayout']);
          // search for correct layout info from name given in ParsePrtStr
          j := 0;
          while (j < Length(LayoutNames)) and (LayoutNames[j] <> ParsePrtStr) do
            Inc(j);
          if j = Length(LayoutNames) then
            raise Exception.CreateFmt(SFH_E_GAME_UNDEFINED_LAYOUT,[i,ParsePrtStr])
          else
            FGames[CurIndex].Paths := FLayouts[j];
        end;
      // gamereldir: Game path relative to steamapps
      end else if ParsePrtStr = 'gamereldir' then begin
        if CurIndex = -1 then
          raise Exception.CreateFmt(SFH_E_GAME_OUTSIDE,[i,'gamereldir'])
        else begin
          ParsePrtStr := '';
          j := 12;
          while (j <= Length(GameInfo.Strings[i])) and (GameInfo.Strings[i][j] <> ':') do begin
            ParsePrtStr := ParsePrtStr + GameInfo.Strings[i][j];
            Inc(j);
          end;
          if j <= Length(GameInfo.Strings[i]) then
            raise Exception.CreateFmt(SFH_E_TOO_MANY_ARGS,[i,'gamereldir']);
          FGames[CurIndex].GameRelPath := ParsePrtStr;
        end;
      end;
    end;
  finally
    GameInfo.Free;
    LayoutInfo.Free;
  end;

  // DEBUG
  DebugDumpLayoutInfo('C:\Users\Gage\Desktop\layoutdump.txt');
  DebugDumpGameInfo('C:\Users\Gage\Desktop\gamedump.txt');
end;

{ Debug Functions }

procedure TSprayFileHandler.DebugDumpLayoutInfo(FileName: string);
var
  TheFile: TFileStream;
  TheOutput: TStringList;
  i,j: NativeInt;
begin
  TheFile := TFileStream.Create(FileName,fmCreate);
  TheOutput := TStringList.Create;

  TheOutput.Append('Parsed Layout Definitions');

  for i := 0 to Length(FLayouts) - 1 do begin
    TheOutput.Append(Format('  Layout Definition %d:',[i]));
    j := 0;
    TheOutput.Append('    VTF Export Relative Paths:');
    while FLayouts[i].GetVTFRelPath(j,'{$USERNAME}','{$SPRAYNAME}') <> SFHC_OOB do begin
      TheOutput.Append(Format('      Path %d:'#$0D#$0A'        %s',[j,FLayouts[i].GetVTFRelPath(j,'{$USERNAME}','{$SPRAYNAME}')]));
      Inc(j);
    end;
    j := 0;
    TheOutput.Append('    VMT Resource Names and Relative Export Paths:');
    while FLayouts[i].GetVMTRelPath(j,'{$USERNAME}','{$SPRAYNAME}') <> SFHC_OOB do begin
      TheOutput.Append(Format('      Entry %d:',[j]));
      TheOutput.Append(Format('        Path: %s',[FLayouts[i].GetVMTRelPath(j,'{$USERNAME}','{$SPRAYNAME}')]));
      TheOutput.Append(Format('        Resource: %s',[FLayouts[i].GetVMTResourceName(j)]));
      Inc(j);
    end;
  end;

  TheOutput.SaveToStream(TheFile);
  TheOutput.Free;
  TheFile.Free;
end;

procedure TSprayFileHandler.DebugDumpGameInfo(FileName: string);
var
  TheFile: TFileStream;
  TheOutput: TStringList;
  i,j: NativeInt;
begin
  TheFile := TFileStream.Create(FileName,fmCreate);
  TheOutput := TStringList.Create;

  TheOutput.Append('Parsed Game Definitions');

  for i := 0 to Length(FGames) - 1 do begin
    // Basic Game Info
    TheOutput.Append('');
    TheOutput.Append(Format('  Game Name: %s',[FGames[i].GameName]));
    TheOutput.Append(Format('  Game Relative Path: %s',[FGames[i].GameRelPath]));
    TheOutput.Append(Format('  Max Supported Spray Size (bytes): %d',[FGames[i].SizeSupport]));
    // Layout Info
    TheOutput.Append('  Layout Definition:');
    j := 0;
    TheOutput.Append('    VTF Export Relative Paths:');
    while FGames[i].Paths.GetVTFRelPath(j,'{$USERNAME}','{$SPRAYNAME}') <> SFHC_OOB do begin
      TheOutput.Append(Format('      Path %d:'#$0D#$0A'        %s',[j,FGames[i].Paths.GetVTFRelPath(j,'{$USERNAME}','{$SPRAYNAME}')]));
      Inc(j);
    end;
    j := 0;
    TheOutput.Append('    VMT Resource Names and Relative Export Paths:');
    while FGames[i].Paths.GetVMTRelPath(j,'{$USERNAME}','{$SPRAYNAME}') <> SFHC_OOB do begin
      TheOutput.Append(Format('      Entry %d:',[j]));
      TheOutput.Append(Format('        Path: %s',[FGames[i].Paths.GetVMTRelPath(j,'{$USERNAME}','{$SPRAYNAME}')]));
      TheOutput.Append(Format('        Resource: %s',[FGames[i].Paths.GetVMTResourceName(j)]));
      Inc(j);
    end;
  end;

  TheOutput.SaveToStream(TheFile);
  TheOutput.Free;
  TheFile.Free;
end;

end.
