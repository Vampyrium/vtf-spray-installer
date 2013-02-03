unit SprayFileHandler;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

function SaveSprayToGame(SDir,SprayFileName,SUsername: string; GameIndex: integer): integer;
function GetPartialDirFromGame(GameIndex: integer; SUsername: string): string;
function ExportVMTFilesFromGame(GameIndex: integer; SDir,SUsername,SprayName: string): string;

const
  VTF_MAGIC: integer = $00465456; //Little endian, 'VTF'#0
  INDEX_TF2: integer = 0;
  INDEX_L4D2: integer = 1;
  INDEX_L4D: integer = 2;
  INDEX_CSS: integer = 3;
  INDEX_GMOD: integer = 4;
  INDEX_NMRIH: integer = 5;
  RES_SUPPORT: array[0..5] of NativeInt =
  (
    {TF2} 524288 {hi-res},
    {L4D2} 122880 {normal-res},
    {L4D} 122880 {normal-res},
    {CS:S} 524288 {hi-res},
    {GMod} 524288 {hi-res},
    {NMRiH} 122880 {normal res}
  );

implementation

function SaveSprayToGame(SDir,SprayFileName,SUsername: string; GameIndex: integer): integer;
var
  VTFBuffer: TMemoryStream;
  SprayName: string;
  MagicBuffer: integer;
begin
  SprayName := ExtractFileName(SprayFileName);
  SprayName := ChangeFileExt(SprayName,'');
  VTFBuffer := TMemoryStream.Create;
  try
    VTFBuffer.LoadFromFile(SprayFileName);
  except
    on Exception do begin
      VTFBuffer.Free;
      Result := 1; {There was an I/O error.}
      exit;
    end;
  end;

  // Checking if file is a valid VTF file (prevents accidentally adding a TGA or PNG file)
  VTFBuffer.Seek(0,soFromBeginning);
  VTFBuffer.ReadBuffer(MagicBuffer,4);
  if MagicBuffer <> VTF_MAGIC then begin
    VTFBuffer.Free;
    Result := 3; {Invalid format.}
    Exit;
  end;
  VTFBuffer.Seek(0,soFromBeginning);

  if VTFBuffer.Size > RES_SUPPORT[GameIndex] then begin
    Result := 2; {Spray is too big for this game.}
  end else begin
    try
      case GameIndex of
        {TF2} 0: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                + 'materials\vgui\logos\' + SprayName + '.vtf');
        {L4D2} 1: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                 + 'materials\vgui\logos\custom\' + SprayName + '.vtf');
        {L4D} 2: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                + 'materials\vgui\logos\custom\' + SprayName + '.vtf');
        {CS:S} 3: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                 + 'materials\vgui\logos\' + SprayName + '.vtf');
        {GMod} 4: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                 + 'materials\vgui\logos\' + SprayName + '.vtf');
        {NMRiH} 5: VTFBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
                  + 'materials\vgui\logos\' + SprayName + '.vtf');
      end;
    except
      on Exception do begin
        VTFBuffer.Free;
        Result := 1; {There was an I/O error.}
        exit;
      end;
    end;
    ExportVMTFilesFromGame(GameIndex,SDir,SUsername,SprayName);

    Result := 0;
  end;

  VTFBuffer.Free;
end;

function GetPartialDirFromGame(GameIndex: integer; SUsername: string): string;
begin
  case GameIndex of
    0: Result := Format('%s\team fortress 2\tf\',[SUsername]);            //TF2
    1: Result := 'common\left 4 dead 2\left4dead2\';                      //L4D2
    2: Result := 'common\left 4 dead\left4dead\';                         //L4D
    3: Result := Format('%s\counter-strike source\cstrike\',[SUsername]); //CS:S
    4: Result := Format('%s\garrysmod\garrysmod\',[SUsername]);           //GMod
    5: Result := 'sourcemods\nmrih\';                                     //NMRiH
  end;
end;

function ExportVMTFilesFromGame(GameIndex: integer; SDir,SUsername,SprayName: string): string;
var
  VMTBuffer: TStringList;
  VMTResource: TResourceStream;
  i: Integer;
begin
  VMTBuffer := TStringList.Create;
  case GameIndex of


    0: begin //TF2
      VMTResource := TResourceStream.Create(HInstance,'VMTTF2VGUILOGOS',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\' + SprayName + '.vmt');
    end;


    1: begin //L4D2
      // for the VMT file in custom
      VMTResource := TResourceStream.Create(HInstance,'VMTL4D2VGUILOGOSCUSTOM',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\custom\' + SprayName + '.vmt');
      // for the VMT file in UI
      VMTResource := TResourceStream.Create(HInstance,'VMTL4D2VGUILOGOSUI',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\UI\' + SprayName + '.vmt');
    end;


    2: begin //L4D
      // for the VMT file in custom
      VMTResource := TResourceStream.Create(HInstance,'VMTL4DVGUILOGOSCUSTOM',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\custom\' + SprayName + '.vmt');
      // for the VMT file in UI
      VMTResource := TResourceStream.Create(HInstance,'VMTL4DVGUILOGOSUI',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\UI\' + SprayName + '.vmt');
    end;


    3: begin //CS:S
      // for the VMT file in logos
      VMTResource := TResourceStream.Create(HInstance,'VMTCSSVGUILOGOS',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\' + SprayName + '.vmt');
      // for the VMT file in logos\UI
      VMTResource := TResourceStream.Create(HInstance,'VMTCSSVGUILOGOSUI',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\UI\' + SprayName + '.vmt');
    end;


    4: begin //GMod
      // for the VMT file in logos
      VMTResource := TResourceStream.Create(HInstance,'VMTGMODVGUILOGOS',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\' + SprayName + '.vmt');
      // for the VMT file in logos\UI
      VMTResource := TResourceStream.Create(HInstance,'VMTGMODVGUILOGOSUI',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\UI\' + SprayName + '.vmt');
    end;


    5: begin //NMRiH
      // for the VMT file in logos
      VMTResource := TResourceStream.Create(HInstance,'VMTNMRIHVGUILOGOS',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\' + SprayName + '.vmt');
      // for the VMT file in logos\UI
      VMTResource := TResourceStream.Create(HInstance,'VMTNMRIHVGUILOGOSUI',RT_RCDATA);
      VMTBuffer.LoadFromStream(VMTResource,TEncoding.ANSI);
      VMTResource.Free;
      for i := 0 to VMTBuffer.Count - 1 do
        VMTBuffer.Strings[i] := StringReplace(VMTBuffer.Strings[i],'{$SPRAYNAME}',SprayName,[rfReplaceAll]);
      VMTBuffer.SaveToFile(IncludeTrailingBackslash(SDir) + GetPartialDirFromGame(GameIndex,SUsername)
        + 'materials\vgui\logos\UI\' + SprayName + '.vmt');
    end;


  end;
  VMTBuffer.Free;
end;

end.
