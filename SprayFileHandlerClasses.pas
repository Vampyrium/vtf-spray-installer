{SprayFileHandlerClasses.pas - supplemental classes for managing spray exports

 TSprayInfo
   Methods:
     Create:
       basic constructor, creates empty class
     Destroy:
       basic destructor (for possible future use)
     GetGamePath(Username: string): string
       Retrieves the game relative path, with all USERNAME placeholders replaced
   Properties:
     GameName: string
       the name of a game; used for the user interface
     GameRelPath:
       The path to a game relative to steamapps
     SizeSupport: NativeUInt
       the maximum size VTF the game supports, in bytes
     Paths: TSprayFileLayout
       a TSprayFileLayout that contains the relative paths to the game and spray
       files (VTF and VMT)

 TSprayFileLayout
   Methods:
     Create:
       basic constructor; instantiates a blank TSprayFileLayout
     Destroy:
       frees up any memory used by the object, then removes object from memory
     GetVTFRelPath(index: NatuveUInt; Username, Sprayname: string): string
       Retrieves a VTF export relative path, with all USERNAME and SPRAYNAME
       placeholders replaced
     GetVMTRelPath(index: NatuveUInt; Username, Sprayname: string): string
       Retrieves a VMT export relative path, with all USERNAME and SPRAYNAME
       placeholders replaced
     GetVMTResourceName(index: NativeUInt): string
       Retrieves a VMT resource name (for retrieving a resource from the
       executable)
     AddVTFRelPath(APath: string)
       Stores a new VTF export path
     AddVMTRelPath(ARsrcName, APath: string)
       Stores a new VMT export path record
}

unit SprayFileHandlerClasses;

interface

uses
  System.SysUtils;

const
  SFHC_OOB: string = #1;

type TSprayFileLayout = class
  private
    FVTFRelPaths: array of string;
    FVMTRelPaths: array of string;
    FVMTRsrcs: array of string;
  public
    constructor Create;
    destructor Destroy;
    function GetVTFRelPath(index: NativeUInt; Username, Sprayname: string): string;
    function GetVMTRelPath(index: NativeUInt; Username, Sprayname: string): string;
    function GetVMTResourceName(index: NativeUInt): string;
    procedure AddVTFRelPath(APath: string);
    procedure AddVMTRelPath(ARsrcName, APath: string);

  end;

type TSprayGameInfo = class
  private
    FGameRelPath: string;
    FGameName: string;
    FSizeSupport: NativeUInt;
    FPaths: TSprayFileLayout;
  public
    constructor Create;
    destructor Destroy;
    function GetGameRelPath(Username: string): string;
    property GameName: string read FGameName write FGameName;
    property GameRelPath: string read FGameRelPath write FGameRelPath;
    property SizeSupport: NativeUInt read FSizeSupport write FSizeSupport;
    property Paths: TSprayFileLayout read FPaths write FPaths;
  end;

implementation

{ TSprayGameInfo }

constructor TSprayGameInfo.Create;
begin
  FGameName := '';
  FGameRelPath := '';
  FPaths := TSprayFileLayout.Create;
end;

destructor TSprayGameInfo.Destroy;
begin
  FPaths.Free;
  inherited;
end;

function TSprayGameInfo.GetGameRelPath(Username: string): string;
begin
  Result := StringReplace(GameRelPath,'{$USERNAME}',Username,[rfReplaceAll]);
end;

{ TSprayFileLayout }

constructor TSprayFileLayout.Create;
begin
  SetLength(FVTFRelPaths,0);
  SetLength(FVMTRelPaths,0);
  SetLength(FVMTRsrcs,0);
end;

destructor TSprayFileLayout.Destroy;
begin
  SetLength(FVTFRelPaths,0);
  SetLength(FVMTRelPaths,0);
  SetLength(FVMTRsrcs,0);
  inherited;
end;

function TSprayFileLayout.GetVTFRelPath(index: NativeUInt; Username, Sprayname: string): string;
var
  ProcString: string;
begin
  if index < Length(FVTFRelPaths) then begin
    ProcString := StringReplace(FVTFRelPaths[index],'{$USERNAME}',Username,[rfReplaceAll]);
    Result := StringReplace(ProcString,'{$SPRAYNAME}',Sprayname,[rfReplaceAll]);
  end else
    Result := SFHC_OOB;
end;

function TSprayFileLayout.GetVMTRelPath(index: NativeUInt; Username, Sprayname: string): string;
var
  ProcString: string;
begin
  if index < Length(FVMTRelPaths) then begin
    ProcString := StringReplace(FVMTRelPaths[index],'{$USERNAME}',Username,[rfReplaceAll]);
    Result := StringReplace(ProcString,'{$SPRAYNAME}',Sprayname,[rfReplaceAll]);
  end else
    Result := SFHC_OOB;
end;

function TSprayFileLayout.GetVMTResourceName(index: NativeUInt): string;
begin
  if index < Length(FVMTRsrcs) then
    Result := FVMTRsrcs[index]
  else
    Result := SFHC_OOB;
end;

procedure TSprayFileLayout.AddVTFRelPath(APath: string);
begin
  SetLength(FVTFRelPaths,Length(FVTFRelPaths) + 1);
  FVTFRelPaths[Length(FVTFRelPaths) - 1] := APath;
end;

procedure TSprayFileLayout.AddVMTRelPath(ARsrcName, APath: string);
begin
  SetLength(FVMTRelPaths,Length(FVMTRelPaths) + 1);
  FVMTRelPaths[Length(FVMTRelPaths) - 1] := APath;
  SetLength(FVMTRsrcs,Length(FVMTRsrcs) + 1);
  FVMTRsrcs[Length(FVMTRsrcs) - 1] := ARsrcName;
end;

end.
