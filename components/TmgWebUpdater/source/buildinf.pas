unit buildinf;

interface

uses
    Windows, Messages, SysUtils, Classes, Forms;

procedure GetBuildInfo( var v1, v2, v3, v4 : Word );
function GetBuildInfoString(FName: string): string;

var
  FileName: string;

implementation
    
procedure GetBuildInfo( var v1, v2, v3, v4 : Word );
var
  VerInfoSize: DWord;
  VerInfo: Pointer;
  VerValueSize: DWord;
  VerValue: PVSFixedFileInfo;
  Dummy: DWord;
begin
  VerInfoSize := GetFileVersionInfoSize( PChar( FileName ), dummy );
  GetMem( VerInfo, VerInfoSize );
  GetFileVersionInfo( PChar( FileName ), 0, VerInfoSize, VerInfo );
  VerQueryValue( VerInfo, '\', Pointer( VerValue ), VerValueSize );
  with VerValue^ do
  begin
    v1 := dwFileVersionMS shr 16;
    v2 := dwFileVersionMS and $FFFF;
    v3 := dwFileVersionLS shr 16;
    v4 := dwFileVersionLS and $FFFF;
  end;
  FreeMem( VerInfo, VerInfoSize );
end;

function GetBuildInfoString(FName: string): string;
var
  v1, v2, v3, v4: Word;
begin
  FileName := FName;
  GetBuildInfo( v1, v2, v3, v4 );
  Result := Format( '%d.%d.%d  (Build %d)', [v1,v2,v3,v4] );
end;

end.
