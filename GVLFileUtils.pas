unit GVLFileUtils;

interface

uses Winapi.Windows, System.Sysutils;

function FileInUse(FileName: string): Boolean;

implementation

// WINDOWS: elegant solution to checking if file is in use
// http://www.scalabium.com/faq/dct0066.htm

function FileInUse(FileName: string): Boolean;
var hFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FileName) then exit;
  hFileRes := CreateFile(PChar(FileName),
                                    GENERIC_READ or GENERIC_WRITE,
                                    0,
                                    nil,
                                    OPEN_EXISTING,
                                    FILE_ATTRIBUTE_NORMAL,
                                    0);
  Result := (hFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(hFileRes);
end;

end.
