unit UpdMainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Winapi.ShellApi;

function FileInUse(FileName: string): Boolean;

type
  TForm2 = class(TForm)
    StatusLbl: TLabel;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

function FileInUse(FileName: string): Boolean;
var
  hFileRes: HFILE;
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

procedure TForm2.FormActivate(Sender: TObject);
var
  ExtractRes: TResourceStream;
  ExtractDest: TFileStream;
  DestFileName: string;
begin
  DestFileName := ExtractFilePath(Application.ExeName) + 'SprayInstaller.exe';
  ExtractRes := TResourceStream.Create(HInstance,'PAYLOAD',RT_RCDATA);
  StatusLbl.Caption := 'Wait for the application being updated to close.';
  while (FileInUse(DestFileName))and (FileExists(DestFileName)) do begin
    Application.ProcessMessages;
  end;

  StatusLbl.Caption := 'Extracting update...';

  Application.ProcessMessages;

  try
    ExtractDest := TFileStream.Create(DestFileName,fmCreate);
    ExtractRes.SaveToStream(ExtractDest);
  finally
    ExtractRes.Free;
    ExtractDest.Free;
  end;

  ShellExecute(0, 'OPEN', PChar(DestFileName), '', '', SW_SHOWNORMAL);

  Close;
end;

end.
