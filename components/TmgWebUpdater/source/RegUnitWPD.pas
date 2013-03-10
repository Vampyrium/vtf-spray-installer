unit RegUnitWPD;

interface

uses DesignIntf, DesignEditors, Dialogs, SysUtils, Classes, mgWebUpdater, Forms;

type
  TFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  procedure Register;

implementation

{ TFileNameProperty }

procedure Register;
begin
  RegisterComponents('mgToolbox', [TmgWebUpdater]);
  RegisterPropertyEditor(TypeInfo(TFileName), nil, '', TFileNameProperty);
end;


procedure TFileNameProperty.Edit;
begin
  inherited;
  with TOpenDialog.Create(Application) do
  try
    Title := GetName; { name of property as OpenDialog caption }
    Filename := GetValue;
    Filter := 'All supported Files (*.bmp;*.gif;*.jpg;*.jpeg)|*.bmp;*.gif;*.jpg;*.jpeg';
    HelpContext := 0;
    Options := Options + [ofShowHelp, ofPathMustExist, ofFileMustExist];
    if Execute then SetValue(Filename);
  finally
    Free
  end;
end;

function TFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
