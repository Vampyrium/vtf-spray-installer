object ChangelogFrm: TChangelogFrm
  Left = 0
  Top = 0
  Caption = 'Vampyrium Spray Installer: Change Log'
  ClientHeight = 262
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 221
    Width = 484
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      484
      41)
    object OKBtn: TButton
      Left = 401
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = OKBtnClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 484
    Height = 221
    Align = alClient
    Lines.Strings = (
      'Vampyrium Spray Installer'
      '=========='
      'Version 1.6.0.0'
      
        ' - Added update check feature. Just click the Vampyrium VTF Spra' +
        'y Handler logo and Check for '
      'Updates regularly!'
      ' - Overwrites currently-used spray in Counter-Strike: Source'
      '----------'
      'Version 1.5.0.0'
      ' - HUMONGOUS internal rewrite:'
      
        '     - Overhauled VTF/VMT export engine - less hardcoding, more ' +
        'code legibiity'
      '     - Added check to see if game is installed'
      
        '     - Force creates directories now if the proper directories d' +
        'o not exist (reduces errors!)'
      ' - Source code is now available!'
      '     - https://github.com/Vampyrium/vtf-spray-installer'
      '----------'
      'Version 1.2.1.0:'
      ' - Added an About Box'
      '----------'
      'Version 1.2.0.0:'
      ' - Added four games to the dropdown list:'
      '     - Left 4 Dead'
      '     - Counter-Strike: Source'
      '     - Garry'#39's Mod'
      '     - No More Room in Hell'
      
        ' - Added a file size check -- the installer will no longer allow' +
        ' you to add a spray that'#39's too big to '
      'a game'
      
        '     - The installer will add hi-resolution sprays to games that' +
        ' support them'
      
        ' - Added a file format check -- the installer will prevent you f' +
        'rom adding a non-VTF file as a '
      'spray')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
