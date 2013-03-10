object frmUpdater: TfrmUpdater
  Left = 948
  Top = 161
  BorderIcons = []
  BorderStyle = bsToolWindow
  ClientHeight = 122
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 0
    Top = 0
    Width = 311
    Height = 122
    Align = alClient
    Transparent = True
  end
  object lblProgress: TLabel
    Left = 37
    Top = 44
    Width = 55
    Height = 13
    Caption = 'Percentage'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    Visible = False
  end
  object lblTitle: TLabel
    Left = 33
    Top = 16
    Width = 246
    Height = 16
    Caption = 'Please, wait while downloading updates...'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    Visible = False
  end
  object lblChecking: TLabel
    Left = 88
    Top = 53
    Width = 135
    Height = 16
    Caption = 'Checking for updates...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Visible = False
  end
  object prb1: TProgressBar
    Left = 37
    Top = 64
    Width = 236
    Height = 17
    Smooth = True
    TabOrder = 0
    Visible = False
  end
end
