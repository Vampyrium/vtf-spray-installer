object GameFolderHintFrm: TGameFolderHintFrm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'GameFolderHintFrm'
  ClientHeight = 54
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object HintLbl: TLabel
    Left = 1
    Top = 1
    Width = 198
    Height = 52
    AutoSize = False
    Caption = 
      'Hint: The game folder will look something like this:'#13#10#13#10'     ...' +
      '\Team Fortress 2\tf'
    Color = clInfoBk
    ParentColor = False
    Transparent = False
    WordWrap = True
  end
end
