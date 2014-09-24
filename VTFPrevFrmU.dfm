object VTFPrevForm: TVTFPrevForm
  Left = 857
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Spray Preview'
  ClientHeight = 546
  ClientWidth = 650
  Color = clBtnFace
  Constraints.MinHeight = 128
  Constraints.MinWidth = 150
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 520
    Height = 546
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clAppWorkSpace
    ParentColor = False
    TabOrder = 0
    object VTFPaintBox: TPaintBox
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      OnPaint = VTFPaintBoxPaint
    end
  end
  object VTFControlPnl: TPanel
    Left = 520
    Top = 0
    Width = 130
    Height = 546
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 68
      Height = 13
      Caption = 'Mipmap Level:'
    end
    object Label2: TLabel
      Left = 8
      Top = 42
      Width = 34
      Height = 13
      Caption = 'Frame:'
    end
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 1
      Height = 546
      Align = alLeft
      Brush.Style = bsClear
    end
    object MipSelector: TSpinEdit
      Left = 82
      Top = 5
      Width = 39
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = MipSelectorChange
    end
    object FrameSelector: TSpinEdit
      Left = 72
      Top = 39
      Width = 49
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = FrameSelectorChange
    end
    object AnimEnable: TCheckBox
      Left = 6
      Top = 67
      Width = 115
      Height = 17
      Action = AnimEnableAction
      State = cbChecked
      TabOrder = 2
    end
  end
  object AnimTimer: TTimer
    Interval = 200
    OnTimer = AnimTimerTimer
    Left = 320
    Top = 280
  end
  object ActionList1: TActionList
    Left = 320
    Top = 128
    object AnimEnableAction: TAction
      Category = 'Checkbox'
      AutoCheck = True
      Caption = 'Enable Animation'
      Checked = True
      OnExecute = AnimEnableActionExecute
    end
  end
end
