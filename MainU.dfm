object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Vampyrium Spray Importer 2.0.1.0'
  ClientHeight = 243
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 256
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GameDropPanel: TPanel
    Left = 8
    Top = 8
    Width = 568
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 528
    DesignSize = (
      568
      34)
    object GameLabel: TLabel
      Left = 0
      Top = 3
      Width = 31
      Height = 13
      Caption = 'Game:'
    end
    object GameStatusLbl: TLabel
      Left = 0
      Top = 21
      Width = 568
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Caption = 'Select a game above to begin.'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
      ExplicitWidth = 146
    end
    object GameComboBox: TComboBox
      Left = 37
      Top = 0
      Width = 356
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = GameComboBoxChange
      ExplicitWidth = 316
    end
    object GamePathSelBtn: TButton
      Left = 393
      Top = 0
      Width = 78
      Height = 21
      Hint = 
        'Use if using a custom directory for this game instead of the Ste' +
        'am default'
      Anchors = [akTop, akRight]
      Caption = 'Game Path...'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = GamePathSelBtnClick
      ExplicitLeft = 353
    end
    object ClearGamePathBtn: TButton
      Left = 472
      Top = 0
      Width = 96
      Height = 21
      Hint = 'Clear current game'#39's install path from application'
      Anchors = [akTop, akRight]
      Caption = 'Clear Game Path'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ClearGamePathBtnClick
      ExplicitLeft = 432
    end
  end
  object MainPnl: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 50
    Width = 568
    Height = 152
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 528
    object Splitter1: TSplitter
      Left = 304
      Top = 0
      Width = 8
      Height = 152
      Align = alRight
      AutoSnap = False
      MinSize = 256
      ResizeStyle = rsUpdate
      ExplicitLeft = 316
      ExplicitTop = -2
    end
    object ExistingSprayPnl: TPanel
      Left = 312
      Top = 0
      Width = 256
      Height = 152
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 272
      object ExistingSpraysLbl: TLabel
        Left = 0
        Top = 0
        Width = 256
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Sprays In Game'
        ExplicitWidth = 76
      end
      object ExistingSprayList: TListBox
        Left = 0
        Top = 13
        Width = 256
        Height = 114
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 0
        OnClick = ExistingSprayListClick
        OnKeyDown = ListKeyDown
        OnKeyPress = ExistingSprayListKeyPress
      end
      object DeleteSprayBtn: TButton
        Left = 0
        Top = 127
        Width = 256
        Height = 25
        Align = alBottom
        Caption = 'Delete Spray...'
        Enabled = False
        TabOrder = 1
        OnClick = DeleteSprayBtnClick
      end
    end
    object ImportSpraysPnl: TPanel
      Left = 0
      Top = 0
      Width = 304
      Height = 152
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 264
      object ImportSpraysLbl: TLabel
        Left = 0
        Top = 0
        Width = 304
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Sprays To Import'
        ExplicitWidth = 83
      end
      object ImportSpraysList: TListBox
        Left = 0
        Top = 13
        Width = 304
        Height = 89
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 0
        OnClick = ImportSpraysListClick
        OnKeyDown = ListKeyDown
        OnKeyPress = ImportSpraysListKeyPress
        ExplicitWidth = 264
      end
      object ImportSpraysBtnPnl: TPanel
        Left = 0
        Top = 102
        Width = 304
        Height = 50
        Align = alBottom
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 101
        ExplicitWidth = 264
        object RemoveSpraysBtn: TButton
          Left = 109
          Top = 0
          Width = 94
          Height = 25
          Hint = 'Remove selected sprays from the Sprays To Import list'
          Align = alRight
          Caption = 'Remove Sprays'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = RemoveSpraysBtnClick
          ExplicitLeft = 50
          ExplicitTop = -6
        end
        object ImportSelBtn: TButton
          Left = 203
          Top = 0
          Width = 101
          Height = 25
          Hint = 'Import selected sprays into %s'
          Align = alRight
          Caption = 'Import Selected >'
          Enabled = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = ImportSelBtnClick
          ExplicitLeft = 288
        end
        object ImportAllBtn: TButton
          Left = 0
          Top = 25
          Width = 304
          Height = 25
          Hint = 'Import all Sprays To Import into %s'
          Align = alBottom
          Caption = 'Import All >>>'
          Enabled = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = ImportAllBtnClick
          ExplicitWidth = 264
        end
        object AddSpraysBtn: TButton
          Left = 0
          Top = 0
          Width = 109
          Height = 25
          Align = alClient
          Caption = 'Add Sprays...'
          TabOrder = 3
          OnClick = AddSpraysBtnClick
          ExplicitWidth = 105
        end
      end
    end
  end
  object AboutBtnPnl: TPanel
    Left = 8
    Top = 202
    Width = 568
    Height = 33
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 528
    DesignSize = (
      568
      33)
    object AboutBtn: TButton
      AlignWithMargins = True
      Left = 512
      Top = 8
      Width = 56
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akTop, akRight]
      Caption = 'About...'
      TabOrder = 0
      OnClick = AboutBtnClick
      ExplicitLeft = 472
    end
  end
  object SprayImportDlg: TOpenDialog
    DefaultExt = 'vtf'
    Filter = 'Valve Texture Files (*.vtf)|*.vtf'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Import Sprays'
    Left = 400
    Top = 48
  end
end
