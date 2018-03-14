object FilterWizard: TFilterWizard
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object PopupMenu1: TPopupMenu
    Left = 16
    Top = 24
    object miOption: TMenuItem
      Caption = #22806#37096#25972#24418#12484#12540#12523#12458#12503#12471#12519#12531'(&U)...'
      OnClick = miOptionClick
    end
  end
end
