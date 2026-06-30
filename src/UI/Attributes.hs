module UI.Attributes
  ( attributes,
    timerAttr,
    taskEditorAttr,
    selectedTaskAttr,
    selectedTimerAttr,
    taskCompletedLabelAttr,
    taskCompletedWhiteBgLabelAttr,
    taskPendingLabelAttr,
    taskPendingWhiteBgLabelAttr,
    selectedConfigAttr,
    blackOnWhiteAttr,
    strikeThroughTextAttr,
    defaultTextAttr,
  )
where

import Brick (AttrMap, AttrName, attrMap, attrName, on)
import Brick.Widgets.Dialog (buttonAttr, buttonSelectedAttr)
import Brick.Widgets.FileBrowser (fileBrowserCurrentDirectoryAttr, fileBrowserRegularFileAttr, fileBrowserSelectionInfoAttr)
import Brick.Widgets.List (listSelectedAttr)
import qualified Graphics.Vty as V

attributes :: AttrMap
attributes =
  attrMap
    V.defAttr
    [ (timerAttr, V.black `on` V.white),
      (taskEditorAttr, V.black `on` V.white),
      (selectedTaskAttr, V.black `on` V.white),
      (selectedTimerAttr, V.black `on` V.white),
      (taskCompletedLabelAttr, V.defAttr `V.withForeColor` V.blue),
      (taskPendingLabelAttr, V.defAttr `V.withForeColor` V.red),
      (taskCompletedWhiteBgLabelAttr, V.blue `on` V.white),
      (taskPendingWhiteBgLabelAttr, V.red `on` V.white),
      (selectedConfigAttr, V.black `on` V.white),
      (buttonSelectedAttr, V.black `on` V.white),
      (buttonAttr, V.white `on` V.black),
      (listSelectedAttr, V.black `on` V.white),
      (fileBrowserCurrentDirectoryAttr, V.defAttr `V.withForeColor` V.green),
      (fileBrowserSelectionInfoAttr, V.defAttr `V.withForeColor` V.green),
      (fileBrowserRegularFileAttr, V.defAttr `V.withForeColor` V.cyan),
      (blackOnWhiteAttr, V.black `on` V.white),
      (strikeThroughTextAttr, V.defAttr `V.withStyle` V.strikethrough)
    ]

timerAttr :: AttrName
timerAttr = attrName "timerAttr"

taskEditorAttr :: AttrName
taskEditorAttr = attrName "taskEditor"

selectedTaskAttr :: AttrName
selectedTaskAttr = attrName "selectedTaskAttr"

selectedTimerAttr :: AttrName
selectedTimerAttr = attrName "selectedTimerAttr"

taskCompletedLabelAttr :: AttrName
taskCompletedLabelAttr = attrName "taskCompletedLabelAttr"

taskCompletedWhiteBgLabelAttr :: AttrName
taskCompletedWhiteBgLabelAttr = attrName "taskCompletedWhiteBgLabelAttr"

taskPendingLabelAttr :: AttrName
taskPendingLabelAttr = attrName "taskPendingLabelAttr"

taskPendingWhiteBgLabelAttr :: AttrName
taskPendingWhiteBgLabelAttr = attrName "taskPendingWhiteBgLabelAttr"

selectedConfigAttr :: AttrName
selectedConfigAttr = attrName "selectedConfigAttr"

blackOnWhiteAttr :: AttrName
blackOnWhiteAttr = attrName "blackOnWhiteAttr"

strikeThroughTextAttr :: AttrName
strikeThroughTextAttr = attrName "strikeThroughTextAttr"

defaultTextAttr :: AttrName
defaultTextAttr = attrName "defaultTextAttr"
