module UI.Attributes (
    attributes,
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
)
where

import Brick (AttrMap, AttrName, attrMap, attrName, on)
import Brick.Widgets.Dialog (buttonAttr, buttonSelectedAttr)
import Brick.Widgets.FileBrowser (fileBrowserCurrentDirectoryAttr, fileBrowserRegularFileAttr, fileBrowserSelectionInfoAttr)
import Brick.Widgets.List (listSelectedAttr)
import Graphics.Vty (withForeColor)
import qualified Graphics.Vty as V

attributes :: AttrMap
attributes =
    attrMap
        V.defAttr
        [ (timerAttr, V.black `on` V.white)
        , (taskEditorAttr, V.black `on` V.white)
        , (selectedTaskAttr, V.black `on` V.white)
        , (selectedTimerAttr, V.black `on` V.white)
        , (taskCompletedLabelAttr, V.defAttr `withForeColor` V.blue)
        , (taskPendingLabelAttr, V.defAttr `withForeColor` V.red)
        , (taskCompletedWhiteBgLabelAttr, V.blue `on` V.white)
        , (taskPendingWhiteBgLabelAttr, V.red `on` V.white)
        , (selectedConfigAttr, V.black `on` V.white)
        , (buttonSelectedAttr, V.black `on` V.white)
        , (buttonAttr, V.white `on` V.black)
        , (listSelectedAttr, V.black `on` V.white)
        , (fileBrowserCurrentDirectoryAttr, V.defAttr `withForeColor` V.green)
        , (fileBrowserSelectionInfoAttr, V.defAttr `withForeColor` V.green)
        , (fileBrowserRegularFileAttr, V.defAttr `withForeColor` V.cyan)
        , (blackOnWhiteAttr, V.black `on` V.white)
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
