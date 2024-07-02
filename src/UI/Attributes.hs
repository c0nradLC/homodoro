module UI.Attributes (
    attributes,
    timerAttr,
    taskEditorAttr,
    selectedTaskAttr,
    selectedTimerAttr,
    taskCompletedLabelAttr,
    taskPendingLabelAttr,
)
where

import Brick (AttrMap, AttrName, attrMap, attrName, on)
import qualified Graphics.Vty as V

attributes :: AttrMap
attributes =
    attrMap
        V.defAttr
        [ (timerAttr, V.black `on` V.white)
        , (taskEditorAttr, V.black `on` V.white)
        , (selectedTaskAttr, V.black `on` V.white)
        , (selectedTimerAttr, V.black `on` V.white)
        , (taskCompletedLabelAttr, V.blue `on` V.white)
        , (taskPendingLabelAttr, V.red `on` V.white)
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

taskPendingLabelAttr :: AttrName
taskPendingLabelAttr = attrName "taskPendingLabelAttr"
