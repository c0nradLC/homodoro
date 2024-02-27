module UI.Attributes
  ( attributes,
    timerAttr,
    taskEditorAttr,
    selectedTaskAttr,
  )
where

import Brick (AttrMap, AttrName, attrMap, attrName, on)
import qualified Graphics.Vty as V

attributes :: AttrMap
attributes =
  attrMap
    V.defAttr
    [ (timerAttr, V.black `on` V.white),
      (taskEditorAttr, V.black `on` V.white),
      (selectedTaskAttr, V.black `on` V.white)
    ]

timerAttr :: AttrName
timerAttr = attrName "timerAttr"

taskEditorAttr :: AttrName
taskEditorAttr = attrName "taskEditor"

selectedTaskAttr :: AttrName
selectedTaskAttr = attrName "selectedTaskAttr"
