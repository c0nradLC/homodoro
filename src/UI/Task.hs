{-# LANGUAGE OverloadedStrings #-}

module UI.Task (drawTaskEditor, drawTaskList) where

import Brick (Padding (Pad), Widget, emptyWidget, padLeft, padTop, str, txt, txtWrap, vBox, withAttr, withBorderStyle, (<=>))
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit (renderEditor)
import qualified Brick.Widgets.List as BL
import Control.Lens
import qualified Data.Text as Txt
import Resources
import qualified Resources as R
import UI.Attributes (selectedTaskAttr, taskCompletedLabelAttr, taskCompletedWhiteBgLabelAttr, taskPendingLabelAttr, taskPendingWhiteBgLabelAttr)

drawTaskEditor :: AppState -> Widget Name
drawTaskEditor s =
    padTop (Pad 1) $
        C.hCenter $
            B.borderWithLabel (str "Task") $
                renderEditor drawTaskEditorContent (BF.focusGetCurrent (s ^. focus) == Just (TaskEdit Insert) || BF.focusGetCurrent (s ^. focus) == Just (TaskEdit Edit)) (s ^. taskEditor)

drawTaskEditorContent :: [Txt.Text] -> Widget Name
drawTaskEditorContent t = txt (Txt.unlines t)

drawTaskList :: BL.List Name Task -> Widget Name
drawTaskList tasks = do
    B.borderWithLabel (str "Tasks") $
        BL.renderList drawTaskListItem True tasks

drawTaskListItem :: Bool -> Task -> Widget Name
drawTaskListItem sel task
    | sel =
        withAttr selectedTaskAttr $
            padLeft (Pad 1) $
                taskListItem task sel
    | otherwise =
        taskListItem task sel

taskListItem :: Task -> Bool -> Widget Name
taskListItem task sel =
    vBox
        [ withBorderStyle BS.unicodeRounded $
            B.border $
                padLeft (Pad 1) $
                    txtWrap (task ^. R.taskContent)
                        <=> padTop
                            (Pad 1)
                            ( emptyWidget
                                <=> drawTaskStatus (task ^. R.taskCompleted) sel
                            )
        ]

drawTaskStatus :: Bool -> Bool -> Widget Name
drawTaskStatus isCompleted isSelected
    | isCompleted =
        if isSelected
            then withAttr taskCompletedWhiteBgLabelAttr $ txt "Completed"
            else withAttr taskCompletedLabelAttr $ txt "Completed"
    | otherwise =
        if isSelected
            then withAttr taskPendingWhiteBgLabelAttr $ txt "Pending"
            else withAttr taskPendingLabelAttr $ txt "Pending"
