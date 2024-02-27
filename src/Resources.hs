{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( Name (..),
    AppState (..),
    Tick (..),
    timerRunning,
    currentTimer,
    initialTimer,
    taskEditor,
    taskList,
    focus,
  )
where

import qualified Brick.Focus as BF
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as BL
import Control.Lens
import qualified Data.Text as Txt
import qualified Task.Task as TK

data Name
  = TaskInsert
  | TaskEdit
  | TaskList
  | Commands
  deriving (Show, Eq, Ord)

data Tick = Tick

data AppState = AppState
  { _timerRunning :: Bool,
    _currentTimer :: Int,
    _initialTimer :: Int,
    _taskEditor :: Editor Txt.Text Name,
    _taskList :: BL.List Name TK.Task,
    _focus :: BF.FocusRing Name
  }

makeLenses ''AppState
