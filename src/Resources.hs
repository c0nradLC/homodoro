{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( Name (..),
    Timer (..),
    AppState (..),
    Tick (..),
    timerRunning,
    pomodoroTimer,
    shortBreakTimer,
    longBreakTimer,
    shortBreakInitialTimer,
    longBreakInitialTimer,
    pomodoroInitialTimer,
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

data Timer
  = Pomodoro
  | ShortBreak
  | LongBreak
  deriving (Show, Eq, Ord)

data Name
  = TaskInsert
  | TaskEdit
  | TaskList Timer
  | Commands
  deriving (Show, Eq, Ord)

data Tick = Tick

data AppState = AppState
  { _timerRunning :: Bool,
    _pomodoroTimer :: Int,
    _shortBreakTimer :: Int,
    _longBreakTimer :: Int,
    _pomodoroInitialTimer :: Int,
    _shortBreakInitialTimer :: Int,
    _longBreakInitialTimer :: Int,
    _taskEditor :: Editor Txt.Text Name,
    _taskList :: BL.List Name TK.Task,
    _focus :: BF.FocusRing Name
  }

makeLenses ''AppState
