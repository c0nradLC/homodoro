{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Task.Task
  ( Task (..),
    TaskListOperation (..),
    TaskListUpdate,
    updateTaskList,
    mkTask,
    taskContent,
    taskCompleted,
  )
where

import Control.Lens (filtered, makeLenses, over, traversed, (%~), (.~))
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T

data Task = Task
  { _taskContent :: T.Text,
    _taskCompleted :: Bool
  }

deriveJSON defaultOptions ''Task
makeLenses ''Task

data TaskListOperation
  = AppendTask Task
  | DeleteTask Task
  | EditTask Task T.Text
  | ChangeTaskCompletion Task

type TaskListUpdate = TaskListOperation -> [Task] -> [Task]

instance Eq Task where
  (==) :: Task -> Task -> Bool
  (Task content1 _) == (Task content2 _) =
    content1 == content2

mkTask :: T.Text -> Maybe Bool -> Task
mkTask txt mb
  | Just b <- mb = Task {_taskContent = txt, _taskCompleted = b}
  | Nothing <- mb = Task {_taskContent = txt, _taskCompleted = False}

updateTaskList :: TaskListOperation -> [Task] -> [Task]
updateTaskList (AppendTask task) = (task :)
updateTaskList (DeleteTask task) = filter (/= task)
updateTaskList (ChangeTaskCompletion targetTask) = over (traversed . filtered (== targetTask)) (taskCompleted %~ not)
updateTaskList (EditTask task newContent) = over (traversed . filtered (== task)) (taskContent .~ newContent)
