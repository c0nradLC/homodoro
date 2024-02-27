{-# LANGUAGE OverloadedStrings #-}

module Task.File
  ( createTasksFileIfNotExists,
    getTasksFilePath,
    updateTaskList,
    taskExists,
    getTasks,
    writeTasks,
    TaskListOperation (..)
  )
where

import Control.Lens (filtered, over, traversed, view, (%~), (.~))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Task.Task as TK (Task (..), taskCompleted, taskContent)

data TaskListOperation = AppendTask TK.Task
                       | DeleteTask TK.Task
                       | EditTask TK.Task T.Text
                       | ChangeTaskCompletion TK.Task

type TaskListUpdate = TaskListOperation -> [TK.Task] -> [TK.Task]

createTasksFileIfNotExists :: FilePath -> IO ()
createTasksFileIfNotExists filePath = do
  D.createDirectoryIfMissing True (FP.takeDirectory filePath)
  fileExists <- D.doesFileExist filePath
  unless fileExists $ do BSL.writeFile filePath ""

getTasksFilePath :: IO FilePath
getTasksFilePath = do
  homePath <- D.getXdgDirectory D.XdgData ""
  pure $ homePath FP.</> "homodoro/tasks"

writeTasks :: TaskListUpdate -> TaskListOperation -> IO [TK.Task]
writeTasks function task = do
  tasks <- getTasks
  tasksFilePath <- getTasksFilePath
  case tasks of
    Just fTasks -> do
      let modifiedTaskList = function task fTasks
      BSL.writeFile tasksFilePath $ encode modifiedTaskList
      return modifiedTaskList
    Nothing -> return []

updateTaskList :: TaskListUpdate
updateTaskList (AppendTask task) taskList = taskList ++ [task]
updateTaskList (DeleteTask task) (t : tl)
  | task == t = tl
  | otherwise = t : updateTaskList (DeleteTask task) tl
updateTaskList (ChangeTaskCompletion targetTask) tasks = over (traversed . filtered (== targetTask)) toggleCompletion tasks
  where
    toggleCompletion = TK.taskCompleted %~ not
updateTaskList (EditTask task newContent) taskList = over (traversed . filtered (== task)) editTaskContent taskList
  where editTaskContent = TK.taskContent .~ newContent
updateTaskList _ taskList = taskList

taskExists :: T.Text -> IO Bool
taskExists content = do
  fileTasks <- getTasks
  case fileTasks of
    Just tasks -> do
      let tasksContents = map (view TK.taskContent) tasks
      return $ content `elem` tasksContents
    Nothing -> return False

getTasks :: IO (Maybe [TK.Task])
getTasks = do
  tasksFilePath <- getTasksFilePath
  tasksFromFile <- BSL.readFile tasksFilePath
  case decode tasksFromFile of
    Just text -> do
      return text
    Nothing -> return (Just [])
