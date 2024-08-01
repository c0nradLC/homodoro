{-# LANGUAGE OverloadedStrings #-}

module Task
  ( createTasksFileIfNotExists,
    taskExists,
    getTasks,
    mkTask,
    updateTaskList,
  )
where

import qualified Config as CFG
import Control.Lens (filtered, over, traversed, view, (%~), (.~))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Resources (Task (..), TaskListOperation (..), TaskListUpdate, taskCompleted, taskContent)
import qualified System.Directory as D
import qualified System.FilePath as FP

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
  filePath <- CFG.getTasksFilePath
  D.createDirectoryIfMissing True (FP.takeDirectory filePath)
  fileExists <- D.doesFileExist filePath
  unless fileExists $ do BSL.writeFile filePath ""

mkTask :: T.Text -> Task
mkTask txt = Task {_taskContent = txt, _taskCompleted = False}

updateTaskList :: TaskListUpdate
updateTaskList cop = do
  tasks <- getTasks
  case cop of
    (AppendTask task) -> writeTasks (task : tasks)
    (DeleteTask task) -> writeTasks $ filter (/= task) tasks
    (ChangeTaskCompletion task) -> writeTasks $ over (traversed . filtered (== task)) (taskCompleted %~ not) tasks
    (EditTask task newContent) -> writeTasks $ over (traversed . filtered (== task)) (taskContent .~ newContent) tasks

writeTasks :: [Task] -> IO [Task]
writeTasks tasks = do
  tasksFilePath <- CFG.getTasksFilePath
  BSL.writeFile tasksFilePath $ encode tasks
  return tasks

getTasks :: IO [Task]
getTasks = do
  tasksFilePath <- CFG.getTasksFilePath
  tasksFromFile <- BSL.readFile tasksFilePath
  case decode tasksFromFile of
    Just task -> do
      return task
    Nothing -> return []

taskExists :: T.Text -> IO Bool
taskExists content = do
  tasks <- getTasks
  let tasksContents = map (view taskContent) tasks
  return $ content `elem` tasksContents