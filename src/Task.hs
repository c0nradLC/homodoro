{-# LANGUAGE OverloadedStrings #-}

module Task (
    createTasksFileIfNotExists,
    taskExists,
    getTasks,
    writeTasks,
    mkTask,
    updateTaskList
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
import Data.UUID.V4 (nextRandom)

mkTask :: T.Text -> IO Task
mkTask txt = do
    taskId <- nextRandom
    return Task{_taskId = taskId, _taskContent = txt, _taskCompleted = False}

updateTaskList :: TaskListUpdate
updateTaskList (AppendTask task) = (task :)
updateTaskList (DeleteTask task) = filter (/= task)
updateTaskList (ChangeTaskCompletion task) = over (traversed . filtered (== task)) (taskCompleted %~ not)
updateTaskList (EditTask task newContent) = over (traversed . filtered (== task)) (taskContent .~ newContent)

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- CFG.getTasksFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory filePath)
    fileExists <- D.doesFileExist filePath
    unless fileExists $ do BSL.writeFile filePath ""

writeTasks :: TaskListUpdate -> TaskListOperation -> IO [Task]
writeTasks updateFunction task = do
    tasks <- getTasks
    tasksFilePath <- CFG.getTasksFilePath
    let modifiedTaskList = updateFunction task tasks
    BSL.writeFile tasksFilePath $ encode modifiedTaskList
    return modifiedTaskList

getTasks :: IO [Task]
getTasks = do
    tasksFilePath <- CFG.getTasksFilePath
    tasksFromFile <- BSL.readFile tasksFilePath
    case decode tasksFromFile of
        Just task -> do
            return task
        Nothing -> return []

taskExists :: [Task] -> T.Text -> Bool
taskExists tasks content = do
    let tasksContents = map (view taskContent) tasks
    content `elem` tasksContents