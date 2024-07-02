{-# LANGUAGE OverloadedStrings #-}

module Task.File (
    createTasksFileIfNotExists,
    taskExists,
    getTasks,
    writeTasks,
)
where

import Control.Lens (view)
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Task.Task as TK (Task (..), TaskListOperation, TaskListUpdate, taskContent)
import qualified Config as CFG

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- CFG.getTasksFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory filePath)
    fileExists <- D.doesFileExist filePath
    unless fileExists $ do BSL.writeFile filePath ""

writeTasks :: TK.TaskListUpdate -> TK.TaskListOperation -> IO [TK.Task]
writeTasks function task = do
    tasks <- getTasks
    tasksFilePath <- CFG.getTasksFilePath
    case tasks of
        Just fTasks -> do
            let modifiedTaskList = function task fTasks
            BSL.writeFile tasksFilePath $ encode modifiedTaskList
            return modifiedTaskList
        Nothing -> return []

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
    tasksFilePath <- CFG.getTasksFilePath
    tasksFromFile <- BSL.readFile tasksFilePath
    case decode tasksFromFile of
        Just task -> do
            return task
        Nothing -> return (Just [])
