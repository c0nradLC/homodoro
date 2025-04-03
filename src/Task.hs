{-# LANGUAGE OverloadedStrings #-}

module Task (
    createTasksFileIfNotExists,
    taskExists,
    readTasks,
    mkTask,
    updateTaskList,
)
where

import Config
import Control.Lens (filtered, over, traversed, view, (%~), (.~))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Resources (Task (..), TaskListOperation (..), taskCompleted, taskContent)
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( takeDirectory )
import Data.ByteString.Lazy.Char8 (unpack, pack)

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- readTasksFilePath
    createDirectoryIfMissing True (takeDirectory filePath)
    fileExists <- doesFileExist filePath
    unless fileExists $ do writeFile filePath ""

mkTask :: String -> Task
mkTask str = Task{_taskContent = str, _taskCompleted = False}

updateTaskList :: TaskListOperation -> IO [Task]
updateTaskList cop = do
    tasks <- readTasks
    case cop of
        (AppendTask task) -> writeTasks (task : tasks)
        (DeleteTask task) -> writeTasks $ filter (/= task) tasks
        (ChangeTaskCompletion task) -> writeTasks $ over (traversed . filtered (== task)) (taskCompleted %~ not) tasks
        (EditTask task newContent) -> writeTasks $ over (traversed . filtered (== task)) (taskContent .~ newContent) tasks

writeTasks :: [Task] -> IO [Task]
writeTasks tasks = do
    tasksFilePath <- readTasksFilePath
    writeFile tasksFilePath $ unpack $ encode tasks
    return tasks

readTasks :: IO [Task]
readTasks = do
    tasksFilePath <- readTasksFilePath
    tasksFromFile <- readFile tasksFilePath
    case decode $ pack tasksFromFile of
        Just task -> do
            return task
        Nothing -> return []

taskExists :: String -> IO Bool
taskExists content = do
    tasks <- readTasks
    let tasksContents = map (view taskContent) tasks
    return $ content `elem` tasksContents
