{-# LANGUAGE OverloadedStrings #-}

module Task (
    createTasksFileIfNotExists,
    taskExists,
    getTasks,
    writeTasks,
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

mkTask :: T.Text -> Maybe Bool -> Task
mkTask txt mb
    | Just b <- mb = Task{_taskContent = txt, _taskCompleted = b}
    | Nothing <- mb = Task{_taskContent = txt, _taskCompleted = False}

updateTaskList :: TaskListUpdate
updateTaskList (AppendTask task) = (task :)
updateTaskList (DeleteTask task) = filter (/= task)
updateTaskList (ChangeTaskCompletion targetTask) = over (traversed . filtered (== targetTask)) (taskCompleted %~ not)
updateTaskList (EditTask task newContent) = over (traversed . filtered (== task)) (taskContent .~ newContent)

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- CFG.getTasksFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory filePath)
    fileExists <- D.doesFileExist filePath
    unless fileExists $ do BSL.writeFile filePath ""

writeTasks :: TaskListUpdate -> TaskListOperation -> IO [Task]
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
            let tasksContents = map (view taskContent) tasks
            return $ content `elem` tasksContents
        Nothing -> return False

getTasks :: IO (Maybe [Task])
getTasks = do
    tasksFilePath <- CFG.getTasksFilePath
    tasksFromFile <- BSL.readFile tasksFilePath
    case decode tasksFromFile of
        Just task -> do
            return task
        Nothing -> return (Just [])
