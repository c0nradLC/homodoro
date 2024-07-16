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
import Control.Lens (filtered, over, traversed, view, (%~), (.~), (?~))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Resources (Task (..), TaskListOperation (..), TaskListUpdate, taskCompleted, taskContent, archivedAt)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Data.Time (getCurrentTime, UTCTime (UTCTime))

mkTask :: T.Text -> Maybe Bool -> IO Task
mkTask txt mb = do
    currentTime <- getCurrentTime
    let UTCTime today _ = currentTime
    case mb of 
        Just b -> do
            return Task{_taskContent = txt, _taskCompleted = b, _createdAt = today, _archivedAt = Nothing}
        Nothing -> do 
            return Task{_taskContent = txt, _taskCompleted = False, _createdAt = today, _archivedAt = Nothing}

updateTaskList :: TaskListUpdate
updateTaskList (AppendTask task) = (task :)
updateTaskList (DeleteTask task) = filter (/= task)
updateTaskList (ChangeTaskCompletion task) = over (traversed . filtered (== task)) (taskCompleted %~ not)
updateTaskList (EditTask task newContent) = over (traversed . filtered (== task)) (taskContent .~ newContent)
updateTaskList (ArchiveTask task currentDay) = over (traversed . filtered (== task)) (archivedAt ?~ currentDay)

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
    let modifiedTaskList = function task tasks
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