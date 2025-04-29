{-# LANGUAGE OverloadedStrings #-}

module Task (
    createTasksFileIfNotExists,
    taskExists,
    readTasks,
    mkTask,
    updateTaskList,
)
where

import Config (readTasksFilePath)
import Control.Lens (filtered, over, traversed, view, (%~), (.~), (^.))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.ByteString.Char8 (ByteString, drop, dropWhile, dropWhileEnd, isPrefixOf, lines, pack, readFile, unlines, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (readFile, unpack)
import Data.Maybe (fromMaybe)
import Resources (Task (..), TaskListOperation (..), taskCompleted, taskContent)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, takeExtension)
import Prelude hiding (drop, dropWhile, lines, readFile, take, takeWhile, unlines)

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- readTasksFilePath
    createDirectoryIfMissing True (takeDirectory filePath)
    fileExists <- doesFileExist filePath
    unless fileExists $ do writeFile filePath ""

mkTask :: String -> Maybe Bool -> Task
mkTask str completed = Task{_taskContent = str, _taskCompleted = fromMaybe False completed}

updateTaskList :: TaskListOperation -> IO [Task]
updateTaskList tlop = do
    tasks <- readTasks
    case tlop of
        (AppendTask task) -> writeTasks (task : tasks)
        (DeleteTask task) -> writeTasks $ filter (/= task) tasks
        (ChangeTaskCompletion task) -> writeTasks $ over (traversed . filtered (== task)) (taskCompleted %~ not) tasks
        (EditTask task newContent) -> writeTasks $ over (traversed . filtered (== task)) (taskContent .~ newContent) tasks

writeTasks :: [Task] -> IO [Task]
writeTasks tasks = do
    tasksFilePath <- readTasksFilePath
    case takeExtension tasksFilePath of
        ".json" -> writeFile tasksFilePath $ BSL.unpack $ encode tasks
        ".md" -> writeFile tasksFilePath $ unpack $ encodeMarkdownTasks tasks
        _ -> return ()
    return tasks

readTasks :: IO [Task]
readTasks = do
    tasksFilePath <- readTasksFilePath
    case takeExtension tasksFilePath of
        ".json" -> do
            tasksFromFile <- BSL.readFile tasksFilePath
            case decode tasksFromFile of
                Just tasks -> return tasks
                Nothing -> return []
        ".md" -> do
            tasksFromFile <- readFile tasksFilePath
            return $ decodeMarkdownTasks tasksFromFile
        _ -> return []

encodeMarkdownTasks :: [Task] -> ByteString
encodeMarkdownTasks tasks = unlines $ map encodeMarkdownTask tasks

encodeMarkdownTask :: Task -> ByteString
encodeMarkdownTask task =
    let taskCompletedMarkdownString = (if task ^. taskCompleted then "- [x]" else "- [ ]")
     in pack $ taskCompletedMarkdownString ++ " " ++ (task ^. taskContent)

decodeMarkdownTasks :: ByteString -> [Task]
decodeMarkdownTasks tasksFromFile =
    let taskLinesInFile = filter isCheckListItem $ lines tasksFromFile
     in map decodeMarkdownTask taskLinesInFile

decodeMarkdownTask :: ByteString -> Task
decodeMarkdownTask taskFileLine =
    let parsedTaskContent = unpack $ stripLine $ drop 1 $ dropWhile (/= ']') taskFileLine
        parsedTaskCompleted = if dropWhileEnd (/= ']') taskFileLine `elem` checkedCheckListString then Just True else Just False
     in mkTask parsedTaskContent parsedTaskCompleted

stripLine :: ByteString -> ByteString
stripLine = dropWhile (== ' ') . dropWhileEnd (== ' ')

isCheckListItem :: ByteString -> Bool
isCheckListItem taskLineContent = any (`isPrefixOf` taskLineContent) checkListStrings

checkListStrings :: [ByteString]
checkListStrings =
    uncheckedCheckListString ++ checkedCheckListString

checkedCheckListString :: [ByteString]
checkedCheckListString =
    [ "- [x]"
    , "-[x]"
    ]

uncheckedCheckListString :: [ByteString]
uncheckedCheckListString =
    [ "- []"
    , "-[]"
    , "-[ ]"
    , "- [ ]"
    ]

taskExists :: String -> IO Bool
taskExists content = do
    tasks <- readTasks
    let tasksContents = map (view taskContent) tasks
    return $ content `elem` tasksContents
