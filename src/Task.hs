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
import Data.Aeson (decodeStrict)
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromMaybe)
import Data.Text (Text, drop, dropWhile, dropWhileEnd, isPrefixOf, lines, unlines)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text.Lazy as TL (toStrict)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, takeExtension)
import Types (Task (..), TaskListOperation (..), taskCompleted, taskContent)
import Prelude hiding (drop, dropWhile, lines, take, takeWhile, unlines)

createTasksFileIfNotExists :: IO ()
createTasksFileIfNotExists = do
    filePath <- readTasksFilePath
    createDirectoryIfMissing True (takeDirectory filePath)
    fileExists <- doesFileExist filePath
    unless fileExists $ do writeFile filePath ""

mkTask :: Text -> Maybe Bool -> Task
mkTask txt completed = Task{_taskContent = txt, _taskCompleted = fromMaybe False completed}

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
    tasksFilePathSetting <- readTasksFilePath
    case takeExtension tasksFilePathSetting of
        ".json" -> TIO.writeFile tasksFilePathSetting $ TL.toStrict $ encodeToLazyText tasks
        ".md" -> TIO.writeFile tasksFilePathSetting $ encodeMarkdownTasks tasks
        _ -> return ()
    return tasks

readTasks :: IO [Task]
readTasks = do
    tasksFilePathSetting <- readTasksFilePath
    case takeExtension tasksFilePathSetting of
        ".json" -> do
            tasksFromFile <- TIO.readFile tasksFilePathSetting
            case decodeStrict $ encodeUtf8 tasksFromFile of
                Just tasks -> return tasks
                Nothing -> return []
        ".md" -> do
            tasksFromFile <- TIO.readFile tasksFilePathSetting
            return $ decodeMarkdownTasks tasksFromFile
        _ -> return []

encodeMarkdownTasks :: [Task] -> Text
encodeMarkdownTasks tasks = unlines $ map encodeMarkdownTask tasks

encodeMarkdownTask :: Task -> Text
encodeMarkdownTask task =
    let taskCompletedMarkdownString = if task ^. taskCompleted then "- [x]" else "- [ ]"
     in taskCompletedMarkdownString <> " " <> (task ^. taskContent)

decodeMarkdownTasks :: Text -> [Task]
decodeMarkdownTasks tasksFromFile =
    let taskLinesInFile = filter isCheckListItem $ lines tasksFromFile
     in map decodeMarkdownTask taskLinesInFile

decodeMarkdownTask :: Text -> Task
decodeMarkdownTask taskFileLine =
    let parsedTaskContent = stripLine $ drop 1 $ dropWhile (/= ']') taskFileLine
        parsedTaskCompleted = if dropWhileEnd (/= ']') taskFileLine `elem` checkedCheckListString then Just True else Just False
     in mkTask parsedTaskContent parsedTaskCompleted

stripLine :: Text -> Text
stripLine = dropWhile (== ' ') . dropWhileEnd (== ' ')

isCheckListItem :: Text -> Bool
isCheckListItem taskLineContent = any (`isPrefixOf` taskLineContent) checkListStrings

checkListStrings :: [Text]
checkListStrings =
    uncheckedCheckListString ++ checkedCheckListString

checkedCheckListString :: [Text]
checkedCheckListString =
    [ "- [x]"
    , "-[x]"
    ]

uncheckedCheckListString :: [Text]
uncheckedCheckListString =
    [ "- []"
    , "-[]"
    , "-[ ]"
    , "- [ ]"
    ]

taskExists :: Text -> IO Bool
taskExists content = do
    tasks <- readTasks
    let tasksContents = map (view taskContent) tasks
    return $ content `elem` tasksContents
