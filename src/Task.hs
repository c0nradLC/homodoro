{-# LANGUAGE OverloadedStrings #-}

module Task (
    taskExists,
    readTasks,
    mkTask,
    updateTaskList,
    writeTasks,
)
where

import Control.Lens (filtered, over, traversed, view, (%~), (.~), (^.))
import Data.Aeson (decodeStrict)
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromMaybe)
import Data.Text (Text, drop, dropWhile, dropWhileEnd, isPrefixOf, lines, unlines, isInfixOf, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text.Lazy as TL (toStrict)
import System.FilePath (takeExtension)
import Types (Task (..), TaskListOperation (..), taskCompleted, taskContent)
import Prelude hiding (drop, dropWhile, lines, take, takeWhile, unlines)
import Control.Exception (try, SomeException (..))
import System.Exit ( exitFailure )
import Control.Monad (when)

mkTask :: Text -> Maybe Bool -> Task
mkTask txt completed = Task{_taskContent = txt, _taskCompleted = fromMaybe False completed}

updateTaskList :: [Task] -> TaskListOperation -> [Task]
updateTaskList tasks tlop =
    case tlop of
        (AppendTask task) -> task : tasks
        (DeleteTask task) -> filter (/= task) tasks
        (ChangeTaskCompletion task) -> over (traversed . filtered (== task)) (taskCompleted %~ not) tasks
        (EditTask task newContent) -> over (traversed . filtered (== task)) (taskContent .~ newContent) tasks

taskExists :: [Task] -> Text -> Bool
taskExists tasks content =
    any ((== content) . view taskContent) tasks

writeTasks :: FilePath -> [Task] -> IO [Task]
writeTasks fp tasks = do
    case takeExtension fp of
        ".json" -> TIO.writeFile fp $ TL.toStrict $ encodeToLazyText tasks
        ".md" -> TIO.writeFile fp $ encodeMarkdownTasks tasks
        _ -> return ()
    return tasks

readTasks :: FilePath -> IO [Task]
readTasks fp = do
    taskFileContent <- readTaskFile fp
    case takeExtension fp of
        ".json" -> case decodeStrict $ encodeUtf8 taskFileContent  of
                Just tasks -> return tasks
                Nothing -> return []
        ".md" -> return $ decodeMarkdownTasks taskFileContent  
        _ -> return []

readTaskFile :: FilePath -> IO Text
readTaskFile fp = do
    readAction <- try (TIO.readFile fp) :: IO (Either SomeException Text)
    case readAction of
        Left ex-> do
            when ("invalid byte sequence" `isInfixOf` pack (show ex)) $ do
                putStrLn "An error occured while decoding the tasks file content, this may happen when any of the tasks has unicode characters and \
                \none of the Locale environment variables are set."
                putStrLn "Run any of the following commands to set a Locale environment variable to one that \
                \handles UTF8:"
                putStrLn "export LC_ALL=C.UTF8"
                putStrLn "export LANG=C.UTF8"
                putStrLn "export LC_CTYPE=C.UTF8"
            exitFailure
        Right text -> return text

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
