{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Task.Task
  ( Task (..),
    mkTask,
    taskContent,
    taskCompleted,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T

data Task = Task
  { _taskContent :: T.Text,
    _taskCompleted :: Bool
  }

deriveJSON defaultOptions ''Task
makeLenses ''Task

instance Eq Task where
  (==) :: Task -> Task -> Bool
  (Task content1 _) == (Task content2 _) =
    content1 == content2

mkTask :: T.Text -> Maybe Bool -> Task
mkTask txt mb
  | Just b <- mb = Task {_taskContent = txt, _taskCompleted = b}
  | Nothing <- mb = Task {_taskContent = txt, _taskCompleted = False}
