{-# LANGUAGE OverloadedStrings #-}

module Notify 
  ( NotificationManager
  , newNotificationManager
  , showNotification
  , cleanupNotificationManager
  ) where

import DBus
import DBus.Client
import Data.Int
import Data.Map
import Data.Word
import Control.Concurrent.MVar

newtype NotificationManager = NotificationManager 
  { nmClient :: MVar Client
  }

newNotificationManager :: IO NotificationManager
newNotificationManager = do
  client <- connectSession
  mvar <- newMVar client
  return $ NotificationManager mvar

showNotification :: NotificationManager -> String -> String -> IO ()
showNotification manager summary msg = do
  client <- takeMVar (nmClient manager)
  let buscall = (methodCall "/org/freedesktop/Notifications" 
                        "org.freedesktop.Notifications" 
                        "Notify")
        { methodCallDestination = Just "org.freedesktop.Notifications",
          methodCallBody =
            [ toVariant ("homodoro" :: String),
              toVariant (0 :: Word32),
              toVariant ("" :: String),
              toVariant summary,
              toVariant msg,
              toVariant ([[]] :: [String]),
              toVariant (empty :: Map String Variant),
              toVariant (-1 :: Int32)
            ]
        }
  result <- callNoReply client buscall
  putMVar (nmClient manager) client
  return result

cleanupNotificationManager :: NotificationManager -> IO ()
cleanupNotificationManager manager = do
  client <- takeMVar (nmClient manager)
  disconnect client
  putMVar (nmClient manager) client
