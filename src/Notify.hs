{-# LANGUAGE OverloadedStrings #-}

module Notify (showNotification) where

import DBus
import DBus.Client
import Data.Int
import Data.Map
import Data.Word

showNotification :: String -> String -> IO ()
showNotification summary msg = do
  client <- connectSession
  _ <-
    callNoReply
      client
      $ (methodCall "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "Notify")
        { methodCallDestination = Just "org.freedesktop.Notifications",
          methodCallBody =
            [ toVariant ("homodoro" :: String), -- app_name
              toVariant (0 :: Word32), -- replaces_id
              toVariant ("" :: String), -- app_icon
              toVariant summary, -- summary
              toVariant msg, -- body
              toVariant ([[]] :: [String]), -- actions
              toVariant (empty :: Map String Variant), -- hints
              toVariant (-1 :: Int32) -- expiry (-1 for dependent)
            ]
        }
  return ()
