{-# LANGUAGE TemplateHaskell #-}

module Notify
  ( alertRoundEnded,
    playAlertSound,
  )
where

import Brick (EventM)
import Control.Monad (when)
import Data.ByteString (ByteString, hPut)
import Data.FileEmbed (embedFileRelative)
import Data.Text (pack)
import qualified GI.Notify as GI
import Sound.ALUT
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Resources (AppState, Name)

alertRoundEnded :: EventM Name AppState ()
alertRoundEnded = do
  notificationServerInitiated <- GI.init $ Just $ pack "homodoro"
  when notificationServerInitiated $ do
    notification <- GI.notificationNew (pack "homodoro") (Just $ pack "Pomodoro round ended!") Nothing
    GI.notificationShow notification

playAlertSound :: IO ()
playAlertSound = do
  withProgNameAndArgs runALUT $ \_ _ -> do
    buf <- getBuffer
    source <- genObjectName
    buffer source $= Just buf
    play [source]

    let waitWhilePlaying = do
          sleep 0.1
          state <- get (sourceState source)
          if state == Playing
            then waitWhilePlaying
            else rewind [source]
    waitWhilePlaying

getBuffer :: IO Buffer
getBuffer = do
  withSystemTempFile "tempRingtone" $ \tempPath tempHandle -> do
    hPut tempHandle audioFile
    hClose tempHandle
    createBuffer (File tempPath)

audioFile :: ByteString
audioFile = $(embedFileRelative "resources/ringtone.wav")
