{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Notify
  ( alertRoundEnded,
    playAlertSound,
  )
where

import Brick (EventM)
import Control.Monad (when)
import qualified Data.ByteString as SB (ByteString, hPut)
import Data.Default.Class
import Data.FileEmbed (embedFileRelative)
import Data.Text (pack)
import qualified GI.Notify as GI
import Resources (AppState, Name)
import qualified SDL
import qualified SDL.Mixer as Mix
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

alertRoundEnded :: String -> EventM Name AppState ()
alertRoundEnded msg = do
  notificationServerInitiated <- GI.init $ Just $ pack "homodoro"
  when notificationServerInitiated $ do
    notification <- GI.notificationNew (pack "homodoro") (Just $ pack msg) Nothing
    GI.notificationShow notification

audioFile :: SB.ByteString
audioFile = $(embedFileRelative "resources/ringtone.mp3")

playAlertSound :: IO ()
playAlertSound = do
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  withSystemTempFile "tempRingtone" $ \tempPath tempHandle -> do
    SB.hPut tempHandle audioFile
    let audioFileTest = tempPath
    hClose tempHandle

    Mix.openAudio def 256
    sound <- Mix.load audioFileTest
    Mix.play sound
    whileTrueM $ Mix.playing Mix.AllChannels

    Mix.free sound

  Mix.closeAudio

  Mix.quit
  SDL.quit

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond
