{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Notify (
    playAlertSound,
    alertRoundEnded,
)
where

import Brick (EventM)
import Control.Monad (when)
import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.ByteString as SB (ByteString, hPut)
import Data.Default.Class
import Data.FileEmbed (embedFile)
import qualified Libnotify.C.Notify as LN
import Libnotify.C.NotifyNotification (Timeout (..), notify_notification_set_timeout, notify_notification_show)
import qualified Libnotify.C.NotifyNotification as LN
import Resources (AppState, Name)
import qualified SDL
import qualified SDL.Mixer as Mix
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

alertRoundEnded :: String -> EventM Name AppState ()
alertRoundEnded msg = liftIO $ do
    _ <- LN.notify_init "homodoro"
    notifyInitted <- LN.notify_is_initted
    when notifyInitted $ do
        notification <- LN.notify_notification_new "homodoro" msg ""
        notify_notification_set_timeout notification (Custom 5000)
        _ <- notify_notification_show notification
        return ()

audioFile :: SB.ByteString
audioFile = $(embedFile "resources/ringtone.mp3")

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
