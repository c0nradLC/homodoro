{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Notify (
    playAudio,
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
import Resources (AppState, Audio (..), Name)
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

timerEndedAudio :: SB.ByteString
timerEndedAudio = $(embedFile "resources/timerEnded.mp3")

startAudio :: SB.ByteString
startAudio = $(embedFile "resources/start_audio.mp3")

stopAudio :: SB.ByteString
stopAudio = $(embedFile "resources/stop_audio.mp3")

playAudio :: Audio -> IO ()
playAudio audio = do
    SDL.initialize [SDL.InitAudio]
    Mix.initialize [Mix.InitMP3]

    case audio of
        TimerEnded -> loadAndPlaySound timerEndedAudio
        Start -> loadAndPlaySound startAudio
        Stop -> loadAndPlaySound stopAudio
        _ -> return ()

    Mix.closeAudio
    Mix.quit

loadAndPlaySound :: SB.ByteString -> IO ()
loadAndPlaySound soundName = do
    withSystemTempFile "tempRingtone" $ \tempPath tempHandle -> do
        SB.hPut tempHandle soundName
        hClose tempHandle
        Mix.openAudio def 256
        audio <- Mix.load tempPath
        Mix.play audio
        whileTrueM $ Mix.playing Mix.AllChannels

        Mix.haltMusic
        Mix.free audio

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
    loop <- cond
    when loop $ whileTrueM cond
