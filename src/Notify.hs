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
import qualified Data.ByteString as SB (ByteString)
import Data.FileEmbed (embedFile)
import qualified Libnotify.C.Notify as LN
import Libnotify.C.NotifyNotification (Timeout (..), notify_notification_set_timeout, notify_notification_show)
import qualified Libnotify.C.NotifyNotification as LN
import Resources (AppState, Audio (..), Name)
import qualified SDL
import qualified SDL.Mixer as Mix

alertRoundEnded :: String -> EventM Name AppState ()
alertRoundEnded msg = liftIO $ do
    _ <- LN.notify_init "homodoro"
    notifyInitted <- LN.notify_is_initted
    when notifyInitted $ do
        popup <- LN.notify_notification_new "homodoro" msg ""
        notify_notification_set_timeout popup (Custom 5000)
        _ <- notify_notification_show popup
        return ()

timerEndedAudio :: SB.ByteString
timerEndedAudio = $(embedFile "audio/timerEnded.mp3")

startAudio :: SB.ByteString
startAudio = $(embedFile "audio/start_audio.mp3")

stopAudio :: SB.ByteString
stopAudio = $(embedFile "audio/stop_audio.mp3")

timerTickAudio :: SB.ByteString
timerTickAudio = $(embedFile "audio/timerTick.mp3")

playAudio :: Audio -> Int -> IO ()
playAudio audio vol = do
    SDL.initialize [SDL.InitAudio]
    Mix.initialize [Mix.InitMP3]

    case audio of
        TimerTick -> loadAndPlaySound timerTickAudio vol
        TimerEnded -> loadAndPlaySound timerEndedAudio vol
        Start -> loadAndPlaySound startAudio vol
        Stop -> loadAndPlaySound stopAudio vol
        _ -> return ()

    Mix.closeAudio
    Mix.quit

loadAndPlaySound :: SB.ByteString -> Int -> IO ()
loadAndPlaySound soundName vol = do
    Mix.openAudio defaultAudio 256
    audio <- Mix.decode soundName
    Mix.setVolume vol audio
    Mix.play audio
    whileTrueM $ Mix.playing Mix.AllChannels

    Mix.haltMusic
    Mix.free audio

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
    loop <- cond
    when loop $ whileTrueM cond

defaultAudio :: Mix.Audio
defaultAudio =
    Mix.Audio
        { Mix.audioFrequency = 44100
        , Mix.audioFormat = Mix.FormatU8
        , Mix.audioOutput = Mix.Stereo
        }
