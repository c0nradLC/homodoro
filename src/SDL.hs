module SDL (playAudio) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified SDL.Mixer as Mix

playAudio :: FilePath -> Int -> IO ()
playAudio fp vol = do
    Mix.initialize [Mix.InitMP3]

    Mix.openAudio defaultAudio 256
    audioFile <- BS.readFile fp
    audio <- Mix.decode audioFile
    Mix.setVolume vol audio
    Mix.play audio
    whileTrueM $ Mix.playing Mix.AllChannels
    Mix.haltMusic

    Mix.free audio
    Mix.closeAudio

    Mix.quit

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
