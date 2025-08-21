module SDL (initializeAudio, cleanupAudio, closeSDL, preloadAudio, preloadAllAudio, playAudio) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified SDL.Mixer as Mix
import Types (AudioCache (AudioCache, _audioCacheRef), Audio (TimerAlert, TimerTick, TimerStartStop))
import qualified Data.Map as Map
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import System.Directory (listDirectory)
import Data.Char (toLower)
import System.FilePath (takeBaseName, (</>))

initializeAudio :: IO AudioCache
initializeAudio = do
    Mix.initialize [Mix.InitMP3]
    Mix.openAudio defaultAudio 256
    cacheRef <- newIORef Map.empty
    return $ AudioCache cacheRef

cleanupAudio :: AudioCache -> IO ()
cleanupAudio manager = do
    cache <- readIORef (_audioCacheRef manager)
    mapM_ Mix.free (Map.elems cache)
    writeIORef (_audioCacheRef manager) Map.empty

closeSDL :: IO ()
closeSDL = do
    Mix.closeAudio
    Mix.quit

preloadAllAudio :: AudioCache -> FilePath -> IO [(Audio, FilePath)]
preloadAllAudio manager audioDirFp = do
    filesInDir <- listDirectory audioDirFp
    let audioTypes = [TimerAlert, TimerTick, TimerStartStop]
        foundAudioFiles = [(audio, file) | audio <- audioTypes, 
                                          file <- filesInDir,
                                          map toLower (takeBaseName file) == map toLower (show audio)]
    mapM (\(audio, fp) -> 
              preloadAudio manager (audioDirFp </> fp) audio
          ) foundAudioFiles

preloadAudio :: AudioCache -> FilePath -> Audio -> IO (Audio, FilePath)
preloadAudio manager fp audio = do
    cache <- readIORef (_audioCacheRef manager)
    unless (Map.member audio cache) $ do
        audioFile <- BS.readFile fp
        audioContent <- Mix.decode audioFile
        modifyIORef (_audioCacheRef manager) (Map.insert audio audioContent)
    pure (audio, fp)

playAudio :: AudioCache -> Audio -> Int -> IO ()
playAudio manager audio vol = do
    cache <- readIORef (_audioCacheRef manager)
    case Map.lookup audio cache of
        Just audioF -> do
            Mix.setVolume vol audioF
            _ <- Mix.play audioF
            return ()
        Nothing -> return ()

defaultAudio :: Mix.Audio
defaultAudio =
    Mix.Audio
        { Mix.audioFrequency = 44100
        , Mix.audioFormat = Mix.FormatU8
        , Mix.audioOutput = Mix.Stereo
        }
