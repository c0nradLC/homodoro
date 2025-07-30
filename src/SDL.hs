module SDL (initializeAudio, cleanupAudio, preloadAudio, preloadAllAudio, playAudio) where

import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import qualified SDL.Mixer as Mix
import Types (AudioCache (AudioCache, _audioCacheRef, _isInitialized), Audio (TimerAlert, TimerTick, TimerStartStop))
import qualified Data.Map as Map
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.Directory (listDirectory)
import Data.Char (toLower)
import System.FilePath (takeBaseName, (</>))

initializeAudio :: IO AudioCache
initializeAudio = do
    Mix.initialize [Mix.InitMP3]
    Mix.openAudio defaultAudio 256
    cacheRef <- newIORef Map.empty
    initRef <- newIORef True
    return $ AudioCache cacheRef initRef

cleanupAudio :: AudioCache -> IO ()
cleanupAudio manager = do
    initialized <- readIORef (_isInitialized manager)
    Mix.closeAudio
    Mix.quit
    when initialized $ do
        cache <- readIORef (_audioCacheRef manager)
        mapM_ Mix.free (Map.elems cache)
        writeIORef (_isInitialized manager) False

preloadAllAudio :: AudioCache -> FilePath -> IO ()
preloadAllAudio manager audioDirFp = do
    filesInDir <- listDirectory audioDirFp
    let audioKeys       = map (map toLower . show) [TimerAlert, TimerTick, TimerStartStop]
        foundTimerFiles = filter (\file -> map toLower (takeBaseName file) `elem` audioKeys) filesInDir
    mapM_ (\fp -> 
              let key = map toLower (takeBaseName fp)
                  fullPath = audioDirFp </> fp
              in preloadAudio manager fullPath key
          ) foundTimerFiles

preloadAudio :: AudioCache -> FilePath -> String -> IO ()
preloadAudio manager fp audioKey = do
    initialized <- readIORef (_isInitialized manager)
    when initialized $ do
        cache <- readIORef (_audioCacheRef manager)
        unless (Map.member audioKey cache) $ do
            audioFile <- BS.readFile fp
            audio <- Mix.decode audioFile
            modifyIORef (_audioCacheRef manager) (Map.insert audioKey audio)

playAudio :: AudioCache -> String -> Int -> IO ()
playAudio manager audioKey vol = do
    initialized <- readIORef (_isInitialized manager)
    when initialized $ do
        cache <- readIORef (_audioCacheRef manager)
        case Map.lookup audioKey cache of
            Just audio -> do
                Mix.setVolume vol audio
                _ <- Mix.play audio
                return ()
            Nothing -> return ()

defaultAudio :: Mix.Audio
defaultAudio =
    Mix.Audio
        { Mix.audioFrequency = 44100
        , Mix.audioFormat = Mix.FormatU8
        , Mix.audioOutput = Mix.Stereo
        }
