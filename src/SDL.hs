{-# LANGUAGE TemplateHaskell #-}
module SDL (initializeAudio, cleanupAudio, closeSDL, preloadAudio, preloadAllAudio, playAudio) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import qualified SDL.Mixer as Mix
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Types (Audio (TimerAlert, TimerStartStop, TimerTick), AudioCache (AudioCache, _audioCacheRef))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List ((\\))

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

fallbackAudioFiles :: [(FilePath, ByteString)]
fallbackAudioFiles = $(embedDir "audio")

getFallbackAudio :: Audio -> Maybe ByteString
getFallbackAudio audio = 
    listToMaybe 
        [ bytes 
        | (path, bytes) <- fallbackAudioFiles
        , map toLower (takeBaseName path) == map toLower (show audio)
        ]

preloadAllAudio :: AudioCache -> FilePath -> IO [Audio]
preloadAllAudio manager audioDirFp = do
    filesInDir <- listDirectory audioDirFp
    let audioTypes = [TimerAlert, TimerTick, TimerStartStop]
        foundAudioFiles =
            [ (audio, file) | audio <- audioTypes, file <- filesInDir, map toLower (takeBaseName file) == map toLower (show audio)
            ]
        missingAudioTypes = audioTypes \\ map fst foundAudioFiles
    audioFromDir <- mapM
        ( \(audio, fp) -> do
            audioFile <- BS.readFile $ audioDirFp </> fp
            preloadAudio manager audioFile audio
        )
        foundAudioFiles
    fallbackAudio <- mapM
        (\audio -> do
            case getFallbackAudio audio of
                Just fallbackBytes -> do
                    loadedAudio <- preloadAudio manager fallbackBytes audio
                    return $ Just loadedAudio
                Nothing -> return Nothing
        )
        missingAudioTypes
    return $ audioFromDir ++ catMaybes fallbackAudio

preloadAudio :: AudioCache -> ByteString -> Audio -> IO Audio
preloadAudio manager audioFile audio = do
    cache <- readIORef (_audioCacheRef manager)
    unless (Map.member audio cache) $ do
        audioContent <- Mix.decode audioFile
        modifyIORef (_audioCacheRef manager) (Map.insert audio audioContent)
    return audio

playAudio :: AudioCache -> Audio -> Int -> IO ()
playAudio manager audio vol = do
    cache <- readIORef (_audioCacheRef manager)
    case Map.lookup audio cache of
        Just audioF -> do
            Mix.setVolume vol audioF
            _ <- Mix.playLimit (
                case audio of             -- Set the play time of the Chunk in Milliseconds
                    TimerTick -> 1000
                    TimerAlert -> 10000
                    TimerStartStop -> 3000
                ) Mix.AllChannels 1 audioF
            return ()
        Nothing -> return ()

defaultAudio :: Mix.Audio
defaultAudio =
    Mix.Audio
        { Mix.audioFrequency = 44100
        , Mix.audioFormat = Mix.FormatU8
        , Mix.audioOutput = Mix.Stereo
        }
