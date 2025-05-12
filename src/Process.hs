module Process (
    getNotificationProvider,
    getAudioProvider,
    callNotificationProvider,
    callAudioProvider,
) where

import Control.Exception (try)
import GHC.Exception.Type (SomeException)
import qualified System.Process
import Types (AudioProvider (FFPlay, MPV), NotificationProvider (NotifySend, Zenity))

getNotificationProvider :: IO (Maybe NotificationProvider)
getNotificationProvider = do
    notifySendProbe <- try (callCommand "notify-send -v") :: IO (Either SomeException ())
    case notifySendProbe of
        Right _ -> return $ Just NotifySend
        Left _ -> do
            zenityProbe <- try (callCommand "zenity -v") :: IO (Either SomeException ())
            case zenityProbe of
                Right _ -> return $ Just Zenity
                Left _ -> return Nothing

getAudioProvider :: IO (Maybe AudioProvider)
getAudioProvider = do
    mpvProbe <- try (callCommand "mpv -v") :: IO (Either SomeException ())
    case mpvProbe of
        Right _ -> return $ Just MPV
        Left _ -> do
            zenity <- try (callCommand "ffplay -v") :: IO (Either SomeException ())
            case zenity of
                Right _ -> return $ Just FFPlay
                Left _ -> return Nothing

callNotificationProvider :: NotificationProvider -> String -> IO ()
callNotificationProvider notificationProvider args =
    callCommand $ show notificationProvider <> " " <> args

callAudioProvider :: AudioProvider -> String -> IO ()
callAudioProvider audioProvider args =
    callCommand $ show audioProvider <> " " <> args

callCommand :: String -> IO ()
callCommand str = System.Process.callCommand $ str <> " > /dev/null"
