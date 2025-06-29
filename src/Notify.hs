module Notify (showNotification)
where
import Libnotify.C.NotifyNotification
    ( Timeout(Custom),
      notify_notification_new,
      notify_notification_set_timeout,
      notify_notification_show )
import Control.Monad (when)
import Libnotify.C.Notify (notify_is_initted, notify_init)

showNotification :: String -> IO ()
showNotification msg = do
    _ <- notify_init "homodoro"
    notifyInitted <- notify_is_initted
    when notifyInitted $ do
        notification <- notify_notification_new "homodoro" msg ""
        notify_notification_set_timeout notification (Custom 5000)
        _ <- notify_notification_show notification
        return ()