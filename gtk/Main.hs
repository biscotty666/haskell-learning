import qualified GI.Gtk as Gtk

main = do
    Just app <- Gtk.applicationNew (Just appId) []
    _ <- Gio.onApplicationActivate app (appActivate app)
    _ <- Gio.applicationRun app Nothing
    return ()

appId = Text.pack "biscott666.gui-haskell-app"


appActivate :: Gtk.Application -> IO ()
appActivate app = do
    window <- Gtk.applicationWindowNew app
    Gtk.widgetShowAll window
