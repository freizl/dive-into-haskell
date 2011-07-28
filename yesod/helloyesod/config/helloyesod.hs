{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withHelloyesod)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withHelloyesod $ run 3000
#else
import Controller (withHelloyesod)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withHelloyesod $ run port . debug
#endif
