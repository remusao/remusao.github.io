module Blog.Serve where


import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static


serve :: (String -> IO ()) -> IO ()
serve logging = do
  logging "Serving HTTP on 0.0.0.0 port 8000..."
  run 8000 (staticApp (defaultFileServerSettings "."))
