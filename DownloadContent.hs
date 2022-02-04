{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import System.IO
import System.Directory
import Data.Strings

-- input url and path 
downloadContent :: String -> String -> IO ()
downloadContent url path = do
  putStrLn $ "status : Downloading to " ++ path
  b <- doesFileExist path
  unless b $ do
    res <- httpBS $ parseRequest_ url
    let xs = getResponseHeader "Content-Type" res
        file = getResponseBody res
        contentType = if not (null xs) then head xs else ""
    store path file contentType
  where
    store :: FilePath -> B.ByteString -> B.ByteString -> IO ()
    store path bs c = do
      if B.isInfixOf "video/" c || B.isInfixOf "image/" c
        then do 
          let picType = BS.unpack $ snd $ B.splitAt (B.length "image/") c
          fin <- openBinaryFile path WriteMode
          hPutStr fin (BS.unpack bs)
          hClose fin
          putStrLn $ "status : Success download to " ++ path
        else putStrLn "not content file"


urlToFileName :: String -> String
urlToFileName s = 
  let xs = strSplitAll "/" s
      ls = if null xs then "error" else last xs
    in ls

eliminate :: String -> String
eliminate s = 
  let xs = strSplitAll "." s
      ls = if null xs then s else head xs
    in ls

storeFromUrl :: String -> String -> IO ()
storeFromUrl tmp url = downloadContent url (tmp <> urlToFileName url)
