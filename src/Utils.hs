module Utils (getLines) where

getLines :: String -> IO [String]
getLines fname = do
  str <- readFile fname
  return $ lines str
