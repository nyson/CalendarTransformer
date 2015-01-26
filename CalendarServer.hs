{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import CalendarTransformer 
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text.Lazy as TL



calendar :: Transformer -> String -> IO (Either TL.Text TL.Text)
calendar t url = do
  req <- open url
  case req of
    Left error -> return $ Left $ TL.pack
                  $ mconcat ["<h1>Error: ", error, "</h1>", "<br /> <h2>", url, "</h2>"]
    Right cal  -> return $ Right $ calPrint $ transform cal t

main = scotty 3000 $ do
  get "/" $ do
    url <- (param "url") `rescue` (const next)
    
    cal <- liftIO $ calendar def url
    
    case cal of
      Left error -> html$ error
      Right cal  -> text$ cal 

  get "/" $ do
    html $ mconcat ["<h1>Hello!</h1><p>To start, simply write an url in the "
                   ,"field below!</p>"
                   ,"<form method='get' action='./'>"
                   ,"<input type='text' name='url'/ >"
                   ,"<input type='submit' value='Go!'>"
                   ,"</form>"]
    
