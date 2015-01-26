{-# LANGUAGE OverloadedStrings #-}

module CalTransformer where

import Network.Curl.Download.Lazy

import Text.ICalendar.Parser
import Text.ICalendar.Printer
import Text.ICalendar.Types

import Data.Default
import Data.List.Split
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Traversable
import qualified Data.Map as Map

-- Major functions -----------------------------------------------------------

calPrint :: VCalendar -> TL.Text
calPrint cal = decodeUtf8 $ printICalendar def cal

-- Downloads a iCalendar file by curl and applies transformation functions
--  on it
transform :: VCalendar -> Transformer -> VCalendar
transform calendar trans = transformCalendar trans calendar

open :: String -> IO (Either String VCalendar)
open uri = do
  req <- openLazyURI uri
  case req of 
    Left error -> return $ Left error
    Right doc  -> do
      -- not really sure why we need a FilePath here...
      case parseICalendar def "error.out" doc of
        Left error     -> return $ Left error
        Right (cals,_) -> return $ Right $  head cals

-- Transformer Declarations --------------------------------------------------
data Transformer = T { eventT :: (VEvent -> VEvent)
                     , eventF :: (VEvent -> Bool)
                       
                     , todoT :: (VTodo -> VTodo)
                     , todoF :: (VTodo -> Bool)
                       
                     , journalT :: (VJournal -> VJournal)
                     , journalF :: (VJournal -> Bool)

                     , calendarT :: (VCalendar -> VCalendar)}

instance Default Transformer where
  def = T { eventT    = id, eventF   = ft
          , todoT     = id, todoF    = ft
          , journalT  = id, journalF = ft
          , calendarT = id}
    where ft = \_ -> True

transformCalendars :: Transformer -> [VCalendar] -> [VCalendar]
transformCalendars t = map (transformCalendar t) 

transformCalendar :: Transformer -> VCalendar -> VCalendar
transformCalendar t cal 
  = (calendarT t) cal { vcEvents   = mf eventT   eventF   vcEvents
                      , vcTodos    = mf todoT    todoF    vcTodos
                      , vcJournals = mf journalT journalF vcJournals
                      }
    where mf tf ff mp = Map.map (tf t) $ Map.filter (ff t) $ (mp cal)
transformEvent :: Transformer -> VEvent -> VEvent
transformEvent t = (eventT t)

transformTodo :: Transformer -> VTodo -> VTodo
transformTodo t = (todoT t)

transformJournal :: Transformer -> VJournal -> VJournal
transformJournal t = (journalT t)

-- Transformer functions -----------------------------------------------------
