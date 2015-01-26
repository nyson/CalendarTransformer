module CalTransformer where

import Network.Curl.Download.Lazy

import Text.ICalendar.Parser
import Text.ICalendar.Printer
import Text.ICalendar.Types

import Data.Default
import Data.List.Split
import qualified Data.Text.Lazy as TL
import Data.Traversable
import qualified Data.Map as Map


-- Transforms a 
transform :: String -> Transformer -> IO [VCalendar]
transform uri trans = do
  parse <- open uri
  case parse of
    Left error -> do
      putStrLn error
      return []
    Right cals -> do
      return $ transformCalendars trans cals

open :: String -> IO (Either String [VCalendar])
open uri = do
  req <- openLazyURI uri
  case req of
    Left error -> return $ Left error
    Right doc  -> do
      case parseICalendar def "error.out" doc of
        Left error    -> return $ Left error
        Right (cal,_) -> return $ Right cal

-- Transformer Declarations --------------------------------------------------
data Transformer = T { eventT :: (VEvent -> VEvent)
                     , eventF :: (VEvent -> Bool)
                       
                     , todoT :: (VTodo -> VTodo)
                     , todoF :: (VTodo -> Bool)
                       
                     , journalT :: (VJournal -> VJournal)
                     , journalF :: (VJournal -> Bool)}

instance Default Transformer where
  def = T { eventT   = id, eventF   = ft
          , todoT    = id, todoF    = ft
          , journalT = id, journalF = ft}
    where ft = \_ -> True

transformCalendars :: Transformer -> [VCalendar] -> [VCalendar]
transformCalendars t = map (transformCalendar t) 

transformCalendar :: Transformer -> VCalendar -> VCalendar
transformCalendar t cal 
  = cal { vcEvents   = mf eventT   eventF   vcEvents
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



-- findNth :: TL.Text -> String -> Int -> Maybe TL.Text
-- findNth text divider pos
--   = if length s > pos
--     then Just (s !! pos)
--     else Nothing
--   where s = splitOn divider text



fifthToLocation ve = case veSummary ve of
  Nothing -> ve
  Just x  -> do
    let e = splitOn "," $ (TL.unpack $ summaryValue x)
    if length e >= 6
      then ve {veSummary = Just $ x { summaryValue = TL.pack (e !! 0)},
               veLocation = Just $ case veLocation ve of
                 Nothing -> error "not supported"
                 Just loc -> loc { locationValue = TL.pack (e !! 5)}
              }
      else ve 
      
 
