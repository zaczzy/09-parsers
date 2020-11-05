module Xml where

import Control.Applicative (Alternative (..))
import ParserCombinators (Parser, char, doParse, filter, satisfy, string)
import System.IO
import Prelude hiding (filter)

-- Note that this line imports these functions as well as the instance for Parser
-- for the Functor, Applicative and Alternative classes.

-- | A simplified datatype for storing XML
data SimpleXML
  = PCDATA String
  | Element ElementName [SimpleXML]
  deriving (Show)

type ElementName = String

reserved :: Char -> Bool
reserved c = c `elem` ['/', '<', '>']

text :: Parser String
text = undefined

pcdata :: Parser SimpleXML
pcdata = undefined

emptyContainer :: Parser SimpleXML
emptyContainer = undefined

container :: Parser SimpleXML -> Parser SimpleXML
container p = undefined

xml :: Parser SimpleXML
xml = undefined

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a, String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  return $ doParse parser str

container2 :: Parser SimpleXML -> Parser SimpleXML
container2 p = undefined
