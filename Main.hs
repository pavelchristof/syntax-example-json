{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Category.Structures
import           Control.Lens
import           Control.Lens.SemiIso
import           Control.SIArrow
import qualified Data.Attoparsec.Text.Lazy as AP
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import qualified Data.Syntax as S
import qualified Data.Syntax.Attoparsec.Text.Lazy as S
import           Data.Syntax.Char (SyntaxText)
import qualified Data.Syntax.Char as S
import qualified Data.Syntax.Combinator as S
import           Data.Syntax.Indent (Indent)
import qualified Data.Syntax.Indent as S
import qualified Data.Syntax.Printer.Text as S
import           Data.Text (Text)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T

-- | A JSON value.
data Value = Object (Map Text Value)
           | Array [Value]
           | String Text
           | Number Scientific
           | Bool Bool
           | Null
    deriving (Show)

$(makePrisms ''Value)

-- Indent is basically a Reader transformer.
-- S.breakLine inserts a new line and correct indentation,
-- but does not require any formatting when parsing (it just
-- skips all white space).
-- S.indented increases the indentation level.

-- | Char encoded as 4 hex digits.
hexCode :: SyntaxText syn => Indent syn () Char
hexCode = from enum . bifoldl1_ (iso u f) /$/ sireplicate 4 S.digitHex
  where
    f (x, y) = 16 * x + y
    u 0 = Nothing
    u n = Just (n `quotRem` 16)

string :: SyntaxText syn => Indent syn () Text
string =  S.packed /$/ S.char '"' */ simany character /* S.char '"'
  where
    character =  S.satisfy (\x -> x /= '"' && x /= '\\')
             /+/ S.char '\\' */ escapedChar

    escapedChar =  S.char 'u' */ hexCode
               /+/ S.choice
                   [ exact v /$/ S.char e
                   | (v, e) <- [ ('"', '"'), ('\\', '\\'), ('/', '/'), ('\b', 'b')
                               , ('\f', 'f'), ('\n', 'n') , ('\r', 'r'), ('\t', 't')]
                   ]

mapFromList :: Ord i => Iso' (Map i v) [(i, v)]
mapFromList = iso Map.toList Map.fromList

object :: SyntaxText syn => Indent syn () Value
object = _Object . mapFromList 
      /$~ S.char '{'
      /*/ S.indented (
              S.breakLine /*/
              S.sepBy field (S.spaces_ /* S.char ',' /* S.breakLine)
          )
      /*/ S.breakLine /*/ S.char '}'
  where field = string /* S.spaces_ /* S.char ':' /* S.spaces /*/ value

array :: SyntaxText syn => Indent syn () Value
array = _Array 
     /$~ S.char '[' 
     /*/ S.indented (
             S.breakLine /*/
             S.sepBy value (S.spaces_ /* S.char ',' /* S.breakLine)
         )
     /*/ S.breakLine /*/ S.char ']'

value :: SyntaxText syn => Indent syn () Value
value =  _String /$/ string
     /+/ _Number /$/ S.scientific
     /+/ object
     /+/ array
     /+/ _Bool . exact True /$/ S.string "true"
     /+/ _Bool . exact False /$/ S.string "false"
     /+/ _Null /$/ S.string "null"
     /?/ "Invalid JSON."

main :: IO ()
main = do
    -- Load the standard input.
    t <- T.getContents

    -- Indent with 2 spaces.
    let tab :: SyntaxText syn => syn () ()
        tab = sireplicate_ 2 (S.char ' ')
        parser = S.getParser_ $ S.runIndent value tab
        printer = S.runPrinter_ $ S.runIndent value tab

    -- Try to parse it.
    case AP.parse (AP.skipSpace *> parser <* AP.skipSpace <* AP.endOfInput) t of
      AP.Fail _ _ err  -> putStrLn err
      AP.Done _ val -> do
        -- Try to pretty print it.
        -- (Printing cannot really fail in this example)
        case printer val of
          Left err  -> putStrLn err
          Right bld -> T.putStrLn (T.toLazyText bld)

    return ()
